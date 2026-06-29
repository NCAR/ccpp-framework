#!/usr/bin/env python3

"""Generate the static API module ``<host>_ccpp_cap.F90``.

The static API module is generated once per build (not per suite) and
provides the canonical public entry points that a host model calls:

  - ``ccpp_register``
  - ``ccpp_init``
  - ``ccpp_physics_init``, ``ccpp_physics_timestep_init``,
    ``ccpp_physics_run``, ``ccpp_physics_timestep_final``,
    ``ccpp_physics_final``
  - ``ccpp_final``

Each entry point dispatches by ``suite_name`` (and ``group_name`` for the
physics-phase calls) to the corresponding suite cap subroutine.

The module also exposes five suite-introspection routines so a host
can query at runtime what is compiled into the API:

  - ``ccpp_physics_suite_list(suites)`` — names of all compiled-in suites.
  - ``ccpp_physics_suite_part_list(suite_name, part_list, errmsg, errflg)``
    — group ("part") names belonging to *suite_name*.
  - ``ccpp_physics_suite_schemes(suite_name, scheme_list, errmsg, errflg)``
    — scheme module names that compose *suite_name* (deduped across phases).
  - ``ccpp_physics_suite_variables(suite_name, variable_list, errmsg,
    errflg, [input_vars], [output_vars])`` — flat leaf standard names the
    host exchanges with the suite.  ``input_vars`` selects intent in/inout
    (skipping ``protected`` host vars); ``output_vars`` selects intent
    out/inout.  Both default ``.true.`` (the union, deduplicated).
  - ``ccpp_physics_suite_host_data(suite_name, variable_list, errmsg,
    errflg, [input_vars], [output_vars])`` — same intent-filter semantics,
    but DDT-leaf standard names are collapsed to the standard name of the
    top-level DDT instance the host owns.  Plain (non-DDT) leaves appear
    individually, just like in ``..._suite_variables``.

The suite-variables / suite-host-data routines exclude control variables
(those passed via the framework signature) — hosts already know their own
control table, and excluding them keeps the lists focused on the data the
host has to read/write to interface with the suite.
"""

import logging
import os
from typing import Dict, List, Optional, Set, Tuple

from metadata.parse_tools import CCPPError, open_if_changed
from metadata.variable_resolver import SchemeStore
from generator.suite_resolver import (
    ResolvedArg,
    SuiteResolution,
    iter_phase_calls,
    iter_phase_subcycles,
)
from metadata.variable_resolver import HostVarEntry
from generator.trace import (
    emit_module_gate,
    emit_trace_block,
    ensure_error_unit_use,
)
from generator.suite_cap import (
    _all_suite_scheme_names,
    _schemes_with_register,
    _suite_ctrl_args_for_phase,
    _suite_extra_ctrl_entries_for_phase,
    _PHYSICS_PHASES,
    _CONST_MOD,
    _CONST_DDT,
    _CONST_PROP_TYPE,
)
from generator.group_cap import (
    _active_std_names,
    _ctrl_entries_for_signature,
    _ctrl_intent_for,
    _ctrl_local,
    _fortran_type_str,
    _dim_decl,
    _instance_local,
    _intent_clause,
)

_INDENT = '  '

# Message bodied into stubbed suite-introspection routines when the
# generator was invoked with ``--no-host-introspection``.  Kept here so
# every introspection routine produces the same wording and tests can
# assert against a single constant.
_INTROSPECTION_DISABLED_MSG = (
    'suite introspection disabled at code-generation time; '
    'regenerate caps without --no-host-introspection'
)


def _emit_introspection_stub_body(
    routine_name: str,
    list_arg_name: str,
    indent: str,
) -> List[str]:
    """Emit the stub body shared by the four errflg-bearing introspection
    routines (``suite_part_list``, ``suite_schemes``, ``suite_variables``,
    ``suite_host_data``).

    Sets ``errflg = 1`` and ``errmsg`` to the canonical disabled-message
    prefixed with *routine_name*, then allocates *list_arg_name* to a
    zero-length array so callers can safely ``size()`` / iterate without
    a NULL-deref crash.  Indentation level *indent* matches the body
    block of the calling routine.
    """
    lines: List[str] = []
    lines.append("{}errmsg = '{}: {}'".format(
        indent, routine_name, _INTROSPECTION_DISABLED_MSG,
    ))
    lines.append('{}errflg = 1'.format(indent))
    lines.append('{}allocate({}(0))'.format(indent, list_arg_name))
    return lines


########################################################################
# Helpers
########################################################################

def _all_ctrl_args_for_phase(
    suite_resolutions: List[SuiteResolution],
    phase: str,
) -> List[ResolvedArg]:
    """Return the union of direct scheme control args across all suites for *phase*.

    Deduplicated by standard_name, first-seen order.
    """
    seen: Dict[str, ResolvedArg] = {}
    for suite_resolution in suite_resolutions:
        for arg in _suite_ctrl_args_for_phase(suite_resolution, phase):
            if arg.standard_name not in seen:
                seen[arg.standard_name] = arg
    return list(seen.values())


def _all_extra_ctrl_entries_for_phase(
    suite_resolutions: List[SuiteResolution],
    phase: str,
    ctrl_std_names: Set[str],
    host_dict,
) -> List[HostVarEntry]:
    """Return extra HostVarEntry objects (state indexing / dim subscripts) needed
    by any suite-group for *phase* but not already in *ctrl_std_names*.
    """
    if host_dict is None:
        return []
    seen = set(ctrl_std_names)
    result: Dict[str, HostVarEntry] = {}
    for suite_resolution in suite_resolutions:
        for entry in _suite_extra_ctrl_entries_for_phase(suite_resolution, phase, seen, host_dict):
            if entry.standard_name not in seen and entry.standard_name not in result:
                result[entry.standard_name] = entry
    return list(result.values())


########################################################################
# Helpers — suite-introspection
########################################################################

def _emit_var_set_loop(
    list_name: str,
    items: List[str],
    indent: str,
    allocate: bool = True,
) -> List[str]:
    """Emit ``allocate(<list_name>(N))`` followed by per-element assignments.

    Items are emitted as 1-based Fortran assignments: ``<list_name>(i) = '<item>'``.
    Used by every introspection routine that returns a string list.

    >>> _emit_var_set_loop('x', ['a', 'b'], '  ')
    ['  allocate(x(2))', "  x(1) = 'a'", "  x(2) = 'b'"]
    >>> _emit_var_set_loop('x', [], '  ')
    ['  allocate(x(0))']
    """
    lines = []
    if allocate:
        lines.append('{}allocate({}({}))'.format(indent, list_name, len(items)))
    for i, item in enumerate(items):
        lines.append("{}{}({}) = '{}'".format(indent, list_name, i + 1, item))
    return lines


def _build_local_to_std_top_level_map(host_dict) -> Dict[str, str]:
    """Build local_name → standard_name map for top-level host_dict entries.

    Only entries whose ``access_path`` does not contain ``'%'`` are
    included — those are top-level DDT instances or plain (non-DDT)
    leaves.  Used to collapse a flattened DDT-leaf back to the standard
    name of the DDT instance the host owns.
    """
    if not host_dict:
        return {}
    return {
        entry.local_name: entry.standard_name
        for entry in host_dict.values()
        if '%' not in entry.access_path
    }


def _arg_top_level_name(
    arg: ResolvedArg,
    local_to_std: Dict[str, str],
) -> str:
    """Return the top-level standard name for *arg* (DDT-collapsed view).

    For a plain leaf or a directly-referenced DDT instance, returns
    ``arg.standard_name`` unchanged.  For a DDT-leaf access path like
    ``phys_state(instance_number)%t``, parses the root local name
    (``phys_state``), strips any subscript, and looks up the standard
    name of the host_dict entry whose ``local_name`` matches.

    Falls back to ``arg.standard_name`` if the lookup fails (which would
    indicate inconsistent metadata, but produces a well-defined output).
    """
    if arg.host_entry is None:
        return arg.standard_name
    ap = arg.host_entry.access_path
    if '%' not in ap:
        return arg.standard_name
    root = ap.split('%', 1)[0]
    paren = root.find('(')
    if paren >= 0:
        root = root[:paren]
    return local_to_std.get(root, arg.standard_name)


def _collect_host_io(
    suite_resolution: SuiteResolution,
    host_dict=None,
    collapse_ddts: bool = False,
) -> Tuple[List[str], List[str]]:
    """Collect (inputs, outputs) standard names for the introspection routines.

    Walks every phase of every group of *suite_resolution*.  Includes scheme args from
    every ``source`` category EXCEPT ``'suite'`` — suite-owned vars are
    internal data flow between schemes (one scheme writes them, another
    reads them) and are not part of the host-facing variable list.

    Concretely, the returned lists include:

    * ``source='host'`` — host metadata vars.
    * ``source='control'`` — control vars (errmsg, errflg, …) when they
      appear as scheme args (not when they're framework-injected dummies).
    * ``source='constituent'`` — both auto-resolved base/tendency
      constituents and direct framework-array references
      (``ccpp_constituents`` / ``ccpp_constituent_tendencies`` / etc.) and
      register-phase ``ccpp_constituent_properties_t`` args.

    *inputs*  : intent in ``('in', 'inout')`` and not protected (the
                protected check is host-only; constituent / control args
                have no protected attribute).
    *outputs* : intent in ``('out', 'inout')``.

    When *collapse_ddts* is true, DDT-leaf standard names are mapped to
    the standard name of the top-level DDT instance via *host_dict*.
    If *host_dict* is ``None`` while *collapse_ddts* is true, the lookup
    map is empty and every leaf falls back to its own standard name —
    correct in scenarios with no DDT instances (e.g. minimal unit tests).
    Both lists are returned sorted alphabetically for deterministic output.

    This matches the behavior of the original capgen's
    ``ccpp_physics_suite_variables`` so host comparison tests round-trip
    cleanly.
    """
    local_to_std = _build_local_to_std_top_level_map(host_dict) if collapse_ddts else {}

    def _collapse_std(std_name: str) -> str:
        """Map a CCPP standard name to its DDT-collapsed counterpart.

        For free host variables the standard name maps to itself; for a
        DDT-component entry the result is the standard name of the
        top-level instance.  When ``collapse_ddts`` is False or the
        std_name isn't in ``host_dict``, returns the input unchanged.
        """
        if not collapse_ddts or host_dict is None:
            return std_name
        entry = host_dict.get(std_name)
        if entry is None or '%' not in entry.access_path:
            return std_name
        root = entry.access_path.split('%', 1)[0]
        paren = root.find('(')
        if paren >= 0:
            root = root[:paren]
        return local_to_std.get(root, std_name)

    inputs: Set[str] = set()
    outputs: Set[str] = set()
    for group in suite_resolution.groups:
        for items in group.phase_calls.values():
            # Subcycle loop bounds named by a CCPP standard name (e.g.
            # ``<subcycle loop="num_subcycles_for_effr">``) are pure
            # inputs from the host — the host supplies the value and the
            # generated cap reads it as a do-loop bound.  Every nesting
            # level contributes a (potentially different) bound, so walk
            # the full subcycle tree.  Without this, the host's
            # compile-time bookkeeping (ccpp_physics_suite_variables /
            # _suite_host_data) would silently omit required inputs.
            for subcycle in iter_phase_subcycles(items):
                if subcycle.loop_std_name:
                    inputs.add(_collapse_std(subcycle.loop_std_name))
            for call in iter_phase_calls(items):
                for arg in call.args:
                    # Suite-owned vars are internal scheme-to-scheme
                    # plumbing and not part of the host-facing surface.
                    if arg.source == 'suite':
                        continue
                    name = (
                        _arg_top_level_name(arg, local_to_std)
                        if collapse_ddts
                        else arg.standard_name
                    )
                    if arg.intent in ('in', 'inout'):
                        # Only host args have a meaningful protected flag.
                        if not (arg.host_entry and arg.host_entry.protected):
                            inputs.add(name)
                    if arg.intent in ('out', 'inout'):
                        outputs.add(name)
                    # Framework-constituent dim references (e.g.
                    # number_of_ccpp_constituents as the trailing dim of
                    # ccpp_constituents) appear in the inputs list even
                    # though they don't have a dedicated scheme arg.
                    # Tracked on a dedicated field — these names are
                    # NOT in host_dict and must not be USE'd, so they
                    # don't live on used_dim_std_names.
                    for dim_std in arg.used_const_dim_std_names:
                        inputs.add(dim_std)
                    # Active-expression references (e.g.
                    # ``active = (flag_indicating_...)`` on a host var)
                    # are pure inputs: the host provides the flag so the
                    # suite knows whether the active arg is present.
                    # Without this, flags that *aren't* used as a direct
                    # scheme arg silently fall out of the introspection
                    # input list.
                    for active_std in _active_std_names(arg.active):
                        # Skip Fortran literals captured by the
                        # active-name tokenizer (e.g. ``.true.``).
                        if host_dict is not None and active_std in host_dict:
                            inputs.add(_collapse_std(active_std))
    return sorted(inputs), sorted(outputs)


########################################################################
# Entry point generators
########################################################################

def _register_subroutine(suite_names: List[str], host_dict=None) -> List[str]:
    """Generate ``ccpp_register`` (mandatory entry point).

    Always emitted with the minimal lifecycle signature
    ``(suite_name, ccpp_error_code, ccpp_error_message,
    [instance_number, number_of_instances])``.  The instance pair is
    forwarded to ``<suite>_register`` only when the host declares it.
    The body dispatches to ``<suite>_register`` for every known suite.  Each
    suite's register routine is responsible for allocating its state array
    and DDT instance array, calling its register-phase scheme entrypoints,
    and transitioning suite state to ``REGISTERED`` — even if the suite has
    no register-providing schemes (the state transition still fires).
    """
    i1 = _INDENT
    i2 = _INDENT * 2
    i3 = _INDENT * 3

    inst_local = _instance_local(host_dict)
    ninst_entry = host_dict.get('number_of_instances') if host_dict else None
    ninst_local = ninst_entry.local_name if ninst_entry else None

    errflg_local = _ctrl_local(host_dict, 'ccpp_error_code') or 'errflg'
    errmsg_local = _ctrl_local(host_dict, 'ccpp_error_message') or 'errmsg'
    suite_name_entry = host_dict.get('suite_name') if host_dict else None
    suite_name_local = (
        suite_name_entry.local_name if suite_name_entry else 'suite_name'
    )

    sig_args = [suite_name_local, errflg_local, errmsg_local]
    suite_call_args = []
    if inst_local:
        sig_args.append(inst_local)
        suite_call_args.append(inst_local)
    if ninst_local:
        sig_args.append(ninst_local)
        suite_call_args.append(ninst_local)
    suite_call_args += [errmsg_local, errflg_local]

    lines: List[str] = ['']
    lines.append('{}subroutine ccpp_register({})'.format(i1, ', '.join(sig_args)))
    lines.append('')
    lines.append('{}character(len=*), intent(in) :: {}'.format(i2, suite_name_local))
    lines.append('{}integer, intent(out) :: {}'.format(i2, errflg_local))
    lines.append('{}character(len=*), intent(out) :: {}'.format(i2, errmsg_local))
    if inst_local:
        lines.append('{}integer, intent(in) :: {}'.format(i2, inst_local))
    if ninst_local:
        lines.append('{}integer, intent(in) :: {}'.format(i2, ninst_local))
    trace_entries = [suite_name_entry] if suite_name_entry else []
    extra_in = [ninst_local] if ninst_local else None
    trace_lines = emit_trace_block(
        'ccpp_register', trace_entries, i2,
        instance_local=inst_local, extra_in_names=extra_in,
    )
    if trace_lines:
        lines.append('')
        lines.extend(trace_lines)
    lines += [
        '',
        "{}{} = ''".format(i2, errmsg_local),
        '{}{} = 0'.format(i2, errflg_local),
        '',
        '{}select case(trim({}))'.format(i2, suite_name_local),
    ]
    for sname in suite_names:
        lines.append("{}case('{}')".format(i2, sname))
        lines.append('{}call {}_register({})'.format(
            i3, sname, ', '.join(suite_call_args)
        ))
    lines += [
        '{}case default'.format(i2),
        '{}{} = 1'.format(i3, errflg_local),
        "{}{} = 'ccpp_register: unknown suite: ' // trim({})".format(
            i3, errmsg_local, suite_name_local
        ),
        '{}end select'.format(i2),
        '',
        '{}end subroutine ccpp_register'.format(i1),
    ]
    return lines


def _init_subroutine(suite_names: List[str], host_dict=None) -> List[str]:
    """Generate ``ccpp_init`` (minimal lifecycle signature).

    Signature: ``(suite_name, ccpp_error_code, ccpp_error_message,
    [instance_number, number_of_instances])``.  The instance pair is
    forwarded to ``<suite>_init`` only when the host declares it.
    """
    i1 = _INDENT
    i2 = _INDENT * 2
    i3 = _INDENT * 3

    inst_local = _instance_local(host_dict)
    ninst_entry = host_dict.get('number_of_instances') if host_dict else None
    ninst_local = ninst_entry.local_name if ninst_entry else None

    errflg_local = _ctrl_local(host_dict, 'ccpp_error_code') or 'errflg'
    errmsg_local = _ctrl_local(host_dict, 'ccpp_error_message') or 'errmsg'
    suite_name_entry = host_dict.get('suite_name') if host_dict else None
    suite_name_local = (
        suite_name_entry.local_name if suite_name_entry else 'suite_name'
    )

    sig_args = [suite_name_local, errflg_local, errmsg_local]
    suite_call_args: List[str] = []
    if inst_local:
        sig_args.append(inst_local)
        suite_call_args.append(inst_local)
    if ninst_local:
        sig_args.append(ninst_local)
        suite_call_args.append(ninst_local)
    suite_call_args += [errmsg_local, errflg_local]

    lines: List[str] = ['']
    lines.append('{}subroutine ccpp_init({})'.format(i1, ', '.join(sig_args)))
    lines.append('')
    lines.append('{}character(len=*), intent(in) :: {}'.format(i2, suite_name_local))
    lines.append('{}integer, intent(out) :: {}'.format(i2, errflg_local))
    lines.append('{}character(len=*), intent(out) :: {}'.format(i2, errmsg_local))
    if inst_local:
        lines.append('{}integer, intent(in) :: {}'.format(i2, inst_local))
    if ninst_local:
        lines.append('{}integer, intent(in) :: {}'.format(i2, ninst_local))
    trace_entries = [suite_name_entry] if suite_name_entry else []
    extra_in = [ninst_local] if ninst_local else None
    trace_lines = emit_trace_block(
        'ccpp_init', trace_entries, i2,
        instance_local=inst_local, extra_in_names=extra_in,
    )
    if trace_lines:
        lines.append('')
        lines.extend(trace_lines)
    lines += [
        '',
        "{}{} = ''".format(i2, errmsg_local),
        '{}{} = 0'.format(i2, errflg_local),
        '',
        '{}select case(trim({}))'.format(i2, suite_name_local),
    ]
    for sname in suite_names:
        lines.append("{}case('{}')".format(i2, sname))
        lines.append('{}call {}_init({})'.format(
            i3, sname, ', '.join(suite_call_args)
        ))
    lines += [
        '{}case default'.format(i2),
        '{}{} = 1'.format(i3, errflg_local),
        "{}{} = 'ccpp_init: unknown suite: ' // trim({})".format(
            i3, errmsg_local, suite_name_local
        ),
        '{}end select'.format(i2),
        '',
        '{}end subroutine ccpp_init'.format(i1),
    ]
    return lines


def _final_subroutine(suite_names: List[str], host_dict=None) -> List[str]:
    """Generate ``ccpp_final`` (lifecycle signature).

    Signature: ``(suite_name, ccpp_error_code, ccpp_error_message,
    [instance_number, number_of_instances])``.  ``number_of_instances``
    is carried for API symmetry with ``ccpp_register`` / ``ccpp_init``
    even though the framework does not need it at final time.
    """
    i1 = _INDENT
    i2 = _INDENT * 2
    i3 = _INDENT * 3

    inst_local = _instance_local(host_dict)
    ninst_entry = host_dict.get('number_of_instances') if host_dict else None
    ninst_local = ninst_entry.local_name if ninst_entry else None

    errflg_local = _ctrl_local(host_dict, 'ccpp_error_code') or 'errflg'
    errmsg_local = _ctrl_local(host_dict, 'ccpp_error_message') or 'errmsg'
    suite_name_entry = host_dict.get('suite_name') if host_dict else None
    suite_name_local = (
        suite_name_entry.local_name if suite_name_entry else 'suite_name'
    )

    sig_args = [suite_name_local, errflg_local, errmsg_local]
    suite_call_args: List[str] = []
    if inst_local:
        sig_args.append(inst_local)
        suite_call_args.append(inst_local)
    if ninst_local:
        sig_args.append(ninst_local)
        suite_call_args.append(ninst_local)
    suite_call_args += [errmsg_local, errflg_local]

    lines: List[str] = ['']
    lines.append('{}subroutine ccpp_final({})'.format(i1, ', '.join(sig_args)))
    lines.append('')
    lines.append('{}character(len=*), intent(in) :: {}'.format(i2, suite_name_local))
    lines.append('{}integer, intent(out) :: {}'.format(i2, errflg_local))
    lines.append('{}character(len=*), intent(out) :: {}'.format(i2, errmsg_local))
    if inst_local:
        lines.append('{}integer, intent(in) :: {}'.format(i2, inst_local))
    if ninst_local:
        lines.append('{}integer, intent(in) :: {}'.format(i2, ninst_local))
    trace_entries = [suite_name_entry] if suite_name_entry else []
    extra_in = [ninst_local] if ninst_local else None
    trace_lines = emit_trace_block(
        'ccpp_final', trace_entries, i2,
        instance_local=inst_local, extra_in_names=extra_in,
    )
    if trace_lines:
        lines.append('')
        lines.extend(trace_lines)
    lines += [
        '',
        "{}{} = ''".format(i2, errmsg_local),
        '{}{} = 0'.format(i2, errflg_local),
        '',
        '{}select case(trim({}))'.format(i2, suite_name_local),
    ]
    for sname in suite_names:
        lines.append("{}case('{}')".format(i2, sname))
        lines.append('{}call {}_final({})'.format(
            i3, sname, ', '.join(suite_call_args)
        ))
    lines += [
        '{}case default'.format(i2),
        '{}{} = 1'.format(i3, errflg_local),
        "{}{} = 'ccpp_final: unknown suite: ' // trim({})".format(
            i3, errmsg_local, suite_name_local
        ),
        '{}end select'.format(i2),
        '',
        '{}end subroutine ccpp_final'.format(i1),
    ]
    return lines


def _physics_subroutine(
    phase: str,
    suite_names: List[str],
    suite_resolutions: List[SuiteResolution],
    host_dict=None,
) -> List[str]:
    """Generate one ``ccpp_physics_<phase>`` dispatch subroutine.

    The formal argument list is derived entirely from the host's ``type=control``
    metadata table.  ``suite_name`` drives the top-level dispatch; ``group_name``
    (if present in the control table) is forwarded to the suite cap dispatch.
    """
    sub_name = 'ccpp_physics_{}'.format(phase)
    i1 = _INDENT
    i2 = _INDENT * 2
    i3 = _INDENT * 3

    # Full control arg list (all control vars from host_dict).
    ctrl_entries = _ctrl_entries_for_signature(host_dict)
    ctrl_local_names = [e.local_name for e in ctrl_entries]

    # Args forwarded to suite cap (all ctrl vars except suite_name).
    suite_ctrl_entries = _ctrl_entries_for_signature(host_dict, exclude={'suite_name'})
    suite_ctrl_local = [e.local_name for e in suite_ctrl_entries]

    # Local name for suite_name (used in select case dispatch).
    suite_name_entry = next(
        (e for e in ctrl_entries if e.standard_name == 'suite_name'), None
    )
    suite_name_local = suite_name_entry.local_name if suite_name_entry else 'suite_name'

    lines = ['']

    # Subroutine signature.
    if ctrl_local_names:
        lines.append('{}subroutine {}( &'.format(i1, sub_name))
        for i, lname in enumerate(ctrl_local_names):
            sep = ', &' if i < len(ctrl_local_names) - 1 else ')'
            lines.append('{}    {}{}'.format(i1, lname, sep))
    else:
        lines.append('{}subroutine {}()'.format(i1, sub_name))

    # Dummy declarations.
    lines.append('')
    for entry in ctrl_entries:
        # Character control dummies always use len=* so the host's specific
        # length doesn't propagate into the generated signature.
        kind = 'len=*' if entry.type.strip().lower() == 'character' else entry.kind
        t   = _fortran_type_str(entry.type, kind)
        dim = _dim_decl(entry.dimensions)
        intent = _intent_clause(_ctrl_intent_for(entry.standard_name))
        lines.append(
            '{}{}{}{}  :: {}'.format(i2, t, intent, dim, entry.local_name)
        )

    # Trace block: every intent(in)/inout control dummy is referenced so
    # strict compilers don't flag any of them as unused.
    trace_lines = emit_trace_block(sub_name, ctrl_entries, i2)
    if trace_lines:
        lines.append('')
        lines.extend(trace_lines)

    lines.append('')

    # Initialize error reporting vars before any work.
    errflg_local = _ctrl_local(host_dict, 'ccpp_error_code')
    errmsg_local = _ctrl_local(host_dict, 'ccpp_error_message')
    if errflg_local and errmsg_local:
        lines.append("{}{} = ''".format(i2, errmsg_local))
        lines.append('{}{} = 0'.format(i2, errflg_local))
        lines.append('')

    # select case(suite_name) dispatch.
    lines.append('{}select case(trim({}))'.format(i2, suite_name_local))
    for sname in suite_names:
        lines.append("{}case('{}')".format(i2, sname))
        cap_sub = '{}_physics_{}'.format(sname, phase)
        if suite_ctrl_local:
            lines.append('{}call {}( &'.format(i3, cap_sub))
            for i, carg in enumerate(suite_ctrl_local):
                sep = ', &' if i < len(suite_ctrl_local) - 1 else ')'
                lines.append('{}    {}{}'.format(i3, carg, sep))
        else:
            lines.append('{}call {}()'.format(i3, cap_sub))
    # case default: unknown suite name is a runtime error (not silent
    # fall-through).  Skip emission only when the host doesn't carry the
    # standard error-reporting control vars — without somewhere to write
    # the message, there is nothing meaningful to do here.
    if errflg_local and errmsg_local:
        lines.append('{}case default'.format(i2))
        lines.append('{}{} = 1'.format(i3, errflg_local))
        lines.append(
            "{}{} = '{}: unknown suite: ' // trim({})".format(
                i3, errmsg_local, sub_name, suite_name_local,
            )
        )
    lines.append('{}end select'.format(i2))

    lines.append('')
    lines.append('{}end subroutine {}'.format(i1, sub_name))
    return lines


########################################################################
# Suite-introspection subroutines
########################################################################

def _suite_list_subroutine(
    suite_names: List[str],
    stub_body: bool = False,
) -> List[str]:
    """Generate ``ccpp_physics_suite_list(suites)``.

    Allocates ``suites`` to the number of compiled-in suites and assigns
    each entry to the suite name as a literal string.

    When *stub_body* is true, emit a stub: write a clear message to
    ``error_unit`` and return an empty list.  ``ccpp_physics_suite_list``
    has no errflg/errmsg arguments, so ``error_unit`` is the only
    available error channel.  The module-level ``use iso_fortran_env``
    that this references is added by ``_generate_host_cap`` when
    ``no_host_introspection`` is on.
    """
    i1 = _INDENT
    i2 = _INDENT * 2

    lines: List[str] = ['']
    lines.append('{}subroutine ccpp_physics_suite_list(suites)'.format(i1))
    lines.append('')
    lines.append(
        '{}character(len=*), allocatable, intent(out) :: suites(:)'.format(i2)
    )
    lines.append('')
    if stub_body:
        lines.append(
            "{}write(error_unit, '(a)') 'ccpp_physics_suite_list: ' &"
            .format(i2)
        )
        lines.append(
            "{}    // '{}'".format(i2, _INTROSPECTION_DISABLED_MSG)
        )
        lines.append('{}allocate(suites(0))'.format(i2))
    else:
        lines.extend(_emit_var_set_loop('suites', suite_names, i2))
    lines.append('')
    lines.append('{}end subroutine ccpp_physics_suite_list'.format(i1))
    return lines


def _suite_part_list_subroutine(
    suite_names: List[str],
    suite_resolutions: List[SuiteResolution],
    stub_body: bool = False,
) -> List[str]:
    """Generate ``ccpp_physics_suite_part_list(suite_name, part_list, errmsg, errflg)``.

    Dispatches by ``suite_name`` and returns the group ("part") names of
    that suite in declaration order.  ``case default`` sets ``errflg=1``
    and writes a message naming the unknown suite.
    """
    i1 = _INDENT
    i2 = _INDENT * 2
    i3 = _INDENT * 3

    lines: List[str] = ['']
    lines.append(
        '{}subroutine ccpp_physics_suite_part_list( &'.format(i1)
    )
    lines.append('{}    suite_name, part_list, errmsg, errflg)'.format(i1))
    lines.append('')
    lines.append(
        '{}character(len=*),              intent(in)    :: suite_name'.format(i2)
    )
    lines.append(
        '{}character(len=*), allocatable, intent(out)   :: part_list(:)'.format(i2)
    )
    lines.append(
        '{}character(len=*),              intent(out)   :: errmsg'.format(i2)
    )
    lines.append(
        '{}integer,                       intent(out)   :: errflg'.format(i2)
    )
    lines.append('')
    if stub_body:
        lines.extend(
            _emit_introspection_stub_body(
                'ccpp_physics_suite_part_list', 'part_list', i2,
            )
        )
    else:
        lines.append("{}errmsg = ''".format(i2))
        lines.append('{}errflg = 0'.format(i2))
        lines.append('')
        lines.append('{}select case (trim(suite_name))'.format(i2))
        for sname, suite_resolution in zip(suite_names, suite_resolutions):
            groups = [g.group_name for g in suite_resolution.groups]
            lines.append("{}case ('{}')".format(i2, sname))
            lines.extend(_emit_var_set_loop('part_list', groups, i3))
        lines.append('{}case default'.format(i2))
        lines.append('{}errflg = 1'.format(i3))
        lines.append(
            "{}errmsg = 'ccpp_physics_suite_part_list: unknown suite: ' "
            "// trim(suite_name)".format(i3)
        )
        lines.append('{}end select'.format(i2))
    lines.append('')
    lines.append('{}end subroutine ccpp_physics_suite_part_list'.format(i1))
    return lines


def _suite_schemes_subroutine(
    suite_names: List[str],
    suite_resolutions: List[SuiteResolution],
    stub_body: bool = False,
) -> List[str]:
    """Generate ``ccpp_physics_suite_schemes(suite_name, scheme_list, errmsg, errflg)``.

    Returns the unique scheme names that compose *suite_name*, deduped
    across all phases and groups, sorted alphabetically.
    """
    i1 = _INDENT
    i2 = _INDENT * 2
    i3 = _INDENT * 3

    lines: List[str] = ['']
    lines.append(
        '{}subroutine ccpp_physics_suite_schemes( &'.format(i1)
    )
    lines.append('{}    suite_name, scheme_list, errmsg, errflg)'.format(i1))
    lines.append('')
    lines.append(
        '{}character(len=*),              intent(in)    :: suite_name'.format(i2)
    )
    lines.append(
        '{}character(len=*), allocatable, intent(out)   :: scheme_list(:)'.format(i2)
    )
    lines.append(
        '{}character(len=*),              intent(out)   :: errmsg'.format(i2)
    )
    lines.append(
        '{}integer,                       intent(out)   :: errflg'.format(i2)
    )
    lines.append('')
    if stub_body:
        lines.extend(
            _emit_introspection_stub_body(
                'ccpp_physics_suite_schemes', 'scheme_list', i2,
            )
        )
    else:
        lines.append("{}errmsg = ''".format(i2))
        lines.append('{}errflg = 0'.format(i2))
        lines.append('')
        lines.append('{}select case (trim(suite_name))'.format(i2))
        for sname, suite_resolution in zip(suite_names, suite_resolutions):
            schemes = sorted({
                call.scheme_name
                for group in suite_resolution.groups
                for items in group.phase_calls.values()
                for call in iter_phase_calls(items)
            })
            lines.append("{}case ('{}')".format(i2, sname))
            lines.extend(_emit_var_set_loop('scheme_list', schemes, i3))
        lines.append('{}case default'.format(i2))
        lines.append('{}errflg = 1'.format(i3))
        lines.append(
            "{}errmsg = 'ccpp_physics_suite_schemes: unknown suite: ' "
            "// trim(suite_name)".format(i3)
        )
        lines.append('{}end select'.format(i2))
    lines.append('')
    lines.append('{}end subroutine ccpp_physics_suite_schemes'.format(i1))
    return lines


def _suite_io_subroutine(
    suite_names: List[str],
    suite_resolutions: List[SuiteResolution],
    host_dict=None,
    collapse_ddts: bool = False,
    stub_body: bool = False,
) -> List[str]:
    """Generate ``ccpp_physics_suite_variables`` or ``ccpp_physics_suite_host_data``.

    The two routines share the same Fortran shape — ``select case`` on
    ``suite_name`` with one branch per suite and three nested branches
    keyed on ``input_vars`` / ``output_vars`` — so they are emitted by
    one helper.  *collapse_ddts* controls both the routine name and
    whether DDT-leaves get mapped to their top-level DDT instance.

    Optional ``input_vars``/``output_vars`` default to ``.true.``;
    when both are ``.true.`` the union of inputs and outputs is returned
    (deduplicated).  When both are ``.false.`` an empty list is returned.
    """
    sub_name = (
        'ccpp_physics_suite_host_data'
        if collapse_ddts
        else 'ccpp_physics_suite_variables'
    )
    i1 = _INDENT
    i2 = _INDENT * 2
    i3 = _INDENT * 3
    i4 = _INDENT * 4

    lines: List[str] = ['']
    lines.append('{}subroutine {}( &'.format(i1, sub_name))
    lines.append(
        '{}    suite_name, variable_list, errmsg, errflg, &'.format(i1)
    )
    lines.append('{}    input_vars, output_vars)'.format(i1))
    lines.append('')
    lines.append(
        '{}character(len=*),              intent(in)    :: suite_name'.format(i2)
    )
    lines.append(
        '{}character(len=*), allocatable, intent(out)   :: variable_list(:)'.format(i2)
    )
    lines.append(
        '{}character(len=*),              intent(out)   :: errmsg'.format(i2)
    )
    lines.append(
        '{}integer,                       intent(out)   :: errflg'.format(i2)
    )
    lines.append(
        '{}logical, optional,             intent(in)    :: input_vars'.format(i2)
    )
    lines.append(
        '{}logical, optional,             intent(in)    :: output_vars'.format(i2)
    )
    lines.append('')
    if stub_body:
        # Stubbed body: set errflg + clear errmsg and allocate an empty
        # list.  ``input_vars`` / ``output_vars`` are intentionally left
        # unreferenced — they're declared ``intent(in), optional`` so
        # compilers may warn about the unused dummy arg, but warning is
        # the right outcome: the host built against introspection and
        # is calling with filter flags that no longer matter.
        lines.append('')
        lines.extend(
            _emit_introspection_stub_body(sub_name, 'variable_list', i2)
        )
    else:
        lines.append('{}logical :: input_vars_use'.format(i2))
        lines.append('{}logical :: output_vars_use'.format(i2))
        lines.append('')
        lines.append("{}errmsg = ''".format(i2))
        lines.append('{}errflg = 0'.format(i2))
        lines.append('')
        lines.append('{}if (present(input_vars)) then'.format(i2))
        lines.append('{}input_vars_use = input_vars'.format(i3))
        lines.append('{}else'.format(i2))
        lines.append('{}input_vars_use = .true.'.format(i3))
        lines.append('{}end if'.format(i2))
        lines.append('{}if (present(output_vars)) then'.format(i2))
        lines.append('{}output_vars_use = output_vars'.format(i3))
        lines.append('{}else'.format(i2))
        lines.append('{}output_vars_use = .true.'.format(i3))
        lines.append('{}end if'.format(i2))
        lines.append('')
        lines.append('{}select case (trim(suite_name))'.format(i2))
        for sname, suite_resolution in zip(suite_names, suite_resolutions):
            inputs, outputs = _collect_host_io(suite_resolution, host_dict, collapse_ddts)
            union = sorted(set(inputs) | set(outputs))
            lines.append("{}case ('{}')".format(i2, sname))
            lines.append('{}if (input_vars_use .and. output_vars_use) then'.format(i3))
            lines.extend(_emit_var_set_loop('variable_list', union, i4))
            lines.append('{}else if (input_vars_use) then'.format(i3))
            lines.extend(_emit_var_set_loop('variable_list', inputs, i4))
            lines.append('{}else if (output_vars_use) then'.format(i3))
            lines.extend(_emit_var_set_loop('variable_list', outputs, i4))
            lines.append('{}else'.format(i3))
            lines.append('{}allocate(variable_list(0))'.format(i4))
            lines.append('{}end if'.format(i3))
        lines.append('{}case default'.format(i2))
        lines.append('{}errflg = 1'.format(i3))
        lines.append(
            "{}errmsg = '{}: unknown suite: ' "
            "// trim(suite_name)".format(i3, sub_name)
        )
        lines.append('{}end select'.format(i2))
    lines.append('')
    lines.append('{}end subroutine {}'.format(i1, sub_name))
    return lines


########################################################################
# Module generator
########################################################################

def _generate_host_cap(
    host_name: str,
    suite_names: List[str],
    suite_resolutions: List[SuiteResolution],
    host_dict=None,
    scheme_store: Optional[SchemeStore] = None,
    no_host_introspection: bool = False,
    trace: bool = False,
) -> List[str]:
    """Generate the full ``<host>_ccpp_cap.F90`` module source lines.

    Parameters
    ----------
    host_name : str
        Host identifier; drives the emitted module name
        (``module <host>_ccpp_cap``) and the comment header.
    suite_names : list of str
        Suite names in order.
    suite_resolutions : list of SuiteResolution
        Parallel to suite_names.
    host_dict : dict, optional
        Flat host+control dictionary.  When provided, ``number_of_instances``
        is threaded through ``ccpp_init`` for multi-instance support.
    scheme_store : SchemeStore, optional
        When provided, used to determine which suites have at least one
        scheme with a ``register`` phase.  Only those suites contribute a
        ``case`` arm to ``ccpp_register``, and ``ccpp_register`` itself is
        only emitted if any suite qualifies.  When omitted, no suite is
        treated as having a register phase.

    Returns
    -------
    list of str (no trailing newlines)
    """
    if len(suite_names) != len(suite_resolutions):
        raise CCPPError(
            'suite_names and suite_resolutions must have the same length'
        )

    mod_name = '{}_ccpp_cap'.format(host_name)

    lines: List[str] = []
    lines.append(
        '! {}.F90 -- generated by ccpp_capgen, do not edit'.format(mod_name)
    )
    lines.append('module {}'.format(mod_name))
    lines.append('')

    # Collect USE lines into a list so the trace helper can guarantee
    # ``error_unit`` is present.  The trace block writes to error_unit
    # and is emitted in every cap subroutine (gated by the module
    # ``trace`` parameter), so the USE is now unconditional.  Replaces
    # an earlier --no-host-introspection-only emission.
    use_lines: List[str] = []
    for sname in suite_names:
        suite_cap_mod = 'ccpp_{}_cap'.format(sname)
        suite_subs = []
        suite_subs.append('{}_register'.format(sname))
        suite_subs.append('{}_init'.format(sname))
        for phase in _PHYSICS_PHASES:
            suite_subs.append('{}_physics_{}'.format(sname, phase))
        suite_subs.append('{}_final'.format(sname))
        syms = ', '.join(suite_subs)
        use_lines.append('{}use {}, only: {}'.format(_INDENT, suite_cap_mod, syms))
    ensure_error_unit_use(use_lines, _INDENT)
    lines.extend(use_lines)

    # Re-export the host-facing constituent API + the constituent object so
    # host code can do ``use <host>_ccpp_cap, only: ...`` for *everything*
    # it needs from CCPP.  Mirrors original capgen, which put all of these
    # on the generated host cap module.  Only emitted when any suite uses
    # constituent state (the ccpp_host_constituents module is only emitted
    # in that case too).
    uses_consts = any(
        suite_resolution.uses_constituents or suite_resolution.constituent_register_calls
        for suite_resolution in suite_resolutions
    )
    constituent_pub_syms = [
        'ccpp_model_constituents_obj',
        'ccpp_register_constituents',
        'ccpp_initialize_constituents',
        'ccpp_is_scheme_constituent',
        'ccpp_number_constituents',
        'ccpp_gather_constituents',
        'ccpp_update_constituents',
        'ccpp_const_get_index',
        'ccpp_constituents_array',
        'ccpp_advected_constituents_array',
        'ccpp_model_const_properties',
        'ccpp_deallocate_dynamic_constituents',
    ]
    if uses_consts:
        lines.append('{}use ccpp_host_constituents, only: &'.format(_INDENT))
        for i, sym in enumerate(constituent_pub_syms):
            sep = ', &' if i < len(constituent_pub_syms) - 1 else ''
            lines.append('{}{}{}'.format(_INDENT * 2, sym, sep))

    lines.append('')
    lines.append('{}implicit none'.format(_INDENT))
    lines.append('{}private'.format(_INDENT))
    lines.append('')

    # Public declarations.  ccpp_register is mandatory.
    pub_subs = []
    pub_subs.append('ccpp_register')
    pub_subs.append('ccpp_init')
    for phase in _PHYSICS_PHASES:
        pub_subs.append('ccpp_physics_{}'.format(phase))
    pub_subs.append('ccpp_final')
    pub_subs.append('ccpp_physics_suite_list')
    pub_subs.append('ccpp_physics_suite_part_list')
    pub_subs.append('ccpp_physics_suite_schemes')
    pub_subs.append('ccpp_physics_suite_variables')
    pub_subs.append('ccpp_physics_suite_host_data')
    if uses_consts:
        pub_subs.extend(constituent_pub_syms)
    for sub in pub_subs:
        lines.append('{}public :: {}'.format(_INDENT, sub))

    lines.append('')
    lines.extend(emit_module_gate(trace, _INDENT))
    lines.append('')
    lines.append('contains')

    # Subroutines.
    lines.extend(_register_subroutine(suite_names, host_dict))
    lines.extend(_init_subroutine(suite_names, host_dict))
    for phase in _PHYSICS_PHASES:
        lines.extend(_physics_subroutine(phase, suite_names, suite_resolutions, host_dict))
    lines.extend(_final_subroutine(suite_names, host_dict))
    # Introspection routines (do not advance state, no scheme calls).
    # With --no-host-introspection, each routine retains its signature
    # but the body is replaced with an errflg=1 stub (or, for
    # suite_list, an error_unit write + empty allocation), shrinking
    # <host>_ccpp_cap.F90 dramatically for multi-suite builds.
    lines.extend(_suite_list_subroutine(
        suite_names, stub_body=no_host_introspection,
    ))
    lines.extend(_suite_part_list_subroutine(
        suite_names, suite_resolutions, stub_body=no_host_introspection,
    ))
    lines.extend(_suite_schemes_subroutine(
        suite_names, suite_resolutions, stub_body=no_host_introspection,
    ))
    lines.extend(_suite_io_subroutine(
        suite_names, suite_resolutions, host_dict, collapse_ddts=False,
        stub_body=no_host_introspection,
    ))
    lines.extend(_suite_io_subroutine(
        suite_names, suite_resolutions, host_dict, collapse_ddts=True,
        stub_body=no_host_introspection,
    ))

    lines.append('')
    lines.append('end module {}'.format(mod_name))
    return lines


########################################################################
# Public API
########################################################################

def write_host_cap(
    host_name: str,
    suite_names: List[str],
    suite_resolutions: List[SuiteResolution],
    output_root: str,
    host_dict=None,
    scheme_store: Optional[SchemeStore] = None,
    logger: Optional[logging.Logger] = None,
    no_host_introspection: bool = False,
    trace: bool = False,
) -> str:
    """Write ``<host>_ccpp_cap.F90`` to *output_root*.

    Parameters
    ----------
    host_name : str
        Host identifier; drives both the file name
        (``<host>_ccpp_cap.F90``) and the emitted module name
        (``module <host>_ccpp_cap``).
    suite_names : list of str
    suite_resolutions : list of SuiteResolution
        Parallel to suite_names.
    output_root : str
        Output directory (created if absent).
    host_dict : dict, optional
        Flat host+control dictionary for multi-instance support.
    scheme_store : SchemeStore, optional
        Used to detect which suites have at least one ``register``-providing
        scheme; only those drive emission of ``ccpp_register`` and the
        constituent module USE.  When omitted, ``ccpp_register`` is omitted
        entirely.
    no_host_introspection : bool, optional
        When True, replace the bodies of the five suite-introspection
        routines (``ccpp_physics_suite_list`` / ``..._suite_part_list`` /
        ``..._suite_schemes`` / ``..._suite_variables`` /
        ``..._suite_host_data``) with stubs that set ``errflg=1`` and a
        clear ``errmsg`` (or write to ``error_unit`` for
        ``ccpp_physics_suite_list``, which has no error channel).
        Signatures remain so existing callers still link.  Use this to
        shrink ``<host>_ccpp_cap.F90`` from ~33k lines to ~800 for
        multi-suite builds where the introspection case-blocks make
        even ``-O1`` compilation impractical.

    Returns
    -------
    str
        Absolute path of the written file.
    """
    os.makedirs(output_root, exist_ok=True)
    filename = '{}_ccpp_cap.F90'.format(host_name)
    out_path  = os.path.join(output_root, filename)

    lines = _generate_host_cap(
        host_name, suite_names, suite_resolutions, host_dict, scheme_store,
        no_host_introspection=no_host_introspection,
        trace=trace,
    )
    with open_if_changed(out_path, logger=logger) as fh:
        fh.write('\n'.join(lines) + '\n')
    return out_path
