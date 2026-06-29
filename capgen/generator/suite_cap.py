#!/usr/bin/env python3

"""Generate the suite-level cap module ``ccpp_<suite>_cap.F90``.

The suite cap:

* Imports all group cap modules and the constituent property module.
* Exposes eight public entry points:

  - ``<suite>_register`` — calls each scheme's ``_register`` to populate
    the host-owned ``ccpp_model_constituents_t`` object.
  - ``<suite>_init`` / ``<suite>_final`` — framework setup / teardown.
  - ``<suite>_physics_init``, ``<suite>_physics_timestep_init``,
    ``<suite>_physics_run``, ``<suite>_physics_timestep_final``,
    ``<suite>_physics_final`` — dispatch by ``group_name`` to the
    appropriate group cap subroutine.

The static API (``<host>_ccpp_cap.F90``) dispatches by ``suite_name`` to
these subroutines.
"""

import logging
import os
from typing import Dict, List, Optional, Set

from metadata.parse_tools import open_if_changed
from metadata.variable_resolver import HostVarEntry, SchemeStore
from generator.suite_resolver import (
    ResolvedArg,
    ResolvedGroup,
    SuiteResolution,
    iter_phase_calls,
    _root_symbol,
    # auto-clone-constituents: legacy-shim payload type.
    AutoCloneEntry,
)
from generator.trace import (
    emit_module_gate,
    emit_trace_block,
    ensure_error_unit_use,
)
from generator.group_cap import (
    _ctrl_args_for_phase,
    _ctrl_intent_for,
    _ctrl_local,
    _extra_dim_ctrl_entries,
    _ctrl_entries_for_signature,
    _fortran_type_str,
    _dim_decl,
    _instance_idx,
    _instance_local,
    _intent_clause,
)

_INDENT = '  '

# Canonical set of physics phases, always dispatched by the suite cap.
_PHYSICS_PHASES = ('init', 'timestep_init', 'run', 'timestep_final', 'final')

# Constituent type / module constants.
_CONST_MOD          = 'ccpp_constituent_prop_mod'
_CONST_DDT          = 'ccpp_model_constituents_t'
_CONST_PROP_TYPE    = 'ccpp_constituent_properties_t'
_CONST_PROP_PTR_TYPE = 'ccpp_constituent_prop_ptr_t'
_CONST_OBJ_STDNAME  = 'ccpp_model_constituents_object'

# Framework-provided constituent symbol names — emitted as suite-cap
# module variables when the suite references constituent state.
_CONST_BASE_ARRAY = 'ccpp_constituents'
_CONST_TEND_ARRAY = 'ccpp_constituent_tendencies'
_CONST_PROPS      = 'ccpp_constituent_properties'
_CONST_NUM        = 'number_of_ccpp_constituents'


########################################################################
# Helpers
########################################################################

def _all_suite_scheme_names(suite_res: SuiteResolution) -> List[str]:
    """Return deduplicated scheme names from all groups and phases.

    The order is first-seen across groups (alphabetical by group, then by
    order within each group's phase call list).

    >>> from generator.suite_resolver import SuiteResolution, ResolvedGroup, ResolvedCall
    >>> resolved_group = ResolvedGroup('grp', phase_calls={'run': [ResolvedCall('sch_a', 'run'), ResolvedCall('sch_b', 'run')]})
    >>> suite_resolution = SuiteResolution('s', groups=[resolved_group])
    >>> _all_suite_scheme_names(suite_resolution)
    ['sch_a', 'sch_b']
    """
    seen: Set[str] = set()
    names: List[str] = []
    for resolved_group in suite_res.groups:
        for items in resolved_group.phase_calls.values():
            for resolved_call in iter_phase_calls(items):
                if resolved_call.scheme_name not in seen:
                    seen.add(resolved_call.scheme_name)
                    names.append(resolved_call.scheme_name)
    return names


def _schemes_with_register(
    scheme_names: List[str],
    scheme_store: SchemeStore,
) -> List[str]:
    """Return those scheme names that have a ``register`` phase.

    >>> from unittest.mock import MagicMock
    >>> store = MagicMock()
    >>> store.phases_for.side_effect = lambda n: ['register', 'run'] if n == 'sch_a' else ['run']
    >>> _schemes_with_register(['sch_a', 'sch_b'], store)
    ['sch_a']
    """
    return [n for n in scheme_names if 'register' in scheme_store.phases_for(n)]


def _suite_ctrl_args_for_phase(
    suite_res: SuiteResolution,
    phase: str,
) -> List[ResolvedArg]:
    """Return the union of control args across all groups for *phase*.

    The result is deduplicated by standard_name and preserves first-seen order.
    """
    seen: Dict[str, ResolvedArg] = {}
    for resolved_group in suite_res.groups:
        for arg in _ctrl_args_for_phase(resolved_group, phase):
            if arg.standard_name not in seen:
                seen[arg.standard_name] = arg
    return list(seen.values())


def _group_ctrl_arg_names(resolved_group: ResolvedGroup, phase: str, host_dict=None) -> List[str]:
    """Return the local_name list for the control args of a group phase.

    These are the keyword names passed when calling the group cap subroutine.
    Includes extra control vars needed for state indexing and dimension subscripts
    (instance_number for suite-var access, control vars used only in dim subscripts).
    """
    ctrl_args = _ctrl_args_for_phase(resolved_group, phase)
    names = [
        a.host_entry.local_name
        for a in ctrl_args
        if a.host_entry is not None
    ]
    phase_items = resolved_group.phase_calls.get(phase, [])
    for entry in _extra_dim_ctrl_entries(phase_items, phase, ctrl_args, host_dict):
        if entry.local_name not in names:
            names.append(entry.local_name)
    return names


def _suite_extra_ctrl_entries_for_phase(
    suite_res: SuiteResolution,
    phase: str,
    ctrl_std_names: Set[str],
    host_dict,
) -> List[HostVarEntry]:
    """Return extra HostVarEntry objects needed by any group for *phase* but not
    already represented in *ctrl_std_names* (the direct scheme control args).

    This covers the same cases as ``_extra_dim_ctrl_entries`` but aggregated
    across all groups so the suite-level dispatch subroutine has them in its
    signature and can pass them down.
    """
    if host_dict is None:
        return []
    seen = set(ctrl_std_names)
    result: Dict[str, HostVarEntry] = {}
    for resolved_group in suite_res.groups:
        phase_items = resolved_group.phase_calls.get(phase, [])
        ctrl_args = _ctrl_args_for_phase(resolved_group, phase)
        for entry in _extra_dim_ctrl_entries(phase_items, phase, ctrl_args, host_dict):
            if entry.standard_name not in seen and entry.standard_name not in result:
                result[entry.standard_name] = entry
    return list(result.values())


########################################################################
# Subroutine generators
########################################################################

def _register_calls(suite_res: SuiteResolution):
    """Yield (group_name, ResolvedCall) for every register-phase scheme call.

    Groups are visited in suite-XML order; within each group the calls follow
    the resolver's ordering (which mirrors the suite XML).
    """
    for resolved_group in suite_res.groups:
        for resolved_call in iter_phase_calls(resolved_group.phase_calls.get('register', [])):
            yield resolved_group.group_name, resolved_call


def _register_uses(
    suite_res: SuiteResolution,
    suite_name: str,
    host_dict=None,
) -> Dict[str, Set[str]]:
    """Collect ``{module: {symbol}}`` requirements for register-phase scheme calls.

    Includes:
      - host modules for any host-owned register args,
      - ``ccpp_<suite>_data`` for any suite-owned register args,
      - one entry per scheme module for its ``_register`` symbol,
      - the constituent property type and the per-suite dynamic-constituent
        buffer (owned by ``ccpp_host_constituents``) when any register call
        produces constituents.
    """
    uses: Dict[str, Set[str]] = {}
    seen_schemes: Set[str] = set()
    for _gname, resolved_call in _register_calls(suite_res):
        if resolved_call.scheme_name not in seen_schemes:
            seen_schemes.add(resolved_call.scheme_name)
            # Module is metadata-declared (``module_name`` in table props)
            # when present; otherwise falls back to the scheme name.
            scheme_module = resolved_call.scheme_module or resolved_call.scheme_name
            uses.setdefault(scheme_module, set()).add(
                '{}_register'.format(resolved_call.scheme_name)
            )
        for arg in resolved_call.args:
            if arg.is_constituent_arg:
                continue   # local temp, not a USE'd var
            mod = arg.module_name
            if mod is not None:
                uses.setdefault(mod, set()).add(arg.root_symbol)
            # Dimension variables referenced in the arg's subscript (e.g.
            # ``rad_climate(1:rad_climate_dimension)``) must also be in
            # scope.  Mirror the group cap's _collect_dim_uses: a host dim
            # USEs its access-path root from the declaring module; a
            # suite-owned dim USEs ccpp_<suite>_data.  (Without this the
            # register subroutine references the dimension symbol with no
            # IMPLICIT type.)
            for dim_std in arg.used_dim_std_names:
                entry = host_dict.get(dim_std) if host_dict else None
                if entry is not None and entry.module_name is not None:
                    uses.setdefault(entry.module_name, set()).add(
                        _root_symbol(entry.access_path)
                    )
                elif dim_std in suite_res.suite_vars:
                    sv = suite_res.suite_vars[dim_std]
                    uses.setdefault(sv.module_name, set()).add('ccpp_suite_data')
    # Per-suite dynamic-constituent buffer is owned by ccpp_host_constituents
    # and written into here.  Pull in the constituent property type plus the
    # buffer symbol.
    # auto-clone-constituents: predicate centralised on SuiteResolution so
    # this site does not read legacy-shim state directly.
    if suite_res.needs_dynamic_constituents_buffer:
        uses.setdefault(_CONST_MOD, set()).add(_CONST_PROP_TYPE)
        buf = '{}_dynamic_constituents'.format(suite_name)
        uses.setdefault('ccpp_host_constituents', set()).add(buf)
    # auto-clone-constituents: the synthesised %instantiate calls
    # embed ``<value>_kind_phys`` literals when an entry sets any of
    # molar_mass / default_value / min_value.  Pull in ``kind_phys``
    # from ccpp_kinds in that case so the literal resolves.
    if any(
        e.default_value is not None
        or e.min_value is not None
        or (e.molar_mass and e.molar_mass != 0.0)
        for e in suite_res.auto_cloned_constituents
    ):
        uses.setdefault('ccpp_kinds', set()).add('kind_phys')
    return uses


def _add_call_uses(uses: Dict[str, Set[str]], resolved_call) -> None:
    """Merge USE-statement requirements for a single :class:`ResolvedCall`.

    Adds:
      * the scheme module → ``<scheme_name>_<phase>`` symbol so the call
        site can resolve;
      * each non-control arg's host/suite module → ``arg.root_symbol``
        (the top-level token of its access path) so the value is in
        scope.

    Mutates *uses* in place.  Used by ``<suite>_init`` and
    ``<suite>_final`` to integrate the suite-level <init>/<final>
    scheme calls into the USE block.
    """
    scheme_module = resolved_call.scheme_module or resolved_call.scheme_name
    uses.setdefault(scheme_module, set()).add(
        '{}_{}'.format(resolved_call.scheme_name, resolved_call.phase)
    )
    for arg in resolved_call.args:
        mod = arg.module_name
        if mod is not None:
            uses.setdefault(mod, set()).add(arg.root_symbol)


def _emit_register_call(resolved_call, indent: str, errflg_local: str, lines: List[str]) -> None:
    """Emit one scheme ``_register`` call with keyword args + error guard.

    Register-phase calls are kept simple: no transformations (transform code
    paths are physics-phase only), keyword-arg style for clarity.
    """
    sub = '{}_register'.format(resolved_call.scheme_name)
    if not resolved_call.args:
        lines.append('{}call {}()'.format(indent, sub))
    else:
        lines.append('{}call {}( &'.format(indent, sub))
        for i, arg in enumerate(resolved_call.args):
            sep = ', &' if i < len(resolved_call.args) - 1 else ')'
            lines.append('{}    {}={}{}'.format(
                indent, arg.scheme_local_name, arg.call_expr, sep
            ))
    if errflg_local:
        lines.append('{}if ({} /= 0) return'.format(indent, errflg_local))


# auto-clone-constituents: BEGIN legacy-shim emission helpers.
# Delete this block together with the rest of the
# auto-clone-constituents touchpoints.

def _esc_fortran_char(value: str) -> str:
    """Escape a Python string for embedding in a Fortran character
    literal — double every embedded single quote."""
    return value.replace("'", "''")


def _fmt_kind_phys_real(value) -> str:
    """Format a Python float as a Fortran ``kind_phys`` real literal.

    Always emits exponent notation so the result is unambiguously a
    real (no risk of being parsed as an integer literal) and reuses
    the same ``kind_phys`` suffix the framework's ``%instantiate``
    declares for ``default_value`` / ``min_value`` / ``molar_mass``.
    """
    return '{:.17e}_kind_phys'.format(float(value))


def _emit_auto_clone_instantiate(
    entry: AutoCloneEntry,
    buf: str,
    inst_idx: str,
    indent: str,
    errflg_local: str,
    errmsg_local: str,
    lines: List[str],
) -> None:
    """auto-clone-constituents: emit one synthesised ``%instantiate``
    call into the per-suite dynamic-constituents buffer for one
    :class:`AutoCloneEntry`.

    Required kwargs (std_name, long_name, diag_name, units,
    vertical_dim) are always emitted.  Optional kwargs are emitted
    only when the metadata explicitly set the value (``None`` on the
    backing field means "unset" — let the framework default kick in).
    ``errcode`` and ``errmsg`` are passed by keyword for clarity.
    """
    i = indent
    lines.append('{}num_consts = num_consts + 1'.format(i))
    lines.append(
        '{}call {}({})%items(num_consts)%instantiate( &'.format(
            i, buf, inst_idx,
        )
    )
    lines.append("{}    std_name     = '{}', &".format(
        i, _esc_fortran_char(entry.std_name)))
    lines.append("{}    long_name    = '{}', &".format(
        i, _esc_fortran_char(entry.long_name)))
    lines.append("{}    diag_name    = '{}', &".format(
        i, _esc_fortran_char(entry.diag_name)))
    lines.append("{}    units        = '{}', &".format(
        i, _esc_fortran_char(entry.units)))
    lines.append("{}    vertical_dim = '{}', &".format(
        i, _esc_fortran_char(entry.vertical_dim)))
    # Optional kwargs — only emit when explicitly set.  ``advected``
    # defaults to .false. in metadata; only emit when True so we
    # don't pollute the call with a redundant kwarg.
    if entry.advected:
        lines.append("{}    advected     = .true., &".format(i))
    if entry.molar_mass and entry.molar_mass != 0.0:
        lines.append("{}    molar_mass   = {}, &".format(
            i, _fmt_kind_phys_real(entry.molar_mass)))
    if entry.default_value is not None:
        lines.append("{}    default_value= {}, &".format(
            i, _fmt_kind_phys_real(entry.default_value)))
    if entry.min_value is not None:
        lines.append("{}    min_value    = {}, &".format(
            i, _fmt_kind_phys_real(entry.min_value)))
    if entry.water_species is not None:
        lines.append("{}    water_species= .{}., &".format(
            i, 'true' if entry.water_species else 'false'))
    if entry.mixing_ratio_type is not None:
        lines.append("{}    mixing_ratio_type = '{}', &".format(
            i, _esc_fortran_char(entry.mixing_ratio_type)))
    lines.append("{}    errcode      = {}, &".format(i, errflg_local))
    lines.append("{}    errmsg       = {})".format(i, errmsg_local))
    lines.append('{}if ({} /= 0) return'.format(i, errflg_local))

# auto-clone-constituents: END legacy-shim emission helpers.


def _register_lines(
    suite_name: str,
    suite_res: SuiteResolution,
    host_dict=None,
) -> List[str]:
    """Generate the ``<suite>_register`` subroutine lines.

    Mandatory entry point: emitted unconditionally.  Allocates the suite
    state array and the suite-owned DDT array on first call, dispatches each
    register-phase scheme call across all groups in suite-XML order, and
    transitions the suite state for this instance to ``CCPP_SUITE_REGISTERED``.

    Minimal signature: ``(instance_number, number_of_instances, errmsg,
    errflg)`` (the instance pair is included only when the host declares it).
    """
    sub_name = '{}_register'.format(suite_name)
    i1 = _INDENT
    i2 = _INDENT * 2

    inst_local = _instance_local(host_dict)
    inst_idx   = _instance_idx(host_dict)

    # ``number_of_instances`` is now a paired control variable (see
    # ccpp_capgen._PAIRED_OPTIONAL_CTRL_VARS).  When present it enters
    # the suite-cap signature as a dummy alongside ``instance_number``;
    # the framework consumes it at register-time to size the per-instance
    # state arrays.  When absent we fall back to the literal ``1``
    # (single-instance API).
    ninstances_entry = host_dict.get('number_of_instances') if host_dict else None
    ninstances_local = ninstances_entry.local_name if ninstances_entry else None
    ninstances_arg   = ninstances_local if ninstances_local else '1'

    errflg_local = _ctrl_local(host_dict, 'ccpp_error_code') or 'errflg'
    errmsg_local = _ctrl_local(host_dict, 'ccpp_error_message') or 'errmsg'

    sig_args: List[str] = []
    if inst_local:
        sig_args.append(inst_local)
    if ninstances_local:
        sig_args.append(ninstances_local)
    sig_args += [errmsg_local, errflg_local]

    lines: List[str] = []
    lines.append('')
    lines.append('{}subroutine {}({})'.format(i1, sub_name, ', '.join(sig_args)))

    # USE statements: scheme modules + host/suite-data modules referenced by
    # register-phase scheme args.  (``number_of_instances`` used to be USE'd
    # from the host module here; now it arrives as a dummy argument.)
    reg_uses = _register_uses(suite_res, suite_name, host_dict)
    for mod in sorted(reg_uses):
        syms = ', '.join(sorted(reg_uses[mod]))
        lines.append('{}use {}, only: {}'.format(i2, mod, syms))

    lines.append('')
    if inst_local:
        lines.append('{}integer, intent(in) :: {}'.format(i2, inst_local))
    if ninstances_local:
        lines.append('{}integer, intent(in) :: {}'.format(i2, ninstances_local))
    lines += [
        '{}character(len=*), intent(out) :: {}'.format(i2, errmsg_local),
        '{}integer, intent(out) :: {}'.format(i2, errflg_local),
    ]

    # Constituent merge: declare a per-scheme array temporary and a counter.
    has_consts = bool(suite_res.constituent_register_calls)
    # auto-clone-constituents: the legacy shim contributes additional
    # constituent registrations synthesised in capgen from
    # is_constituent consumer metadata; ``has_dyn_consts`` covers
    # both sources so the buffer-allocation + counter machinery is
    # set up whenever any synthesised constituent will be emitted.
    has_auto_cloned = bool(suite_res.auto_cloned_constituents)
    has_dyn_consts  = has_consts or has_auto_cloned
    if has_dyn_consts:
        lines.append('')
        if has_consts:
            # Each constituent scheme's _register returns its array into this
            # temp; it is appended to the buffer and reused for the next
            # scheme, so _register is called EXACTLY ONCE per scheme.
            lines.append(
                '{}type({}), allocatable :: scheme_consts(:)'.format(
                    i2, _CONST_PROP_TYPE
                )
            )
        if has_auto_cloned:
            # Counter used only by the auto-clone-constituents buffer growth.
            lines.append('{}integer :: num_consts'.format(i2))

    # Trace block: dummies referenced inside the gated write so strict
    # compilers don't flag intent(in) args as unused when the gate is off.
    extra_in = [ninstances_local] if ninstances_local else None
    trace_lines = emit_trace_block(
        sub_name, [], i2,
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
    ]

    # Allocate state and DDT array on first call (idempotent).
    suite_alloc_sub = '{}_suite_state_alloc'.format(suite_name)
    lines.append('{}call {}({}, {}, {})'.format(
        i2, suite_alloc_sub, ninstances_arg, errmsg_local, errflg_local
    ))
    lines.append('{}if ({} /= 0) return'.format(i2, errflg_local))
    lines.append('')

    # Per-instance idempotent skip: already registered or further along.
    lines.append(
        '{}if (ccpp_suite_state({}) >= CCPP_SUITE_REGISTERED) return'.format(
            i2, inst_idx
        )
    )
    lines.append('')

    if has_dyn_consts:
        # Pack constituent-producing schemes' arrays into the per-suite
        # buffer in ccpp_host_constituents.  The actual merge into each
        # instance's ``ccpp_model_constituents_obj(inst)`` happens later
        # when the host calls ``ccpp_register_constituents`` per instance.
        #
        # Each instance owns its own slot ``<buf>(inst)%items(:)``: the
        # property objects are independent across instances so that
        # ``ccpp_register_constituents`` can ``set_const_index`` on each
        # without conflicting with other instances.  The outer wrapper
        # array is allocated once on first call (any instance); each
        # instance then appends each scheme's constituents into its slot,
        # calling every scheme's ``_register`` EXACTLY ONCE (register may
        # allocate persistent module state, so the earlier two-pass
        # count+copy that called it twice broke non-idempotent schemes such
        # as ``prescribed_aerosols_register``).  The state-machine guard
        # above this block ensures each instance runs the fill at most once.
        #
        # auto-clone-constituents: the legacy shim contributes one
        # additional ``%instantiate`` per consumer-side
        # ``is_constituent`` arg with no register-phase source.  Those
        # synthesised entries are appended to the same per-instance buffer
        # slot after the scheme-registered entries.
        const_scheme_names = {scheme_name for scheme_name, _ in suite_res.constituent_register_calls}
        buf = '{}_dynamic_constituents'.format(suite_name)
        n_auto_clone = len(suite_res.auto_cloned_constituents)

        # Allocate the outer wrapper array on first call (any instance).
        lines.append(
            '{}if (.not. allocated({})) then'.format(i2, buf)
        )
        lines.append('{}allocate({}({}))'.format(
            i2 + _INDENT, buf, ninstances_arg,
        ))
        lines.append('{}end if'.format(i2))
        lines.append('')

        # Single pass: call each constituent scheme's _register EXACTLY ONCE
        # and append its returned array to this instance's slot.  Start from
        # an empty slot and grow it; intrinsic assignment of the constituent
        # array constructor deep-copies each entry (same assignment the old
        # copy loop used element-wise).
        lines.append(
            "{}! Pack each scheme's constituents (register run once each).".format(i2)
        )
        lines.append('{}allocate({}({})%items(0))'.format(i2, buf, inst_idx))
        for _gname, resolved_call in _register_calls(suite_res):
            if resolved_call.scheme_name in const_scheme_names:
                _emit_register_call(resolved_call, i2, errflg_local, lines)
                lines.append(
                    '{0}{1}({2})%items = [{1}({2})%items, scheme_consts]'.format(
                        i2, buf, inst_idx,
                    )
                )
                lines.append('{}deallocate(scheme_consts)'.format(i2))
        # auto-clone-constituents: reserve n_auto_clone trailing slots after
        # the scheme-registered entries, then instantiate into them.
        if n_auto_clone > 0:
            lines.append('')
            lines.append(
                '{}! auto-clone-constituents: reserve + instantiate synthesised entries'.format(
                    i2,
                )
            )
            lines.append(
                '{}num_consts = size({}({})%items, 1)'.format(i2, buf, inst_idx)
            )
            if has_consts:
                # Grow the existing scheme-registered slot by n_auto_clone.
                lines.append(
                    '{}call move_alloc({}({})%items, scheme_consts)'.format(
                        i2, buf, inst_idx,
                    )
                )
                lines.append(
                    '{}allocate({}({})%items(num_consts + {}))'.format(
                        i2, buf, inst_idx, n_auto_clone,
                    )
                )
                lines.append(
                    '{0}if (num_consts > 0) {1}({2})%items(1:num_consts) = '
                    'scheme_consts(1:num_consts)'.format(i2, buf, inst_idx)
                )
                lines.append(
                    '{}if (allocated(scheme_consts)) deallocate(scheme_consts)'.format(i2)
                )
            else:
                # No scheme-registered entries: just size the slot directly.
                lines.append('{}deallocate({}({})%items)'.format(i2, buf, inst_idx))
                lines.append(
                    '{}allocate({}({})%items({}))'.format(
                        i2, buf, inst_idx, n_auto_clone,
                    )
                )
            for entry in suite_res.auto_cloned_constituents:
                _emit_auto_clone_instantiate(
                    entry, buf, inst_idx, i2, errflg_local, errmsg_local, lines,
                )
        lines.append('')
        # Emit any non-constituent register calls in addition (always, per instance).
        for _gname, resolved_call in _register_calls(suite_res):
            if resolved_call.scheme_name not in const_scheme_names:
                _emit_register_call(resolved_call, i2, errflg_local, lines)
    else:
        # No constituent merge — emit register calls in suite-XML order.
        for _gname, resolved_call in _register_calls(suite_res):
            _emit_register_call(resolved_call, i2, errflg_local, lines)

    lines.append('')
    lines.append(
        '{}ccpp_suite_state({}) = CCPP_SUITE_REGISTERED'.format(i2, inst_idx)
    )
    lines.append('')
    lines.append('{}end subroutine {}'.format(i1, sub_name))
    return lines


def _init_lines(
    suite_name: str,
    suite_res: SuiteResolution,
    host_dict=None,
) -> List[str]:
    """Generate the ``<suite>_init`` framework-setup subroutine lines.

    Per-instance lifecycle: every call passes ``instance_number`` (when the host
    declares it).  Requires the suite to be in ``CCPP_SUITE_REGISTERED`` (i.e.
    ``ccpp_register`` was called).  The body:

    1. Verifies state is ``REGISTERED``; idempotent skip if already
       ``FRAMEWORK_INITIALIZED``; error otherwise.
    2. Calls each group ``state_alloc`` routine (idempotent first-call alloc).
    3. Calls the suite-data ``init_fields`` routine which allocates inner
       allocatable suite-data fields using suite-owned dim values that may have
       been written during the register phase.
    4. Sets ``ccpp_suite_state(instance_number) = CCPP_SUITE_FRAMEWORK_INITIALIZED``.

    Minimal signature: ``(instance_number, number_of_instances, errmsg,
    errflg)`` -- the instance pair is included only when the host declares it.
    """
    sub_name = '{}_init'.format(suite_name)
    i1 = _INDENT
    i2 = _INDENT * 2

    # ``number_of_instances`` is now a paired control variable; it arrives
    # as a dummy argument rather than via ``use <host_mod>``.
    ninstances_entry = host_dict.get('number_of_instances') if host_dict else None
    ninstances_local = ninstances_entry.local_name if ninstances_entry else None
    ninstances_arg   = ninstances_local if ninstances_local else '1'

    inst_local = _instance_local(host_dict)
    inst_idx   = _instance_idx(host_dict)

    errflg_local = _ctrl_local(host_dict, 'ccpp_error_code') or 'errflg'
    errmsg_local = _ctrl_local(host_dict, 'ccpp_error_message') or 'errmsg'

    sig_args: List[str] = []
    if inst_local:
        sig_args.append(inst_local)
    if ninstances_local:
        sig_args.append(ninstances_local)
    sig_args += [errmsg_local, errflg_local]

    lines: List[str] = ['']
    lines.append('{}subroutine {}({})'.format(i1, sub_name, ', '.join(sig_args)))

    # USE: suite_data init_fields routine when this suite owns any vars;
    # constituent object (from host module) for pointer binding;
    # suite-level <init> scheme module + per-arg host modules.
    # (``number_of_instances`` used to be USE'd here; now it's a dummy arg.)
    extra_uses: Dict[str, Set[str]] = {}
    if suite_res.suite_vars:
        data_mod    = 'ccpp_{}_data'.format(suite_name)
        init_fields = 'suite_data_init_fields'
        extra_uses.setdefault(data_mod, set()).add(init_fields)
    if suite_res.suite_init_call is not None:
        _add_call_uses(extra_uses, suite_res.suite_init_call)
    for mod in sorted(extra_uses):
        syms = ', '.join(sorted(extra_uses[mod]))
        lines.append('{}use {}, only: {}'.format(i2, mod, syms))

    lines.append('')
    if inst_local:
        lines.append('{}integer, intent(in) :: {}'.format(i2, inst_local))
    if ninstances_local:
        lines.append('{}integer, intent(in) :: {}'.format(i2, ninstances_local))
    lines += [
        '{}character(len=*), intent(out) :: {}'.format(i2, errmsg_local),
        '{}integer, intent(out) :: {}'.format(i2, errflg_local),
    ]
    extra_in = [ninstances_local] if ninstances_local else None
    trace_lines = emit_trace_block(
        sub_name, [], i2,
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
    ]

    # State guard: must be in REGISTERED state (or already INITIALIZED — idempotent).
    sub_label = '{}_init'.format(suite_name)
    lines += [
        '{}if (.not. allocated(ccpp_suite_state)) then'.format(i2),
        "{}  {} = '{}: ccpp_register has not been called'".format(
            i2, errmsg_local, sub_label
        ),
        '{}  {} = 1'.format(i2, errflg_local),
        '{}  return'.format(i2),
        '{}end if'.format(i2),
        '{}if (ccpp_suite_state({}) == CCPP_SUITE_FRAMEWORK_INITIALIZED) return'.format(
            i2, inst_idx
        ),
        '{}if (ccpp_suite_state({}) /= CCPP_SUITE_REGISTERED) then'.format(
            i2, inst_idx
        ),
        "{}  {} = '{}: invalid suite state (expected REGISTERED)'".format(
            i2, errmsg_local, sub_label
        ),
        '{}  {} = 1'.format(i2, errflg_local),
        '{}  return'.format(i2),
        '{}end if'.format(i2),
        '',
    ]

    # Group state allocators (idempotent).
    for resolved_group in suite_res.groups:
        alloc_sub = '{}_state_alloc'.format(resolved_group.group_name)
        lines.append('{}call {}({}, {}, {})'.format(
            i2, alloc_sub, ninstances_arg, errmsg_local, errflg_local
        ))
        lines.append('{}if ({} /= 0) return'.format(i2, errflg_local))

    # Allocate inner suite-data allocatable fields for this instance.
    if suite_res.suite_vars:
        init_fields = 'suite_data_init_fields'
        lines.append('{}call {}({}, {}, {})'.format(
            i2, init_fields, inst_idx, errmsg_local, errflg_local
        ))
        lines.append('{}if ({} /= 0) return'.format(i2, errflg_local))

    # Constituent state binding is owned by the host_constituents module
    # under option A — the host calls ccpp_initialize_constituents separately
    # to bind ccpp_constituents/ccpp_constituent_tendencies and populate the
    # index_of_<X> integers.

    # Suite-level <init> scheme call (if declared in the SDF).  Runs once
    # per ``<suite>_init`` invocation, after all group state allocators
    # have populated their state arrays, and before the suite-state
    # transition to FRAMEWORK_INITIALIZED.  Errflg check follows the call.
    if suite_res.suite_init_call is not None:
        lines.append('')
        from generator.group_cap import _emit_one_call
        _emit_one_call(suite_res.suite_init_call, i2, lines)

    lines += [
        '',
        '{}ccpp_suite_state({}) = CCPP_SUITE_FRAMEWORK_INITIALIZED'.format(
            i2, inst_idx
        ),
        '',
        '{}end subroutine {}'.format(i1, sub_name),
    ]
    return lines


def _final_lines(
    suite_name: str,
    suite_res: SuiteResolution,
    host_dict=None,
) -> List[str]:
    """Generate the ``<suite>_final`` framework-teardown subroutine lines.

    Per-instance lifecycle:

    1. Errors if ``ccpp_suite_state`` is not allocated.
    2. Per-instance idempotent skip: returns immediately if this instance's slot
       is already ``CCPP_SUITE_UNREGISTERED``.
    3. Calls suite-data ``final_fields`` for this instance to deallocate inner
       allocatable suite-data fields (when this suite owns any).
    4. Sets ``ccpp_suite_state(instance_number) = CCPP_SUITE_UNREGISTERED``.
    5. Last-to-leave dealloc: when every slot is ``UNREGISTERED`` after the
       flip, calls each group ``state_dealloc`` and the suite ``state_dealloc``
       (which also tears down the suite_data DDT array).

    Signature: ``(instance_number, number_of_instances, errmsg, errflg)``
    when the host declares the multi-instance pair, else
    ``(errmsg, errflg)``.  ``number_of_instances`` is carried for API
    symmetry with ``<suite>_register`` / ``<suite>_init``; the framework
    does not consume it at final time.
    """
    sub_name = '{}_final'.format(suite_name)
    i1 = _INDENT
    i2 = _INDENT * 2

    inst_local = _instance_local(host_dict)
    inst_idx   = _instance_idx(host_dict)
    ninstances_entry = host_dict.get('number_of_instances') if host_dict else None
    ninstances_local = ninstances_entry.local_name if ninstances_entry else None

    errflg_local = _ctrl_local(host_dict, 'ccpp_error_code') or 'errflg'
    errmsg_local = _ctrl_local(host_dict, 'ccpp_error_message') or 'errmsg'

    sig_args: List[str] = []
    if inst_local:
        sig_args.append(inst_local)
    if ninstances_local:
        sig_args.append(ninstances_local)
    sig_args += [errmsg_local, errflg_local]

    lines: List[str] = ['']
    lines.append('{}subroutine {}({})'.format(i1, sub_name, ', '.join(sig_args)))

    final_uses: Dict[str, Set[str]] = {}
    if suite_res.suite_vars:
        data_mod     = 'ccpp_{}_data'.format(suite_name)
        final_fields = 'suite_data_final_fields'
        final_uses.setdefault(data_mod, set()).add(final_fields)

    # If we registered constituents, the per-suite buffer (owned by
    # ccpp_host_constituents) is torn down here in the last-to-leave block.
    if suite_res.constituent_register_calls:
        buf = '{}_dynamic_constituents'.format(suite_name)
        final_uses.setdefault('ccpp_host_constituents', set()).add(buf)

    # Suite-level <final> scheme call (if declared in the SDF) — pull
    # in the scheme module and any host modules its args reference.
    if suite_res.suite_final_call is not None:
        _add_call_uses(final_uses, suite_res.suite_final_call)

    for mod in sorted(final_uses):
        syms = ', '.join(sorted(final_uses[mod]))
        lines.append('{}use {}, only: {}'.format(i2, mod, syms))

    lines.append('')
    if inst_local:
        lines.append('{}integer, intent(in) :: {}'.format(i2, inst_local))
    if ninstances_local:
        lines.append('{}integer, intent(in) :: {}'.format(i2, ninstances_local))
    lines += [
        '{}character(len=*), intent(out) :: {}'.format(i2, errmsg_local),
        '{}integer, intent(out) :: {}'.format(i2, errflg_local),
    ]
    extra_in = [ninstances_local] if ninstances_local else None
    trace_lines = emit_trace_block(
        sub_name, [], i2,
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
        # ``<suite>_final`` is silently idempotent: a repeat call must return
        # cleanly with ``errflg=0``.  After the first call's last-to-leave
        # teardown the state array is deallocated, so the unallocated path is
        # the normal post-final state — silent-return rather than error.  The
        # per-instance ``UNREGISTERED`` skip covers any other instance that
        # was already finalized before the last-to-leave dealloc fired.
        '{}if (.not. allocated(ccpp_suite_state)) return'.format(i2),
        '{}if (ccpp_suite_state({}) == CCPP_SUITE_UNREGISTERED) return'.format(
            i2, inst_idx
        ),
        '',
    ]

    # Deallocate inner suite-data fields if this instance was past REGISTERED.
    if suite_res.suite_vars:
        final_fields = 'suite_data_final_fields'
        lines.append(
            '{}if (ccpp_suite_state({}) == CCPP_SUITE_FRAMEWORK_INITIALIZED) then'.format(
                i2, inst_idx
            )
        )
        lines.append('{}  call {}({}, {}, {})'.format(
            i2, final_fields, inst_idx, errmsg_local, errflg_local
        ))
        lines.append('{}  if ({} /= 0) return'.format(i2, errflg_local))
        lines.append('{}end if'.format(i2))
        lines.append('')

    # Suite-level <final> scheme call (if declared in the SDF).  Runs
    # once per ``<suite>_final`` invocation, before the suite-state
    # transition to UNREGISTERED.  Errflg check follows the call.
    if suite_res.suite_final_call is not None:
        from generator.group_cap import _emit_one_call
        _emit_one_call(suite_res.suite_final_call, i2, lines)

    lines.append(
        '{}ccpp_suite_state({}) = CCPP_SUITE_UNREGISTERED'.format(i2, inst_idx)
    )
    lines.append('')
    lines.append(
        '{}if (all(ccpp_suite_state == CCPP_SUITE_UNREGISTERED)) then'.format(i2)
    )
    for resolved_group in suite_res.groups:
        dealloc_sub = '{}_state_dealloc'.format(resolved_group.group_name)
        lines.append('{}  call {}({}, {})'.format(
            i2, dealloc_sub, errmsg_local, errflg_local
        ))
        lines.append('{}  if ({} /= 0) return'.format(i2, errflg_local))
    suite_dealloc_sub = '{}_suite_state_dealloc'.format(suite_name)
    lines.append('{}  call {}({}, {})'.format(
        i2, suite_dealloc_sub, errmsg_local, errflg_local
    ))
    lines.append('{}  if ({} /= 0) return'.format(i2, errflg_local))
    # Constituent OBJ teardown lives in ccpp_deallocate_dynamic_constituents
    # (the host calls it per instance + last-to-leave dealloc).  The
    # per-suite ``<suite>_dynamic_constituents`` buffer, however, is
    # tied to THIS suite's lifecycle — populated by ``<suite>_register``
    # under the suite-cap state guard — so it must be deallocated here
    # in the last-to-leave block, not in the constituent-deallocate
    # routine.  Otherwise the next ``ccpp_register`` short-circuits on
    # the state guard without re-filling the buffer.
    if suite_res.constituent_register_calls:
        buf = '{}_dynamic_constituents'.format(suite_name)
        lines.append('{}  if (allocated({})) deallocate({})'.format(
            i2, buf, buf,
        ))
    lines += [
        '{}end if'.format(i2),
        '',
        '{}end subroutine {}'.format(i1, sub_name),
    ]
    return lines


def _physics_dispatch_lines(
    suite_name: str,
    phase: str,
    suite_res: SuiteResolution,
    host_dict=None,
) -> List[str]:
    """Generate a ``<suite>_physics_<phase>`` dispatch subroutine.

    The subroutine signature is derived entirely from the host's ``type=control``
    metadata (all control variables except ``suite_name``, which is consumed at the
    static API dispatch level).  When ``group_name`` is in the control table the
    body uses a ``select case`` dispatch; otherwise all groups are called
    unconditionally.
    """
    sub_name = '{}_physics_{}'.format(suite_name, phase)
    i1 = _INDENT
    i2 = _INDENT * 2
    i3 = _INDENT * 3

    # Suite-level signature: all ctrl vars excluding suite_name.
    ctrl_entries = _ctrl_entries_for_signature(host_dict, exclude={'suite_name'})
    ctrl_local_names = [e.local_name for e in ctrl_entries]

    # Determine if group_name is in the control table.
    group_name_entry = next(
        (e for e in ctrl_entries if e.standard_name == 'group_name'), None
    )
    has_group_name = group_name_entry is not None

    # Group-level args passed when calling group cap subroutines.
    group_ctrl_entries = _ctrl_entries_for_signature(
        host_dict, exclude={'suite_name', 'group_name'}
    )
    group_ctrl_local = [e.local_name for e in group_ctrl_entries]

    lines: List[str] = []
    lines.append('')

    # Subroutine signature.
    if ctrl_local_names:
        lines.append('{}subroutine {}( &'.format(i1, sub_name))
        for i, lname in enumerate(ctrl_local_names):
            sep = ', &' if i < len(ctrl_local_names) - 1 else ')'
            lines.append('{}    {}{}'.format(i1, lname, sep))
    else:
        lines.append('{}subroutine {}()'.format(i1, sub_name))

    # Dummy argument declarations.
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

    # Trace block: references every intent(in)/inout control dummy so that
    # strict compilers don't flag any of them as unused.
    trace_lines = emit_trace_block(sub_name, ctrl_entries, i2)
    if trace_lines:
        lines.append('')
        lines.extend(trace_lines)

    lines.append('')

    # Initialize error reporting vars before any work, then guard on the
    # per-instance suite state.
    errflg_local = _ctrl_local(host_dict, 'ccpp_error_code')
    errmsg_local = _ctrl_local(host_dict, 'ccpp_error_message')
    if errflg_local and errmsg_local:
        lines.append("{}{} = ''".format(i2, errmsg_local))
        lines.append('{}{} = 0'.format(i2, errflg_local))
        lines.append('')

        inst_idx = _instance_idx(host_dict)
        sub_label = '{}_physics_{}'.format(suite_name, phase)
        if phase == 'final':
            # ``physics_final`` is silently idempotent: a repeat call (or a
            # call issued after ``ccpp_final``) must return cleanly with
            # ``errflg=0`` rather than erroring.  The group-level guard
            # handles the per-group skip when ``ccpp_final`` has not been
            # called; the two checks below cover the post-``ccpp_final``
            # cases (state array deallocated on the last instance, or set
            # to ``UNREGISTERED`` on any other instance).
            lines += [
                '{}if (.not. allocated(ccpp_suite_state)) return'.format(i2),
                '{}if (ccpp_suite_state({}) == CCPP_SUITE_UNREGISTERED) return'.format(
                    i2, inst_idx
                ),
            ]
        else:
            lines += [
                '{}if (.not. allocated(ccpp_suite_state)) then'.format(i2),
                "{}  {} = '{}: ccpp_register has not been called'".format(
                    i2, errmsg_local, sub_label
                ),
                '{}  {} = 1'.format(i2, errflg_local),
                '{}  return'.format(i2),
                '{}end if'.format(i2),
            ]
        lines += [
            '{}if (ccpp_suite_state({}) /= CCPP_SUITE_FRAMEWORK_INITIALIZED) then'.format(
                i2, inst_idx
            ),
            "{}  {} = '{}: invalid suite state'".format(
                i2, errmsg_local, sub_label
            ),
            '{}  {} = 1'.format(i2, errflg_local),
            '{}  return'.format(i2),
            '{}end if'.format(i2),
            '',
        ]

    def _emit_group_call(resolved_group, indent):
        # Group phase subroutines are always emitted (so the per-group state
        # machine transitions through every phase), so we always dispatch.
        cap_sub = '{}_{}'.format(resolved_group.group_name, phase)
        if group_ctrl_local:
            lines.append('{}call {}( &'.format(indent, cap_sub))
            for idx, lname in enumerate(group_ctrl_local):
                sep = ', &' if idx < len(group_ctrl_local) - 1 else ')'
                lines.append('{}    {}{}'.format(indent, lname, sep))
        else:
            lines.append('{}call {}()'.format(indent, cap_sub))
        # Stop and propagate on the first group error.  Each group phase
        # subroutine resets ``errflg = 0`` on entry, so without this guard a
        # ``group_name='all'`` dispatch would let a LATER group's success
        # overwrite an EARLIER group's failure -- the error (and its message)
        # would be silently masked and only resurface downstream as an
        # "invalid group state" when ``run`` finds the failed group never
        # reached ``IN_TIMESTEP``.  Mirrors the per-scheme call guards.
        if errflg_local:
            lines.append('{}if ({} /= 0) return'.format(indent, errflg_local))

    if has_group_name:
        grp_local = group_name_entry.local_name
        lines.append('{}select case(trim({}))'.format(i2, grp_local))
        # '' or 'all' → call all groups.
        lines.append("{}case('', 'all')".format(i2))
        for resolved_group in suite_res.groups:
            _emit_group_call(resolved_group, i3)
        # Individual group cases.
        for resolved_group in suite_res.groups:
            lines.append("{}case('{}')".format(i2, resolved_group.group_name))
            _emit_group_call(resolved_group, i3)
        # case default: anything other than '', 'all', or a known group
        # is a runtime error — caller asked for a group this suite
        # doesn't define.  Without ccpp_error_code/_message in the host
        # control table there's nowhere to write the message, so skip
        # emission rather than silently swallow.
        if errflg_local and errmsg_local:
            sub_label = '{}_physics_{}'.format(suite_name, phase)
            lines.append('{}case default'.format(i2))
            lines.append('{}{} = 1'.format(i3, errflg_local))
            lines.append(
                "{}{} = '{}: unknown group: ' // trim({})".format(
                    i3, errmsg_local, sub_label, grp_local,
                )
            )
            lines.append('{}return'.format(i3))
        lines.append('{}end select'.format(i2))
    else:
        # No group_name control var: call all groups unconditionally.
        for resolved_group in suite_res.groups:
            _emit_group_call(resolved_group, i2)

    lines.append('')
    lines.append('{}end subroutine {}'.format(i1, sub_name))
    return lines


def _suite_state_alloc_lines(
    suite_name: str,
    has_suite_vars: bool,
) -> List[str]:
    """Generate the ``<suite>_suite_state_alloc`` subroutine.

    Idempotent allocator for the per-instance suite state array and the
    suite-owned DDT array.  Inner allocatable fields inside the DDT are NOT
    allocated here — that happens in ``ccpp_<suite>_suite_data_init_fields``,
    called from ``<suite>_init`` after register-phase scheme calls have set
    any suite-owned scalar dimensions.
    """
    sub_name    = '{}_suite_state_alloc'.format(suite_name)
    data_alloc  = 'suite_data_alloc'
    data_mod    = 'ccpp_{}_data'.format(suite_name)
    i1 = _INDENT
    i2 = _INDENT * 2
    lines = [
        '',
        '{}subroutine {}(number_of_instances, errmsg, errflg)'.format(i1, sub_name),
    ]
    if has_suite_vars:
        lines.append('{}use {}, only: {}'.format(i2, data_mod, data_alloc))
    lines += [
        '',
        '{}integer, intent(in) :: number_of_instances'.format(i2),
        '{}character(len=*), intent(out) :: errmsg'.format(i2),
        '{}integer, intent(out) :: errflg'.format(i2),
        '',
        "{}errmsg = ''".format(i2),
        '{}errflg = 0'.format(i2),
        '{}if (allocated(ccpp_suite_state)) return'.format(i2),
        '{}allocate(ccpp_suite_state(number_of_instances))'.format(i2),
        '{}ccpp_suite_state(:) = CCPP_SUITE_UNREGISTERED'.format(i2),
    ]
    if has_suite_vars:
        lines += [
            '{}call {}(number_of_instances, errmsg, errflg)'.format(i2, data_alloc),
            '{}if (errflg /= 0) return'.format(i2),
        ]
    lines += [
        '',
        '{}end subroutine {}'.format(i1, sub_name),
    ]
    return lines


def _suite_state_dealloc_lines(
    suite_name: str,
    has_suite_vars: bool,
) -> List[str]:
    """Generate the ``<suite>_suite_state_dealloc`` subroutine."""
    sub_name      = '{}_suite_state_dealloc'.format(suite_name)
    data_dealloc  = 'suite_data_dealloc'
    data_mod      = 'ccpp_{}_data'.format(suite_name)
    i1 = _INDENT
    i2 = _INDENT * 2
    lines = [
        '',
        '{}subroutine {}(errmsg, errflg)'.format(i1, sub_name),
    ]
    if has_suite_vars:
        lines.append('{}use {}, only: {}'.format(i2, data_mod, data_dealloc))
    lines += [
        '',
        '{}character(len=*), intent(out) :: errmsg'.format(i2),
        '{}integer, intent(out) :: errflg'.format(i2),
        '',
        "{}errmsg = ''".format(i2),
        '{}errflg = 0'.format(i2),
    ]
    if has_suite_vars:
        lines += [
            '{}call {}(errmsg, errflg)'.format(i2, data_dealloc),
            '{}if (errflg /= 0) return'.format(i2),
        ]
    lines += [
        '{}if (allocated(ccpp_suite_state)) deallocate(ccpp_suite_state)'.format(i2),
        '',
        '{}end subroutine {}'.format(i1, sub_name),
    ]
    return lines


########################################################################
# Module generator
########################################################################

def _generate_suite_cap(
    suite_name: str,
    suite_res: SuiteResolution,
    scheme_store: SchemeStore,
    host_dict=None,
    trace: bool = False,
) -> List[str]:
    """Generate the full ``ccpp_<suite>_cap.F90`` module source lines.

    Parameters
    ----------
    suite_name : str
    suite_res : SuiteResolution
    scheme_store : SchemeStore
    host_dict : dict, optional
        Flat host+control variable dictionary.  When provided, ``number_of_instances``
        and ``instance_number`` are used for multi-instance state array sizing and
        indexing.

    Returns
    -------
    list of str (no trailing newlines)
    """
    mod_name = 'ccpp_{}_cap'.format(suite_name)
    lines: List[str] = []

    # Module header.
    lines.append(
        '! ccpp_{}_cap.F90 -- generated by ccpp_capgen, do not edit'.format(
            suite_name
        )
    )
    lines.append('module {}'.format(mod_name))
    lines.append('')

    # USE statements: one per group cap (all phase + state subroutines).
    # Group cap subroutine names are short (``<group>_<phase>`` etc.) so
    # the mangled global ``<mod>_mp_<sub>`` stays under Intel's ~90-char
    # limit even for long suite/group name combinations.
    use_lines: List[str] = []
    for resolved_group in suite_res.groups:
        group_cap_mod = 'ccpp_{}_{}_{}'.format(suite_name, resolved_group.group_name, 'cap')
        syms_list = [
            '{}_{}'.format(resolved_group.group_name, p)
            for p in _PHYSICS_PHASES
        ]
        syms_list.append('{}_state_alloc'.format(resolved_group.group_name))
        syms_list.append('{}_state_dealloc'.format(resolved_group.group_name))
        use_lines.append('{}use {}, only: {}'.format(
            _INDENT, group_cap_mod, ', '.join(syms_list)
        ))

    # Trace block writes to error_unit; ensure the USE is present.
    ensure_error_unit_use(use_lines, _INDENT)
    lines.extend(use_lines)

    lines.append('')
    lines.append('{}implicit none'.format(_INDENT))
    lines.append('{}private'.format(_INDENT))
    lines.append('')

    # Public declarations: all framework lifecycle and physics phase entry
    # points are always emitted.  ``ccpp_register`` is mandatory in the new
    # design — even an empty register phase fires the state transition.
    pub_subs = []
    pub_subs.append('{}_register'.format(suite_name))
    pub_subs.append('{}_init'.format(suite_name))
    for phase in _PHYSICS_PHASES:
        pub_subs.append('{}_physics_{}'.format(suite_name, phase))
    pub_subs.append('{}_final'.format(suite_name))
    pub_subs.append('{}_suite_state_alloc'.format(suite_name))
    pub_subs.append('{}_suite_state_dealloc'.format(suite_name))

    for sub in pub_subs:
        lines.append('{}public :: {}'.format(_INDENT, sub))

    lines.append('')
    lines.append('{}integer, private, parameter :: CCPP_SUITE_UNREGISTERED         = 0'.format(_INDENT))
    lines.append('{}integer, private, parameter :: CCPP_SUITE_REGISTERED           = 1'.format(_INDENT))
    lines.append('{}integer, private, parameter :: CCPP_SUITE_FRAMEWORK_INITIALIZED = 2'.format(_INDENT))
    lines.append('{}integer, private, allocatable :: ccpp_suite_state(:)'.format(_INDENT))
    lines.extend(emit_module_gate(trace, _INDENT))
    lines.append('')
    lines.append('contains')

    # Subroutines.  Order: register, init, physics_*, final, state_alloc/dealloc.
    lines.extend(_register_lines(suite_name, suite_res, host_dict))
    lines.extend(_init_lines(suite_name, suite_res, host_dict))
    for phase in _PHYSICS_PHASES:
        lines.extend(_physics_dispatch_lines(suite_name, phase, suite_res, host_dict))
    lines.extend(_final_lines(suite_name, suite_res, host_dict))

    has_suite_vars = bool(suite_res.suite_vars)
    lines.extend(_suite_state_alloc_lines(suite_name, has_suite_vars))
    lines.extend(_suite_state_dealloc_lines(suite_name, has_suite_vars))

    lines.append('')
    lines.append('end module {}'.format(mod_name))
    return lines


########################################################################
# Public API
########################################################################

def write_suite_cap(
    suite_name: str,
    suite_res: SuiteResolution,
    scheme_store: SchemeStore,
    output_root: str,
    host_dict=None,
    logger: Optional[logging.Logger] = None,
    trace: bool = False,
) -> str:
    """Write ``ccpp_<suite>_cap.F90`` to *output_root*.

    Parameters
    ----------
    suite_name : str
    suite_res : SuiteResolution
    scheme_store : SchemeStore
    output_root : str
        Output directory (created if absent).
    host_dict : dict, optional
        Flat host+control dictionary for multi-instance support.

    Returns
    -------
    str
        Absolute path of the written file.
    """
    os.makedirs(output_root, exist_ok=True)
    filename = 'ccpp_{}_cap.F90'.format(suite_name)
    out_path  = os.path.join(output_root, filename)

    lines = _generate_suite_cap(
        suite_name, suite_res, scheme_store, host_dict, trace=trace,
    )
    with open_if_changed(out_path, logger=logger) as fh:
        fh.write('\n'.join(lines) + '\n')
    return out_path
