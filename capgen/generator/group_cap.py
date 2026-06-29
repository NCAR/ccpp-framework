#!/usr/bin/env python3

"""Generate group-level cap Fortran source files.

A group cap module (``ccpp_<suite>_<group>_cap.F90``) contains one subroutine
per phase.  Each subroutine:

* USEs host-model modules for the variables it references.
* Declares control-variable dummy arguments.
* Declares transformation temporaries and optional pointer variables.
* Calls each scheme in the group in suite-XML order.
* Applies pre-call (forward) and post-call (backward) transformations.

Subcycle loops wrap scheme calls enclosed by ``<subcycle>`` elements in the
suite XML.

Init deduplication (Section 12) is handled by the ``initialized`` guard array
declared in this module:  an ``initialized(number_of_instances)`` integer array
is allocated at suite-init time and reset on suite-final.  Each group's init
subroutine calls scheme ``_init`` routines only when ``initialized(inst) == 0``
and then sets the element to 1.
"""

import logging
import os
from typing import Dict, List, Optional, Set, Tuple

from metadata.parse_tools import FORTRAN_CONDITIONAL_REGEX, open_if_changed
from metadata.variable_resolver import HostVarEntry
from generator.suite_types import _ptr_type_name_for_arg
from generator.suite_resolver import (
    ResolvedArg,
    ResolvedCall,
    ResolvedGroup,
    ResolvedSubcycle,
    _root_symbol,
    iter_phase_calls,
    iter_phase_subcycles,
)
from generator.trace import (
    emit_module_gate,
    emit_trace_block,
    ensure_error_unit_use,
)

_INDENT = '  '
_CONT   = ' &'

# Canonical phase order used for code-emission ordering in the group cap:
# subroutine layout, public declarations, scheme-import ``only:`` lists.
# Group caps do not contain a ``register`` phase (handled at suite-cap level);
# the order matches the user-facing ordering convention.
_GROUP_PHASE_ORDER = ('init', 'timestep_init', 'run', 'timestep_final', 'final')

_CTRL_STDNAMES_ORDER = (
    'suite_name',
    'group_name',
    'horizontal_loop_begin',
    'horizontal_loop_end',
    'thread_number',              # paired-optional (with number_of_threads); ordered here when declared
    'number_of_threads',          # paired-optional; ordered here when declared
    'number_of_physics_threads',
    'ccpp_error_code',
    'ccpp_error_message',
    'instance_number',
)


def _ctrl_entries_for_signature(host_dict, exclude=None):
    """Return all control HostVarEntry objects in canonical order.

    Parameters
    ----------
    host_dict : dict
        Flat host+control variable dictionary.
    exclude : set of str, optional
        Standard names to exclude (e.g. ``{'suite_name'}`` at suite cap level).
    """
    if host_dict is None:
        return []
    exclude_set = set(exclude or [])
    result = []
    seen: Set[str] = set()
    for std_name in _CTRL_STDNAMES_ORDER:
        if std_name in exclude_set:
            continue
        entry = host_dict.get(std_name)
        if entry is not None and entry.is_control:
            result.append(entry)
            seen.add(std_name)
    for std_name, entry in host_dict.items():
        if entry.is_control and std_name not in seen and std_name not in exclude_set:
            result.append(entry)
    return result


########################################################################
# Fortran type declaration helpers
########################################################################

def _fortran_type_str(type_: str, kind: str) -> str:
    """Return the Fortran type clause for a declaration.

    >>> _fortran_type_str('real', 'kind_phys')
    'real(kind=kind_phys)'
    >>> _fortran_type_str('real', '')
    'real'
    >>> _fortran_type_str('integer', '')
    'integer'
    >>> _fortran_type_str('character', 'len=512')
    'character(len=512)'
    >>> _fortran_type_str('logical', '')
    'logical'
    >>> _fortran_type_str('gfs_statein_type', '')
    'type(gfs_statein_type)'
    """
    t = type_.strip()
    # DDT types (not intrinsic, not external:...) need type(...) syntax.
    _INTRINSICS = frozenset({
        'real', 'integer', 'character', 'logical', 'complex', 'double precision'
    })
    if t.lower() not in _INTRINSICS and not t.lower().startswith('external:'):
        if not t.lower().startswith('type('):
            t = 'type({})'.format(t)
    if kind:
        if t.lower().startswith('character'):
            return 'character({})'.format(kind)
        return '{}(kind={})'.format(t, kind)
    return t


def _dim_decl(dimensions: List[str]) -> str:
    """Return the Fortran dimension attribute string for a declaration.

    Returns ``''`` for scalars, ``', dimension(d1, d2, ...)'`` for arrays.
    The dimensions here use the declared dimension standard names as-is.

    >>> _dim_decl([])
    ''
    >>> _dim_decl(['horizontal_dimension'])
    ', dimension(horizontal_dimension)'
    >>> _dim_decl(['horizontal_dimension', 'vertical_layer_dimension'])
    ', dimension(horizontal_dimension, vertical_layer_dimension)'
    """
    if not dimensions:
        return ''
    return ', dimension({})'.format(', '.join(dimensions))


def _dim_decl_local(dimensions: List[str], host_dict) -> str:
    """Return the Fortran dimension attribute using local names from host_dict.

    Each standard name in *dimensions* is resolved to the corresponding local
    Fortran variable name.  Falls back to the standard name when not found
    (should not happen with valid metadata).

    Special case for ``horizontal_dimension``: the temp must match the
    chunk slice the scheme actually receives at the call site
    (``host_var(lb:ub, …)`` via :func:`_one_dim_part`).
    Using the host's local name for ``horizontal_dimension`` (e.g. ``ncols``)
    would over-size the temp and break the unit-conversion assignment
    ``ps_l = factor * phys_state%ps(col_start:col_end)`` with a Fortran
    shape mismatch.  Emit ``dimension(<lb>:<ub>)`` instead, where ``<lb>``
    and ``<ub>`` are the host's local names for ``horizontal_loop_begin`` /
    ``horizontal_loop_end``.

    >>> _dim_decl_local([], None)
    ''
    """
    if not dimensions:
        return ''
    locals_ = []
    for std_name in dimensions:
        if std_name == 'horizontal_dimension':
            lb_entry = host_dict.get('horizontal_loop_begin') if host_dict else None
            ub_entry = host_dict.get('horizontal_loop_end')   if host_dict else None
            lb = lb_entry.local_name if lb_entry else 'horizontal_loop_begin'
            ub = ub_entry.local_name if ub_entry else 'horizontal_loop_end'
            locals_.append('{}:{}'.format(lb, ub))
        else:
            entry = host_dict.get(std_name) if host_dict else None
            locals_.append(entry.local_name if entry is not None else std_name)
    return ', dimension({})'.format(', '.join(locals_))


def _intent_clause(intent: str) -> str:
    """Return the Fortran intent attribute.

    >>> _intent_clause('in')
    ', intent(in)'
    >>> _intent_clause('out')
    ', intent(out)'
    >>> _intent_clause('inout')
    ', intent(inout)'
    """
    return ', intent({})'.format(intent)


# Control variables that are output-only (set by the routine, not read).
_CTRL_OUT_STDNAMES = frozenset({'ccpp_error_code', 'ccpp_error_message'})


def _ctrl_intent_for(standard_name: str) -> str:
    """Return ``'out'`` for error-reporting control vars, else ``'in'``.

    >>> _ctrl_intent_for('ccpp_error_code')
    'out'
    >>> _ctrl_intent_for('ccpp_error_message')
    'out'
    >>> _ctrl_intent_for('horizontal_loop_begin')
    'in'
    """
    return 'out' if standard_name in _CTRL_OUT_STDNAMES else 'in'


def _ctrl_local(host_dict, standard_name: str):
    """Return the local Fortran name for a control standard_name, or ``None``."""
    if not host_dict:
        return None
    entry = host_dict.get(standard_name)
    return entry.local_name if entry is not None else None


########################################################################
# USE-statement collection
########################################################################

def _active_std_names(active: str) -> Set[str]:
    """Return the set of identifier tokens from an active expression string.

    Uses the same tokeniser as ``_translate_active_expr`` so that only word
    tokens (potential standard names) are returned, not operators or literals.

    >>> sorted(_active_std_names('my_flag .eqv. .true.'))
    ['my_flag']
    >>> sorted(_active_std_names('a .or. b'))
    ['a', 'b']
    >>> _active_std_names('')
    set()
    """
    if not active:
        return set()
    result: Set[str] = set()
    for m in FORTRAN_CONDITIONAL_REGEX.finditer(active):
        tok = m.group(0)
        # Skip Fortran keywords / literals / operators captured by the regex.
        if tok.strip() and tok[0].isalpha() and '_' not in tok[:1]:
            # Only word-like tokens could be standard names; filter out
            # Fortran logical literals and operators (.true., .false., .not., ...)
            result.add(tok)
        elif tok[0] == '_' or (tok[0].isalpha() and tok.isidentifier()):
            result.add(tok)
    return result


def _collect_group_uses(
    resolved_group: ResolvedGroup,
    host_dict,
) -> Dict[str, Set[str]]:
    """Collect ``{module: {symbol, ...}}`` across all phases of a group.

    Includes direct argument symbols, dimension helper symbols, and variables
    referenced in ``active`` conditional expressions.

    Parameters
    ----------
    resolved_group : ResolvedGroup
    host_dict : dict
        Flat host+control variable dictionary (for dimension look-ups).

    Returns
    -------
    dict
    """
    uses: Dict[str, Set[str]] = {}

    def _add(mod: Optional[str], sym: str) -> None:
        if mod is not None:
            uses.setdefault(mod, set()).add(sym)

    for items in resolved_group.phase_calls.values():
        for resolved_call in iter_phase_calls(items):
            for arg in resolved_call.args:
                # Direct argument symbol.
                if arg.source != 'control':
                    _add(arg.module_name, arg.root_symbol)
                # Dimension helper symbols (non-control only).
                # USE the access-path root, not entry.local_name — DDT-component
                # entries have local_name = component (not a free module symbol).
                for dim_std in arg.used_dim_std_names:
                    entry = host_dict.get(dim_std)
                    if entry is not None and entry.module_name is not None:
                        _add(entry.module_name, _root_symbol(entry.access_path))
                # Variables referenced in active expressions (Gap 1).
                # Same access-path-root rule applies (DDT-component flags
                # reach the cap via the DDT instance, not the component).
                for std_name in _active_std_names(arg.active):
                    entry = host_dict.get(std_name) if host_dict else None
                    if entry is not None and entry.module_name is not None:
                        _add(entry.module_name, _root_symbol(entry.access_path))
                # Constituent extra symbols (index_of_X, etc.) live in the
                # suite cap module along with the constituent arrays.
                if arg.source == 'constituent' and arg.constituent_module_name:
                    for sym in arg.constituent_extra_symbols:
                        _add(arg.constituent_module_name, sym)

    # Also add dim_uses already collected during resolution.
    for mod, syms in resolved_group.dim_uses.items():
        uses.setdefault(mod, set()).update(syms)

    return uses


def _collect_kinds_used(resolved_group: ResolvedGroup) -> List[str]:
    """Collect kind parameter *names* referenced by transformation temporaries.

    Mirrors the kind-resolution logic in :func:`_generate_phase_subroutine`
    (the only place a group cap emits ``kind=<kind>`` directly).  Returned
    names are sorted alphabetically.  Excluded:

    * character ``len=...`` specifiers — not kind parameters.
    * bare integer literals (``kind = 8``, ``kind = 4``, ...) — valid
      Fortran kind specifiers but not module symbols, so they must not
      appear in ``use ccpp_kinds, only: ...``.  They flow through to the
      temp declaration (``real(kind=8)``) and to numeric-literal suffixes
      (``1.0_8``) unchanged; only the USE list needs to filter them out.
    """
    kinds: Set[str] = set()
    for items in resolved_group.phase_calls.values():
        for resolved_call in iter_phase_calls(items):
            for arg in resolved_call.args:
                if not arg.temp_name:
                    continue
                kind = arg.kind_scheme or (
                    arg.host_entry.kind if arg.host_entry else ''
                )
                if not kind:
                    continue
                if kind.startswith('len='):
                    continue
                if kind.isdigit():
                    continue
                kinds.add(kind)
    return sorted(kinds)


########################################################################
# Control variable dummy argument handling
########################################################################

def _collect_control_args(resolved_group: ResolvedGroup) -> List[ResolvedArg]:
    """Return deduplicated control-variable arguments for the group subroutine.

    Returns one ResolvedArg per unique standard_name, in a consistent order.
    """
    seen: Dict[str, ResolvedArg] = {}
    for items in resolved_group.phase_calls.values():
        for resolved_call in iter_phase_calls(items):
            for arg in resolved_call.args:
                if arg.source == 'control' and arg.standard_name not in seen:
                    seen[arg.standard_name] = arg
    return list(seen.values())


def _extra_dim_ctrl_entries(
    phase_items,
    phase: str,
    ctrl_args: List[ResolvedArg],
    host_dict,
) -> List[HostVarEntry]:
    """Return HostVarEntry objects for control vars needed but not in ctrl_args.

    Covers two cases (Gap 3):

    1. ``instance_number`` — needed for state-array indexing in init/final/
       timestep_init/timestep_final, and for suite-data access
       ``ccpp_suite_data(inst_num)%...`` in any phase that references suite vars.
    2. Any control var appearing only in dimension subscripts (``used_dim_std_names``).
    """
    if host_dict is None:
        return []
    already = {a.standard_name for a in ctrl_args if a.host_entry is not None}
    extras: Dict[str, HostVarEntry] = {}

    has_suite_vars = any(
        arg.source == 'suite'
        for resolved_call in iter_phase_calls(phase_items)
        for arg in resolved_call.args
    )
    needs_inst = (
        phase in ('init', 'final', 'timestep_init', 'timestep_final')
        or has_suite_vars
    )
    if needs_inst:
        inst_entry = host_dict.get('instance_number')
        if inst_entry is not None and 'instance_number' not in already:
            extras['instance_number'] = inst_entry

    for resolved_call in iter_phase_calls(phase_items):
        for arg in resolved_call.args:
            for dim_std in arg.used_dim_std_names:
                if dim_std in already or dim_std in extras:
                    continue
                entry = host_dict.get(dim_std)
                if entry is not None and entry.is_control:
                    extras[dim_std] = entry

    # Subcycle loop bounds resolved from a CCPP standard name in the suite
    # XML need the same dummy-arg threading when the resolved entry is a
    # control variable (host-module entries are USE'd instead and are
    # handled by ``_collect_dim_uses``).  Walk all nesting levels —
    # each level's bound is independently a candidate dummy arg.
    for item in iter_phase_subcycles(phase_items):
        std = item.loop_std_name
        if not std or std in already or std in extras:
            continue
        entry = host_dict.get(std)
        if entry is not None and entry.is_control:
            extras[std] = entry

    return list(extras.values())


########################################################################
# Fortran source line helpers
########################################################################

def _use_statements(uses: Dict[str, Set[str]]) -> List[str]:
    """Generate sorted USE statements.

    >>> lines = _use_statements({'mod_a': {'sym1', 'sym2'}, 'mod_b': {'sym3'}})
    >>> lines[0]
    '  use mod_a, only: sym1, sym2'
    >>> lines[1]
    '  use mod_b, only: sym3'
    """
    result = []
    for mod in sorted(uses):
        syms = ', '.join(sorted(uses[mod]))
        result.append('{}use {}, only: {}'.format(_INDENT, mod, syms))
    return result


def _collect_scheme_uses(resolved_group: ResolvedGroup) -> List[Tuple[str, str, List[str]]]:
    """Return ``[(scheme_name, module_name, [phase_routine, ...]), ...]``.

    Schemes are listed in first-seen order across phases (in canonical phase
    iteration order); within each scheme the phase routines are listed in
    canonical phase order.  Each phase routine is the Fortran subroutine name
    ``<scheme_name>_<phase>``.

    ``module_name`` is the Fortran module that exports those subroutines —
    typically equal to the scheme name, but overridden by an explicit
    ``module_name`` attribute in the scheme's ``[ccpp-table-properties]``.
    """
    seen_schemes: Dict[str, Set[str]] = {}
    scheme_modules: Dict[str, str] = {}
    order: List[str] = []
    for phase in _GROUP_PHASE_ORDER:
        for resolved_call in iter_phase_calls(resolved_group.phase_calls.get(phase, [])):
            if resolved_call.scheme_name not in seen_schemes:
                seen_schemes[resolved_call.scheme_name] = set()
                order.append(resolved_call.scheme_name)
            seen_schemes[resolved_call.scheme_name].add(phase)
            # resolved_call.scheme_module is empty for old/legacy ResolvedCall
            # objects built in tests; fall back to the scheme name so emission
            # still works.
            scheme_modules[resolved_call.scheme_name] = (
                resolved_call.scheme_module or resolved_call.scheme_name
            )
    result: List[Tuple[str, str, List[str]]] = []
    for sname in order:
        phases_present = [p for p in _GROUP_PHASE_ORDER if p in seen_schemes[sname]]
        syms = ['{}_{}'.format(sname, p) for p in phases_present]
        result.append((sname, scheme_modules[sname], syms))
    return result


def _scheme_use_statements(resolved_group: ResolvedGroup) -> List[str]:
    """Generate ``use <module>, only: <scheme>_<phase>, ...`` lines.

    Schemes are emitted in first-seen order; phase routines within each
    ``only:`` clause follow the canonical phase order.  When the scheme's
    ``[ccpp-table-properties]`` declares a ``module_name`` distinct from
    the scheme name, the USE statement targets that module.
    """
    return [
        '{}use {}, only: {}'.format(_INDENT, mod, ', '.join(syms))
        for _sname, mod, syms in _collect_scheme_uses(resolved_group)
        if syms
    ]


########################################################################
# Pre/post-call transformation code
########################################################################

def _transform_comment(arg: ResolvedArg, reverse: bool = False) -> str:
    """Compose the trailing inline comment for a transform assignment.

    Lists every active transform (unit conversion, kind change, vertical
    flip) so a reader can tell at a glance what the generated copy does.

    Identity unit conversions (registered for dimensionally-equivalent
    spellings such as ``J kg-1`` ↔ ``m2 s-2``, where the formula is just
    ``{var}``) are suppressed: the assignment carries no scaling factor
    and labelling it as a "unit conversion" is misleading.  Detection is
    done by comparing the rendered transform expression against the raw
    operand — equality means the formula returned the variable unchanged.
    """
    bits: List[str] = []
    if arg.needs_unit_transform or arg.needs_kind_transform:
        # Suppress identity conversions (formula '{var}' for equivalent
        # units; kinds also matching).
        if reverse:
            is_identity = (arg.unit_backward == arg.temp_name)
        else:
            is_identity = (arg.unit_forward == arg.call_expr)
        if not is_identity:
            if reverse:
                bits.append('unit conversion: {} to {}'.format(
                    arg.kind_scheme or '', arg.kind_host or '',
                ))
            else:
                bits.append('unit conversion: {} to {}'.format(
                    arg.kind_host or '', arg.kind_scheme or '',
                ))
    if arg.needs_vert_flip:
        bits.append('vertical flip (top_at_one mismatch)')
    if not bits:
        return ''
    return '! ' + '; '.join(bits)


def _active_required_guard_lines(
    arg: ResolvedArg,
    scheme_name: str,
    phase: str,
    errflg_local: Optional[str],
    errmsg_local: Optional[str],
    indent: str,
) -> List[str]:
    """Runtime guard for a non-optional scheme arg whose host declares ``active``.

    The host says the variable is only valid when ``active`` is true.
    The scheme demands the variable unconditionally.  We emit a runtime
    check at the call site so a violation surfaces as a clean errflg/errmsg
    rather than as a silent read of unallocated/stale memory.

    Emitted before any transform pre-emission and before the call itself.
    If the host did not declare both ``ccpp_error_code`` and
    ``ccpp_error_message`` (extremely unusual), the guard is omitted —
    there is no way to report the violation.
    """
    if not arg.active_local or arg.is_optional:
        return []
    if not errflg_local or not errmsg_local:
        return []
    msg = (
        "scheme '{scheme}' phase '{phase}' requires variable "
        "'{std}' but host active condition ({active}) is false".format(
            scheme=scheme_name, phase=phase,
            std=arg.standard_name, active=arg.active,
        )
    )
    return [
        '{}if (.not. ({})) then'.format(indent, arg.active_local),
        "{}  {} = \"{}\"".format(indent, errmsg_local, msg),
        '{}  {} = 1'.format(indent, errflg_local),
        '{}  return'.format(indent),
        '{}end if'.format(indent),
    ]


def _pre_call_lines(arg: ResolvedArg) -> List[str]:
    """Generate pre-call Fortran lines for one argument."""
    lines = []
    if arg.transform_case == 1:
        return lines

    indent = _INDENT * 2

    if arg.transform_case == 2:
        # Optional, no transform: pointer assignment.
        lines.append('{}if ({}) then'.format(indent, arg.active_local or '.true.'))
        lines.append('{}  {}%ptr => {}'.format(indent, arg.ptr_name, arg.call_expr))
        lines.append('{}else'.format(indent))
        lines.append('{}  nullify({}%ptr)'.format(indent, arg.ptr_name))
        lines.append('{}end if'.format(indent))

    elif arg.transform_case == 3:
        # Transform, not optional.
        if arg.unit_forward:
            comment = _transform_comment(arg)
            sep = '  ' if comment else ''
            lines.append('{}{} = {}{}{}'.format(
                indent, arg.temp_name, arg.unit_forward, sep, comment
            ))

    elif arg.transform_case == 4:
        # Transform + optional pointer.
        lines.append('{}if ({}) then'.format(indent, arg.active_local or '.true.'))
        if arg.unit_forward:
            lines.append('{}  {} = {}'.format(indent, arg.temp_name, arg.unit_forward))
        lines.append('{}  {}%ptr => {}'.format(indent, arg.ptr_name, arg.temp_name))
        lines.append('{}else'.format(indent))
        lines.append('{}  nullify({}%ptr)'.format(indent, arg.ptr_name))
        lines.append('{}end if'.format(indent))

    return lines


def _post_call_lines(arg: ResolvedArg) -> List[str]:
    """Generate post-call Fortran lines for one argument."""
    lines = []
    if arg.transform_case == 1:
        return lines

    indent = _INDENT * 2

    if arg.transform_case == 2:
        lines.append('{}nullify({}%ptr)'.format(indent, arg.ptr_name))

    elif arg.transform_case == 3:
        if arg.unit_backward:
            comment = _transform_comment(arg, reverse=True)
            sep = '  ' if comment else ''
            lines.append('{}{} = {}{}{}'.format(
                indent, arg.call_expr, arg.unit_backward, sep, comment
            ))

    elif arg.transform_case == 4:
        lines.append('{}if ({}) then'.format(indent, arg.active_local or '.true.'))
        lines.append('{}  nullify({}%ptr)'.format(indent, arg.ptr_name))
        if arg.unit_backward:
            lines.append('{}  {} = {}'.format(indent, arg.call_expr, arg.unit_backward))
        lines.append('{}end if'.format(indent))

    return lines


def _call_arg_expr(arg: ResolvedArg) -> str:
    """Return the Fortran expression to pass for this argument at the call site."""
    if arg.transform_case == 1:
        return arg.call_expr
    elif arg.transform_case == 2:
        return '{}%ptr'.format(arg.ptr_name)
    elif arg.transform_case == 3:
        return arg.temp_name
    else:  # 4
        return '{}%ptr'.format(arg.ptr_name)


########################################################################
# Scheme-call code generation helper
########################################################################

def _max_subcycle_depth(items) -> int:
    """Return the maximum subcycle nesting depth in *items*.

    A flat list of scheme calls has depth 0; a single ``<subcycle>``
    wrapping schemes has depth 1; a subcycle wrapping a subcycle has
    depth 2; and so on.  Used to pre-declare one integer loop counter
    per nesting level (``ccpp_loop_counter``, ``ccpp_loop_counter_2``,
    ``ccpp_loop_counter_3``, ...).
    """
    depth = 0
    for item in items:
        if isinstance(item, ResolvedSubcycle):
            depth = max(depth, 1 + _max_subcycle_depth(item.calls))
    return depth


def _loop_counter_name(depth: int) -> str:
    """Return the loop-counter Fortran identifier for *depth* (1-based).

    Depth 1 (outermost / single level) returns ``'ccpp_loop_counter'``
    so existing single-subcycle tests and host expectations are
    unchanged.  Deeper levels get ``ccpp_loop_counter_<depth>``.
    """
    if depth <= 1:
        return 'ccpp_loop_counter'
    return 'ccpp_loop_counter_{}'.format(depth)


def _emit_phase_items(
    items, indent: str, lines: List[str], depth: int,
    phase: str = '',
    errflg_local: Optional[str] = None,
    errmsg_local: Optional[str] = None,
) -> None:
    """Recursively emit Fortran for a list of :data:`PhaseItem` objects.

    A :class:`ResolvedCall` becomes a single scheme call (with pre/post
    transforms).  A :class:`ResolvedSubcycle` becomes a ``do`` loop
    that wraps recursively-emitted children, with one fresh integer
    loop variable per nesting level.
    """
    for item in items:
        if isinstance(item, ResolvedCall):
            _emit_one_call(
                item, indent, lines, phase, errflg_local, errmsg_local,
            )
        elif isinstance(item, ResolvedSubcycle):
            counter = _loop_counter_name(depth)
            lines.append(
                '{}do {} = 1, {}'.format(indent, counter, item.loop)
            )
            _emit_phase_items(
                item.calls, indent + _INDENT, lines, depth=depth + 1,
                phase=phase,
                errflg_local=errflg_local,
                errmsg_local=errmsg_local,
            )
            lines.append('{}end do'.format(indent))
            lines.append('')


def _emit_one_call(
    resolved_call: ResolvedCall,
    indent: str,
    lines: List[str],
    phase: str = '',
    errflg_local: Optional[str] = None,
    errmsg_local: Optional[str] = None,
) -> None:
    """Append Fortran lines for a single scheme call (with transforms + errcheck)."""
    # Pre-call: runtime guard for any non-optional arg whose host declares
    # ``active = (...)``.  Emitted before transforms so an inactive-but-required
    # var bails out with a clear error rather than reading host memory through
    # the transform pipeline.
    for arg in resolved_call.args:
        lines.extend(_active_required_guard_lines(
            arg, resolved_call.scheme_name, resolved_call.phase,
            errflg_local, errmsg_local, indent,
        ))

    # Pre-call transformations.
    for arg in resolved_call.args:
        lines.extend(_pre_call_lines(arg))

    call_args_exprs = [
        '{}={}'.format(a.scheme_local_name, _call_arg_expr(a))
        for a in resolved_call.args
    ]
    call_name = '{}_{}'.format(resolved_call.scheme_name, resolved_call.phase)

    if call_args_exprs:
        lines.append('{}call {}( &'.format(indent, call_name))
        for i, expr in enumerate(call_args_exprs):
            sep = ', &' if i < len(call_args_exprs) - 1 else ')'
            lines.append('{}    {}{}'.format(indent, expr, sep))
    else:
        lines.append('{}call {}()'.format(indent, call_name))

    errflg_arg = next(
        (a for a in resolved_call.args if a.standard_name == 'ccpp_error_code'), None
    )
    if errflg_arg is not None:
        lines.append('{}if ({} /= 0) return'.format(indent, _call_arg_expr(errflg_arg)))

    for arg in resolved_call.args:
        lines.extend(_post_call_lines(arg))
    lines.append('')


########################################################################
# State machine guards
########################################################################

def _state_entry_guard(
    phase: str,
    inst_idx: str,
    errflg_local: Optional[str],
    errmsg_local: Optional[str],
    sub_label: str,
    indent: str,
) -> List[str]:
    """Return Fortran lines that validate the group state on phase entry.

    Per the design table:

    ===================== ============================================
    Phase                 Required state
    ===================== ============================================
    ``init``              ``UNINITIALIZED`` (idempotent skip if ``INITIALIZED``)
    ``timestep_init``     ``== INITIALIZED``
    ``run``               ``== IN_TIMESTEP``
    ``timestep_final``    ``== IN_TIMESTEP``
    ``final``             ``>= INITIALIZED`` (idempotent skip if ``UNINITIALIZED``)
    ===================== ============================================

    Invalid state sets ``errflg = 1``, populates ``errmsg``, and returns.

    If the host did not declare ``ccpp_error_code`` / ``ccpp_error_message``
    (extremely unusual), the guard is omitted — there is no way to report the
    error and the routine cannot proceed without somewhere to write it.
    """
    if not errflg_local or not errmsg_local:
        return []

    state_var = 'ccpp_group_state({})'.format(inst_idx)
    msg = "ccpp_{}: invalid group state".format(sub_label)

    def _err_block(condition: str) -> List[str]:
        return [
            '{}if ({}) then'.format(indent, condition),
            "{}  {} = '{}'".format(indent, errmsg_local, msg),
            '{}  {} = 1'.format(indent, errflg_local),
            '{}  return'.format(indent),
            '{}end if'.format(indent),
            '',
        ]

    if phase == 'init':
        # Idempotent skip when already INITIALIZED; error if past INITIALIZED
        # (e.g. IN_TIMESTEP) — init must come from UNINITIALIZED.
        lines: List[str] = [
            '{}if ({} == CCPP_GROUP_INITIALIZED) return'.format(indent, state_var),
            '',
        ]
        lines.extend(_err_block(
            '{} /= CCPP_GROUP_UNINITIALIZED'.format(state_var)
        ))
        return lines
    if phase == 'timestep_init':
        return _err_block('{} /= CCPP_GROUP_INITIALIZED'.format(state_var))
    if phase == 'run':
        return _err_block('{} /= CCPP_GROUP_IN_TIMESTEP'.format(state_var))
    if phase == 'timestep_final':
        return _err_block('{} /= CCPP_GROUP_IN_TIMESTEP'.format(state_var))
    if phase == 'final':
        # Idempotent skip when already UNINITIALIZED; INITIALIZED and
        # IN_TIMESTEP are both valid entry states (UNINITIALIZED is the only
        # state value < INITIALIZED, so no error block is reachable here).
        return [
            '{}if ({} == CCPP_GROUP_UNINITIALIZED) return'.format(indent, state_var),
            '',
        ]
    return []


########################################################################
# Subroutine generator
########################################################################

def _generate_phase_subroutine(
    suite_name: str,
    group_name: str,
    phase: str,
    phase_items,
    ctrl_entries,
    host_dict,
) -> List[str]:
    """Generate one phase subroutine for a group cap.

    For the ``init`` phase the body is wrapped in a state guard
    (``ccpp_group_state(1) < CCPP_GROUP_INITIALIZED``) and the state is set
    to ``CCPP_GROUP_INITIALIZED`` at the end.

    For the ``final`` phase the state is reset to ``CCPP_GROUP_UNINITIALIZED``
    at the end.

    ``ResolvedSubcycle`` items in *phase_items* generate ``do`` loops (run
    phase only).

    Returns a list of Fortran source lines (no trailing newlines).
    """
    # Short Fortran symbol; module name already namespaces ``<group>_<phase>``
    # as ``ccpp_<suite>_<group>_cap_mp_<group>_<phase>`` at link time, keeping
    # the mangled global name under Intel's ~90-char threshold.  The long
    # form ``<suite>_<group>_<phase>`` is kept in ``sub_label`` below for
    # trace strings and error messages (string literals have no length cap).
    sub_name = '{}_{}'.format(group_name, phase)
    lines: List[str] = []

    # ---- subroutine declaration ------------------------------------------
    ctrl_local_names = [e.local_name for e in ctrl_entries]

    if ctrl_local_names:
        lines.append('{}subroutine {}( &'.format(_INDENT, sub_name))
        for i, lname in enumerate(ctrl_local_names):
            sep = ', &' if i < len(ctrl_local_names) - 1 else ')'
            lines.append('{}    {}{}'.format(_INDENT, lname, sep))
    else:
        lines.append('{}subroutine {}()'.format(_INDENT, sub_name))

    # ---- dummy argument declarations ------------------------------------
    if ctrl_local_names:
        lines.append('')
        for entry in ctrl_entries:
            # Character control dummies always use len=* so the host's specific
            # length doesn't propagate into the generated signature.
            kind = 'len=*' if entry.type.strip().lower() == 'character' else entry.kind
            t      = _fortran_type_str(entry.type, kind)
            intent = _intent_clause(_ctrl_intent_for(entry.standard_name))
            dim    = _dim_decl(entry.dimensions)
            lines.append(
                '{}{}{}{}  :: {}'.format(_INDENT * 2, t, intent, dim, entry.local_name)
            )

    # ---- local variable declarations (transformation temps, subcycle counter)
    local_decls: List[str] = []
    # Each subcycle nesting level needs its own integer loop variable.
    # The outermost (and only one, in the single-level case) is named
    # ``ccpp_loop_counter`` to preserve the existing single-level
    # convention; deeper levels are ``ccpp_loop_counter_2``,
    # ``ccpp_loop_counter_3``, ... so nested loops have distinct vars.
    max_depth = _max_subcycle_depth(phase_items)
    for d in range(1, max_depth + 1):
        name = 'ccpp_loop_counter' if d == 1 else 'ccpp_loop_counter_{}'.format(d)
        local_decls.append('{}integer :: {}'.format(_INDENT * 2, name))

    seen_temp_names: Set[str] = set()
    seen_ptr_names:  Set[str] = set()
    for resolved_call in iter_phase_calls(phase_items):
        for arg in resolved_call.args:
            if arg.temp_name and arg.temp_name not in seen_temp_names:
                seen_temp_names.add(arg.temp_name)
                t   = _fortran_type_str(
                    arg.host_entry.type if arg.host_entry else arg.suite_var.type_,
                    arg.kind_scheme or (arg.host_entry.kind if arg.host_entry else ''),
                )
                # Use scheme dimensions (local names) for the temp declaration
                # so the temp is sized for the chunk the scheme actually receives
                # (Gap 2: avoids emitting standard names in the declaration).
                dim = _dim_decl_local(arg.scheme_dimensions, host_dict)
                # Transform-case 4 emits ``<ptr>%ptr => <temp>``, so the temp
                # must have the TARGET attribute or pointer assignment is illegal.
                target_attr = ', target' if arg.ptr_name else ''
                local_decls.append(
                    '{}{}{}{}  :: {}'.format(
                        _INDENT * 2, t, dim, target_attr, arg.temp_name
                    )
                )
            if arg.ptr_name and arg.ptr_name not in seen_ptr_names:
                seen_ptr_names.add(arg.ptr_name)
                ptr_tname = _ptr_type_name_for_arg(arg, resolved_call.scheme_name)
                local_decls.append(
                    '{}type({}) :: {}'.format(_INDENT * 2, ptr_tname, arg.ptr_name)
                )

    if local_decls:
        lines.append('')
        lines.extend(local_decls)

    lines.append('')

    call_indent = _INDENT * 2

    inst_idx = _instance_idx(host_dict)
    errflg_local = _ctrl_local(host_dict, 'ccpp_error_code')
    errmsg_local = _ctrl_local(host_dict, 'ccpp_error_message')
    # Long form used as the trace-message label and in runtime error
    # strings so grep against logs still finds the suite + group context.
    sub_label = '{}_{}_{}'.format(suite_name, group_name, phase)

    # ---- trace block (always emitted; gated by the module ``trace``
    # parameter so the I/O is dead-code-eliminated when trace=.false.).
    # Placed before errmsg/errflg init so the write references no
    # intent(out) dummy and fires even when a state guard then bails.
    trace_lines = emit_trace_block(sub_label, ctrl_entries, call_indent)
    if trace_lines:
        lines.extend(trace_lines)
        lines.append('')

    # ---- initialize error reporting vars -------------------------------
    if errflg_local and errmsg_local:
        lines.append("{}{} = ''".format(call_indent, errmsg_local))
        lines.append('{}{} = 0'.format(call_indent, errflg_local))
        lines.append('')

    # ---- phase entry state guards --------------------------------------
    lines.extend(
        _state_entry_guard(
            phase, inst_idx, errflg_local, errmsg_local, sub_label, call_indent
        )
    )

    # ---- scheme calls ---------------------------------------------------
    _emit_phase_items(
        phase_items, call_indent, lines, depth=1,
        phase=phase,
        errflg_local=errflg_local,
        errmsg_local=errmsg_local,
    )

    # ---- post-call state transitions ------------------------------------
    if phase == 'init':
        lines.append(
            '{}ccpp_group_state({}) = CCPP_GROUP_INITIALIZED'.format(
                call_indent, inst_idx
            )
        )
        lines.append('')
    elif phase == 'timestep_init':
        lines.append(
            '{}ccpp_group_state({}) = CCPP_GROUP_IN_TIMESTEP'.format(
                call_indent, inst_idx
            )
        )
        lines.append('')
    elif phase == 'timestep_final':
        lines.append(
            '{}ccpp_group_state({}) = CCPP_GROUP_INITIALIZED'.format(
                call_indent, inst_idx
            )
        )
        lines.append('')
    elif phase == 'final':
        lines.append(
            '{}ccpp_group_state({}) = CCPP_GROUP_UNINITIALIZED'.format(
                call_indent, inst_idx
            )
        )
        lines.append('')

    lines.append('{}end subroutine {}'.format(_INDENT, sub_name))
    return lines


########################################################################
# Group cap module generator
########################################################################

def _instance_idx(host_dict) -> str:
    """Return the Fortran index expression for the current model instance.

    Returns the local name of the ``instance_number`` control variable when the
    host declares it; otherwise returns the literal ``'1'`` for single-instance
    models.

    >>> class _FakeEntry:
    ...     local_name = 'inst_num'
    >>> class _FakeDict(dict):
    ...     pass
    >>> d = _FakeDict({'instance_number': _FakeEntry()})
    >>> _instance_idx(d)
    'inst_num'
    >>> _instance_idx({})
    '1'
    """
    entry = host_dict.get('instance_number') if host_dict else None
    return entry.local_name if entry is not None else '1'


def _instance_local(host_dict) -> Optional[str]:
    """Return the host's local name for ``instance_number``, or ``None``.

    Companion to :func:`_instance_idx`.  Callers use the ``None`` return
    to decide whether to inject ``instance_number`` into a subroutine's
    signature (i.e. whether the host opted into the multi-instance API).

    >>> class _FakeEntry:
    ...     local_name = 'inst_num'
    >>> _instance_local({'instance_number': _FakeEntry()})
    'inst_num'
    >>> _instance_local({}) is None
    True
    """
    entry = host_dict.get('instance_number') if host_dict else None
    return entry.local_name if entry is not None else None


def _generate_state_alloc(suite_name: str, group_name: str) -> List[str]:
    """Generate the ``ccpp_<suite>_<group>_state_alloc`` subroutine.

    The subroutine always accepts ``number_of_instances`` as an explicit
    ``intent(in)`` integer argument so the caller (the suite cap's init
    routine) can supply the count at runtime without the group cap needing to
    USE any host module.

    Idempotent: a second call after the array is already allocated is a
    no-op.  The suite cap calls this once per ``<suite>_init`` invocation,
    so when multiple instances initialize, only the first allocates and
    initialises the state array; subsequent calls return immediately to
    avoid clobbering peer-instance state slots.  Matches the
    ``<suite>_suite_state_alloc`` pattern.
    """
    # Short Fortran symbol; the module ``ccpp_<suite>_<group>_cap``
    # already namespaces this routine at link time.
    sub_name = '{}_state_alloc'.format(group_name)
    i1 = _INDENT
    i2 = _INDENT * 2
    lines = [
        '',
        '{}subroutine {}(number_of_instances, errmsg, errflg)'.format(i1, sub_name),
        '',
        '{}integer, intent(in) :: number_of_instances'.format(i2),
        '{}character(len=*), intent(out) :: errmsg'.format(i2),
        '{}integer, intent(out) :: errflg'.format(i2),
        '',
        "{}errmsg = ''".format(i2),
        '{}errflg = 0'.format(i2),
        '{}if (allocated(ccpp_group_state)) return'.format(i2),
        '{}allocate(ccpp_group_state(number_of_instances))'.format(i2),
        '{}ccpp_group_state(:) = CCPP_GROUP_UNINITIALIZED'.format(i2),
        '',
        '{}end subroutine {}'.format(i1, sub_name),
    ]
    return lines


def _generate_state_dealloc(suite_name: str, group_name: str) -> List[str]:
    """Generate the ``<group>_state_dealloc`` subroutine (Fortran symbol).

    The module name ``ccpp_<suite>_<group>_cap`` already namespaces this
    routine, so the short Fortran name keeps the mangled global symbol
    under Intel's ~90-char limit.
    """
    sub_name = '{}_state_dealloc'.format(group_name)
    i1 = _INDENT
    i2 = _INDENT * 2
    return [
        '',
        '{}subroutine {}(errmsg, errflg)'.format(i1, sub_name),
        '',
        '{}character(len=*), intent(out) :: errmsg'.format(i2),
        '{}integer, intent(out) :: errflg'.format(i2),
        '',
        "{}errmsg = ''".format(i2),
        '{}errflg = 0'.format(i2),
        '{}if (allocated(ccpp_group_state)) deallocate(ccpp_group_state)'.format(i2),
        '',
        '{}end subroutine {}'.format(i1, sub_name),
    ]


def _generate_group_cap(
    suite_name: str,
    group_name: str,
    resolved_group: ResolvedGroup,
    host_dict,
    trace: bool = False,
) -> List[str]:
    """Generate the full group cap module source lines.

    Parameters
    ----------
    suite_name : str
    group_name : str
    resolved_group : ResolvedGroup
    host_dict : dict
        Flat host+control dictionary.

    Returns
    -------
    list of str (without trailing newlines)
    """
    mod_name = 'ccpp_{}_{}_{}'.format(suite_name, group_name, 'cap')
    # Short Fortran symbols for the state-management subroutines; the
    # module name carries ``ccpp_<suite>_<group>_`` and keeps the mangled
    # global symbol (``<mod>_mp_<sub>``) under Intel's ~90-char limit.
    alloc_sub   = '{}_state_alloc'.format(group_name)
    dealloc_sub = '{}_state_dealloc'.format(group_name)
    lines: List[str] = []

    # ---- module header --------------------------------------------------
    lines.append(
        '! ccpp_{}_{}_cap.F90 -- generated by ccpp_capgen, do not edit'.format(
            suite_name, group_name
        )
    )
    lines.append('module {}'.format(mod_name))
    lines.append('')

    # ---- USE statements -------------------------------------------------
    uses = _collect_group_uses(resolved_group, host_dict)

    # Add USE for types module when optional pointer args are present.
    # Build the wrapper name via the per-arg helper so any
    # unsupported shape (e.g. character(len=*)) raises a CCPPError
    # naming the offending scheme + argument.
    ptr_type_names: Set[str] = set()
    for items in resolved_group.phase_calls.values():
        for resolved_call in iter_phase_calls(items):
            for arg in resolved_call.args:
                if arg.ptr_name:
                    ptr_type_names.add(
                        _ptr_type_name_for_arg(arg, resolved_call.scheme_name)
                    )
    if ptr_type_names:
        types_mod = 'ccpp_{}_types'.format(suite_name)
        uses[types_mod] = ptr_type_names

    # USE ccpp_kinds for any kind parameter referenced in transformation
    # temporaries declared in this group (e.g. ``real(kind=kind_phys)``).
    kind_names = _collect_kinds_used(resolved_group)
    if kind_names:
        uses['ccpp_kinds'] = set(kind_names)

    use_lines = _use_statements(uses)
    use_lines.extend(_scheme_use_statements(resolved_group))
    # Trace block writes to error_unit; ensure the USE is present.
    ensure_error_unit_use(use_lines, _INDENT)
    lines.extend(use_lines)
    if use_lines:
        lines.append('')

    lines.append('{}implicit none'.format(_INDENT))
    lines.append('{}private'.format(_INDENT))

    # Public physics subroutines, in canonical phase order.
    # Always emit all phases so the suite cap can rely on the state machine
    # transitioning through every phase, even when a group has no scheme
    # routine for a particular phase.
    for phase in _GROUP_PHASE_ORDER:
        sub_name = '{}_{}'.format(group_name, phase)
        lines.append('{}public :: {}'.format(_INDENT, sub_name))

    # Public state management subroutines.
    lines.append('{}public :: {}'.format(_INDENT, alloc_sub))
    lines.append('{}public :: {}'.format(_INDENT, dealloc_sub))

    # ---- state machine module-level declarations -------------------------
    lines.append('')
    lines.append('{}integer, private, parameter :: CCPP_GROUP_UNINITIALIZED = 0'.format(_INDENT))
    lines.append('{}integer, private, parameter :: CCPP_GROUP_INITIALIZED   = 1'.format(_INDENT))
    lines.append('{}integer, private, parameter :: CCPP_GROUP_IN_TIMESTEP   = 2'.format(_INDENT))
    lines.append('{}integer, private, allocatable :: ccpp_group_state(:)'.format(_INDENT))

    # ---- trace gate -------------------------------------------------------
    # Module-level compile-time toggle; flip to .true. (or pass --trace at
    # generation time) to enable the per-subroutine trace writes.  When
    # .false., the gated writes are dead-code-eliminated by the compiler
    # but the control dummies remain syntactically referenced, which
    # silences strict unused-dummy warnings (Intel oneAPI in particular).
    lines.extend(emit_module_gate(trace, _INDENT))

    lines.append('')
    lines.append('contains')
    lines.append('')

    # ---- subroutines per phase ------------------------------------------
    # All phases share the same uniform control arg signature (excluding
    # suite_name and group_name which are consumed at higher dispatch levels).
    ctrl_sig_entries = _ctrl_entries_for_signature(
        host_dict, exclude={'suite_name', 'group_name'}
    )
    for phase in _GROUP_PHASE_ORDER:
        phase_items = resolved_group.phase_calls.get(phase, [])
        sub_lines = _generate_phase_subroutine(
            suite_name, group_name, phase, phase_items, ctrl_sig_entries, host_dict
        )
        lines.extend(sub_lines)
        lines.append('')

    # ---- state management subroutines -----------------------------------
    lines.extend(_generate_state_alloc(suite_name, group_name))
    lines.append('')
    lines.extend(_generate_state_dealloc(suite_name, group_name))
    lines.append('')

    lines.append('end module {}'.format(mod_name))
    return lines


def _ctrl_args_for_phase(resolved_group: ResolvedGroup, phase: str) -> List[ResolvedArg]:
    """Return control args used in a specific phase, deduplicated."""
    seen: Dict[str, ResolvedArg] = {}
    for resolved_call in iter_phase_calls(resolved_group.phase_calls.get(phase, [])):
        for arg in resolved_call.args:
            if arg.source == 'control' and arg.standard_name not in seen:
                seen[arg.standard_name] = arg
    return list(seen.values())


########################################################################
# Public API
########################################################################

def write_group_cap(
    suite_name: str,
    group_name: str,
    resolved_group: ResolvedGroup,
    host_dict,
    output_root: str,
    logger: Optional[logging.Logger] = None,
    trace: bool = False,
) -> str:
    """Write the group cap Fortran module to *output_root*.

    Parameters
    ----------
    suite_name : str
    group_name : str
    resolved_group : ResolvedGroup
        Resolved call information for this group.
    host_dict : dict
        Flat host+control variable dictionary.
    output_root : str
        Output directory (created if absent).

    Returns
    -------
    str
        Absolute path of the written file.
    """
    os.makedirs(output_root, exist_ok=True)
    filename = 'ccpp_{}_{}_cap.F90'.format(suite_name, group_name)
    out_path = os.path.join(output_root, filename)

    lines = _generate_group_cap(
        suite_name, group_name, resolved_group, host_dict, trace=trace,
    )
    with open_if_changed(out_path, logger=logger) as fh:
        fh.write('\n'.join(lines) + '\n')
    return out_path
