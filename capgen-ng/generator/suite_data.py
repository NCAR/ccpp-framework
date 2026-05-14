#!/usr/bin/env python3

"""Generate the suite data module ``ccpp_<suite>_data.F90``."""

import logging
import os
from typing import Dict, List, Optional

from metadata.parse_tools import CCPPError, open_if_changed
from generator.suite_resolver import SuiteVar

_INDENT = '  '

_INTRINSICS = frozenset({
    'real', 'integer', 'character', 'logical', 'complex', 'double precision'
})


def _type_str(type_: str, kind: str) -> str:
    """Return the Fortran type clause for a SuiteVar field.

    >>> _type_str('real', 'kind_phys')
    'real(kind=kind_phys)'
    >>> _type_str('real', '')
    'real'
    >>> _type_str('my_ddt', '')
    'type(my_ddt)'
    >>> _type_str('character', 'len=512')
    'character(len=512)'
    """
    t = type_.strip()
    if t.lower() not in _INTRINSICS and not t.lower().startswith('external:'):
        if not t.lower().startswith('type('):
            t = 'type({})'.format(t)
    if kind:
        if t.lower().startswith('character'):
            return 'character({})'.format(kind)
        return '{}(kind={})'.format(t, kind)
    return t


def _collect_dim_uses(
    suite_vars: Dict[str, SuiteVar],
    host_dict,
) -> Dict[str, List[str]]:
    """Return {module_name: [local_name, ...]} for dimension variables of suite vars.

    Host-module variables (``is_control=False``) are included keyed by their
    declaring module.  Control variables and suite-owned dimensions are
    excluded — suite-owned dim scalars (set during ``_register``) are accessed
    via ``ccpp_suite_data(i)%<local>`` directly within the same module so no
    USE statement is needed for them.  Control variables cannot be dimensions
    of suite-owned data because they are not available at ``init_fields``
    time when allocations happen.

    Raises CCPPError if a dimension is found in host_dict but is a control
    variable.  Suite-owned dimensions (not in host_dict) are silently skipped.
    """
    uses: Dict[str, List] = {}
    seen: set = set()
    suite_var_std_names = set(suite_vars.keys())
    for sv in sorted(suite_vars.values(), key=lambda v: v.standard_name):
        for dim_std in sv.dimensions:
            if dim_std in seen:
                continue
            seen.add(dim_std)
            # Suite-owned dim → no USE needed (same module access).
            if dim_std in suite_var_std_names:
                continue
            if host_dict is None:
                raise CCPPError(
                    "Suite-owned variable '{}' has dimension '{}' but no host "
                    "metadata was provided to resolve it".format(
                        sv.standard_name, dim_std
                    )
                )
            entry = host_dict.get(dim_std)
            if entry is None:
                raise CCPPError(
                    "Suite-owned variable '{}' dimension '{}' not found in "
                    "host metadata or in suite-owned variables".format(
                        sv.standard_name, dim_std
                    )
                )
            if entry.is_control:
                raise CCPPError(
                    "Suite-owned variable '{}' dimension '{}' is a control "
                    "variable; suite data must use host-module or "
                    "suite-owned dimensions".format(sv.standard_name, dim_std)
                )
            mod = entry.module_name
            if mod not in uses:
                uses[mod] = []
            if entry.local_name not in uses[mod]:
                uses[mod].append(entry.local_name)
    return uses


def _dim_local_expr(dim_std: str, suite_vars: Dict[str, SuiteVar], host_dict) -> str:
    """Return the Fortran expression to use as one allocation dimension token.

    Suite-owned scalars: ``ccpp_suite_data(i)%<local>`` (in the alloc loop
    context where ``i`` is the instance index variable).  Host-owned: just the
    local name.  The caller substitutes ``i`` for the instance index variable.
    """
    if dim_std in suite_vars:
        return 'ccpp_suite_data(i)%{}'.format(suite_vars[dim_std].local_name)
    entry = host_dict.get(dim_std) if host_dict else None
    if entry is None:
        raise CCPPError(
            "Cannot resolve suite-owned dimension '{}' from host or "
            "suite vars".format(dim_std)
        )
    return entry.local_name


def _collect_ddt_uses(
    suite_vars: Dict[str, SuiteVar],
    ddt_module_map: Optional[Dict[str, str]],
) -> Dict[str, List[str]]:
    """Return ``{module_name: [ddt_type, ...]}`` for DDT types referenced
    by suite-owned variables.

    A type is treated as a DDT when it is neither a Fortran intrinsic nor
    an ``external:module:typename`` reference.  The DDT type → module
    mapping must be supplied by the caller (typically built via
    :func:`metadata.variable_resolver.build_ddt_module_map`).

    Raises CCPPError if a DDT-typed suite variable references a type that
    is missing from *ddt_module_map*.
    """
    uses: Dict[str, List[str]] = {}
    seen: set = set()
    for sv in sorted(suite_vars.values(), key=lambda v: v.standard_name):
        t = sv.type_.strip()
        tlow = t.lower()
        if tlow in _INTRINSICS or tlow.startswith('external:'):
            continue
        if tlow.startswith('type('):
            t = t[t.index('(') + 1:t.rindex(')')].strip()
        if t in seen:
            continue
        seen.add(t)
        if ddt_module_map is None or t not in ddt_module_map:
            raise CCPPError(
                "Suite-owned variable '{}' has DDT type '{}' but its "
                "defining Fortran module is unknown; the DDT must appear "
                "in a metadata file alongside a scheme/host/control "
                "table".format(sv.standard_name, t)
            )
        mod = ddt_module_map[t]
        uses.setdefault(mod, [])
        if t not in uses[mod]:
            uses[mod].append(t)
    return uses


def _generate_suite_data(
    suite_name: str,
    suite_vars: Dict[str, SuiteVar],
    host_dict=None,
    ddt_module_map: Optional[Dict[str, str]] = None,
) -> List[str]:
    """Generate the ``ccpp_<suite>_data.F90`` module source lines.

    >>> lines = _generate_suite_data('mysuite', {})
    >>> 'module ccpp_mysuite_data' in lines
    True
    >>> any('ccpp_mysuite_data_t' in l for l in lines)
    True
    """
    mod_name           = 'ccpp_{}_data'.format(suite_name)
    type_name          = 'ccpp_{}_data_t'.format(suite_name)
    alloc_sub          = 'ccpp_{}_suite_data_alloc'.format(suite_name)
    dealloc_sub        = 'ccpp_{}_suite_data_dealloc'.format(suite_name)
    init_fields_sub    = 'ccpp_{}_suite_data_init_fields'.format(suite_name)
    final_fields_sub   = 'ccpp_{}_suite_data_final_fields'.format(suite_name)
    i1 = _INDENT
    i2 = _INDENT * 2
    lines: List[str] = []

    lines.append(
        '! ccpp_{}_data.F90 -- generated by ccpp_capgen_ng, do not edit'.format(
            suite_name
        )
    )
    lines.append('module {}'.format(mod_name))
    lines.append('')

    # USE ccpp_kinds for any kind parameters referenced in suite-var
    # declarations (e.g. ``real(kind=kind_phys)``).
    kind_names = sorted({
        sv.kind for sv in suite_vars.values()
        if sv.kind and not sv.kind.startswith('len=')
    })
    if kind_names:
        lines.append(
            '{}use ccpp_kinds, only: {}'.format(i1, ', '.join(kind_names))
        )

    # USE the defining module of each DDT type referenced by suite-owned
    # variables so that ``type(<ddt>) :: <name>`` declarations are valid.
    ddt_uses = _collect_ddt_uses(suite_vars, ddt_module_map)
    for mod in sorted(ddt_uses):
        types = sorted(ddt_uses[mod])
        lines.append(
            '{}use {}, only: {}'.format(i1, mod, ', '.join(types))
        )
    if kind_names or ddt_uses:
        lines.append('')

    lines.append('{}implicit none'.format(i1))
    lines.append('{}private'.format(i1))
    lines.append('')

    # DDT type — allocatable fields use deferred-shape colons.
    #
    # ``TARGET`` is not a valid component attribute in Fortran; instead
    # the module-level ``ccpp_suite_data(:)`` array below carries
    # ``TARGET`` so that every ``ccpp_suite_data(i)%component(...)``
    # subobject is a valid pointer-assignment target.  This is what
    # the group cap needs to do ``ptr%ptr => ccpp_suite_data(i)%fld(...)``
    # for optional-arg passing and transformation temporaries.
    lines.append('{}type, public :: {}'.format(i1, type_name))
    if suite_vars:
        for sv in sorted(suite_vars.values(), key=lambda v: v.standard_name):
            t = _type_str(sv.type_, sv.kind)
            if sv.dimensions:
                rank = len(sv.dimensions)
                deferred = '({})'.format(','.join([':'] * rank))
                lines.append(
                    '{}{}, allocatable :: {}{}'.format(
                        i2, t, sv.local_name, deferred,
                    )
                )
            else:
                lines.append('{}{} :: {}'.format(i2, t, sv.local_name))
    else:
        lines.append('{}! (no suite-owned variables)'.format(i2))
    lines.append('{}end type {}'.format(i1, type_name))
    lines.append('')

    # Module-level allocatable instance array (one per model instance).
    # ``TARGET`` makes every subobject (component access, array section,
    # nested DDT field, ...) a valid pointer-assignment target.  Without
    # it Fortran rejects ``ptr => ccpp_suite_data(i)%fld(...)`` with
    # "Pointer assignment target is neither TARGET nor POINTER".
    lines.append(
        '{}type({}), allocatable, target, public :: ccpp_suite_data(:)'.format(
            i1, type_name,
        )
    )

    # Alloc/dealloc subroutines are only generated when host_dict is provided
    # (the integration path).  Unit tests that call without host_dict get the
    # type definition and allocatable array but no subroutines.
    if suite_vars and host_dict is not None:
        lines.append('')
        lines.append('{}public :: {}'.format(i1, alloc_sub))
        lines.append('{}public :: {}'.format(i1, dealloc_sub))
        lines.append('{}public :: {}'.format(i1, init_fields_sub))
        lines.append('{}public :: {}'.format(i1, final_fields_sub))
        lines.append('')
        lines.append('contains')

        sorted_svs = sorted(suite_vars.values(), key=lambda v: v.standard_name)

        # ---- suite_data_alloc: only allocate the DDT array ---------------
        # Inner allocatable fields are NOT allocated here because their
        # dimensions may depend on suite-owned scalars set during the
        # register phase.  Inner allocations live in init_fields, called
        # from <suite>_init after all _register calls have run.
        lines.append('')
        lines.append(
            '{}subroutine {}(number_of_instances, errmsg, errflg)'.format(i1, alloc_sub)
        )
        lines += [
            '',
            '{}integer, intent(in) :: number_of_instances'.format(i2),
            '{}character(len=*), intent(out) :: errmsg'.format(i2),
            '{}integer, intent(out) :: errflg'.format(i2),
            '',
            "{}errmsg = ''".format(i2),
            '{}errflg = 0'.format(i2),
            '{}if (allocated(ccpp_suite_data)) return'.format(i2),
            '{}allocate(ccpp_suite_data(number_of_instances))'.format(i2),
            '',
            '{}end subroutine {}'.format(i1, alloc_sub),
        ]

        # ---- suite_data_dealloc: only deallocate the DDT array -----------
        lines.append('')
        lines.append(
            '{}subroutine {}(errmsg, errflg)'.format(i1, dealloc_sub)
        )
        lines += [
            '',
            '{}character(len=*), intent(out) :: errmsg'.format(i2),
            '{}integer, intent(out) :: errflg'.format(i2),
            '',
            "{}errmsg = ''".format(i2),
            '{}errflg = 0'.format(i2),
            '{}if (.not. allocated(ccpp_suite_data)) return'.format(i2),
            '{}deallocate(ccpp_suite_data)'.format(i2),
            '',
            '{}end subroutine {}'.format(i1, dealloc_sub),
        ]

        # ---- suite_data_init_fields: allocate inner fields per instance --
        dim_uses = _collect_dim_uses(suite_vars, host_dict)
        lines.append('')
        lines.append(
            '{}subroutine {}(i, errmsg, errflg)'.format(i1, init_fields_sub)
        )
        for mod in sorted(dim_uses):
            syms = ', '.join(sorted(dim_uses[mod]))
            lines.append('{}use {}, only: {}'.format(i2, mod, syms))
        lines += [
            '',
            '{}integer, intent(in) :: i'.format(i2),
            '{}character(len=*), intent(out) :: errmsg'.format(i2),
            '{}integer, intent(out) :: errflg'.format(i2),
            '',
            "{}errmsg = ''".format(i2),
            '{}errflg = 0'.format(i2),
        ]
        for sv in sorted_svs:
            if sv.dimensions:
                dim_exprs = [
                    _dim_local_expr(d, suite_vars, host_dict)
                    for d in sv.dimensions
                ]
                lines.append(
                    '{}allocate(ccpp_suite_data(i)%{}({}))'.format(
                        i2, sv.local_name, ', '.join(dim_exprs)
                    )
                )
        lines += [
            '',
            '{}end subroutine {}'.format(i1, init_fields_sub),
        ]

        # ---- suite_data_final_fields: deallocate inner fields per inst ---
        lines.append('')
        lines.append(
            '{}subroutine {}(i, errmsg, errflg)'.format(i1, final_fields_sub)
        )
        lines += [
            '',
            '{}integer, intent(in) :: i'.format(i2),
            '{}character(len=*), intent(out) :: errmsg'.format(i2),
            '{}integer, intent(out) :: errflg'.format(i2),
            '',
            "{}errmsg = ''".format(i2),
            '{}errflg = 0'.format(i2),
        ]
        for sv in sorted_svs:
            if sv.dimensions:
                lines.append(
                    '{}if (allocated(ccpp_suite_data(i)%{})) '
                    'deallocate(ccpp_suite_data(i)%{})'.format(
                        i2, sv.local_name, sv.local_name
                    )
                )
        lines += [
            '',
            '{}end subroutine {}'.format(i1, final_fields_sub),
        ]

    lines.append('')
    lines.append('end module {}'.format(mod_name))
    return lines


def write_suite_data(
    suite_name: str,
    suite_vars: Dict[str, SuiteVar],
    output_root: str,
    host_dict=None,
    ddt_module_map: Optional[Dict[str, str]] = None,
    logger: Optional[logging.Logger] = None,
) -> str:
    """Write ``ccpp_<suite>_data.F90`` to *output_root*."""
    os.makedirs(output_root, exist_ok=True)
    filename = 'ccpp_{}_data.F90'.format(suite_name)
    out_path  = os.path.join(output_root, filename)
    lines = _generate_suite_data(suite_name, suite_vars, host_dict,
                                 ddt_module_map=ddt_module_map)
    with open_if_changed(out_path, logger=logger) as fh:
        fh.write('\n'.join(lines) + '\n')
    return out_path


def _generate_suite_meta(
    suite_name: str,
    suite_vars: Dict[str, SuiteVar],
) -> List[str]:
    """Generate metadata lines for ``ccpp_<suite>.meta``.

    The file documents all suite-owned variables in the standard ``.meta``
    format so that downstream tools can inspect what each suite provides.

    >>> lines = _generate_suite_meta('mysuite', {})
    >>> lines[0].startswith('!')
    True
    >>> any('ccpp_mysuite_data' in l for l in lines)
    True
    """
    mod_name = 'ccpp_{}_data'.format(suite_name)
    i1 = _INDENT
    lines: List[str] = []
    lines.append(
        '! ccpp_{}.meta -- generated by ccpp_capgen_ng, do not edit'.format(suite_name)
    )
    lines.append('[ccpp-table-properties]')
    lines.append('{}name = {}'.format(i1, mod_name))
    lines.append('{}type = suite'.format(i1))
    lines.append('')
    lines.append('[ccpp-arg-table]')
    lines.append('{}name = {}'.format(i1, mod_name))
    lines.append('{}type = suite'.format(i1))
    for sv in sorted(suite_vars.values(), key=lambda v: v.standard_name):
        lines.append('')
        lines.append('[ {} ]'.format(sv.local_name))
        lines.append('{}standard_name = {}'.format(i1, sv.standard_name))
        lines.append('{}long_name = {}'.format(i1, sv.standard_name))
        lines.append('{}units = {}'.format(i1, sv.units))
        dim_str = '({})'.format(', '.join(sv.dimensions)) if sv.dimensions else '()'
        lines.append('{}dimensions = {}'.format(i1, dim_str))
        lines.append('{}type = {}'.format(i1, sv.type_))
        if sv.kind:
            lines.append('{}kind = {}'.format(i1, sv.kind))
    return lines


def write_suite_meta(
    suite_name: str,
    suite_vars: Dict[str, SuiteVar],
    output_root: str,
    logger: Optional[logging.Logger] = None,
) -> str:
    """Write ``ccpp_<suite>.meta`` to *output_root* and return its path."""
    os.makedirs(output_root, exist_ok=True)
    filename = 'ccpp_{}.meta'.format(suite_name)
    out_path  = os.path.join(output_root, filename)
    lines = _generate_suite_meta(suite_name, suite_vars)
    with open_if_changed(out_path, logger=logger) as fh:
        fh.write('\n'.join(lines) + '\n')
    return out_path
