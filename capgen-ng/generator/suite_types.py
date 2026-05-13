#!/usr/bin/env python3

"""Generate the shared pointer-wrapper types module for a suite.

``ccpp_<suite>_types.F90`` is a small Fortran module that declares one derived
type per unique (intrinsic-type, kind, rank) combination required by optional
arguments across all groups in the suite.  Each generated type looks like::

    type :: real_kind_phys_rank1_ptr_type
      real(kind=kind_phys), pointer :: ptr(:) => null()
    end type real_kind_phys_rank1_ptr_type

Group cap modules USE this types module to declare optional-argument pointer
variables.

Only generated when at least one optional argument is present.
"""

import os
from typing import List, Optional, Set, Tuple

from generator.suite_resolver import SuiteResolution, iter_phase_calls

_INDENT = '  '


########################################################################
# Type-name helpers
########################################################################

def _ptr_type_name(type_: str, kind: str, rank: int) -> str:
    """Return the Fortran derived-type name for a pointer wrapper.

    Parameters
    ----------
    type_ : str
        Fortran intrinsic type (e.g. ``'real'``, ``'integer'``).
    kind : str
        Kind parameter (e.g. ``'kind_phys'``), or ``''`` if none.
    rank : int
        Number of array dimensions (0 = scalar).

    Returns
    -------
    str

    Examples
    --------
    >>> _ptr_type_name('integer', '', 1)
    'integer_rank1_ptr_type'
    >>> _ptr_type_name('real', 'kind_phys', 1)
    'real_kind_phys_rank1_ptr_type'
    >>> _ptr_type_name('real', '', 0)
    'real_rank0_ptr_type'
    >>> _ptr_type_name('real', 'kind_phys', 2)
    'real_kind_phys_rank2_ptr_type'
    """
    parts = [type_]
    if kind and not kind.startswith('len='):
        parts.append(kind)
    parts.append('rank{}'.format(rank))
    parts.append('ptr_type')
    return '_'.join(parts)


def _ptr_rank(arg) -> int:
    """Return the effective rank of the value pointed to by *arg*'s pointer.

    For optional args without transform (Case 2) or with transform (Case 4),
    the pointer rank equals the number of dimensions of the host/suite variable.
    """
    if arg.host_entry is not None:
        return len(arg.host_entry.dimensions)
    if arg.suite_var is not None:
        return len(arg.suite_var.dimensions)
    return 0


def _ptr_type_for_arg(arg) -> Tuple[str, str, int]:
    """Return the (type_, kind, rank) tuple for *arg*'s pointer wrapper.

    For Case 2 (optional, no transform), the pointer targets the host
    variable directly — same type and kind.  For Case 4 (optional +
    transform), the pointer targets the transformation temporary, which
    carries the scheme's kind.
    """
    if arg.host_entry is not None:
        type_ = arg.host_entry.type
    else:
        type_ = arg.suite_var.type_
    kind  = arg.kind_scheme or (arg.host_entry.kind if arg.host_entry else
                                arg.suite_var.kind)
    rank  = _ptr_rank(arg)
    return type_, kind, rank


########################################################################
# Collection helpers
########################################################################

def _collect_ptr_type_combos(
    suite_res: SuiteResolution,
) -> Set[Tuple[str, str, int]]:
    """Collect unique (type, kind, rank) tuples needed by optional args.

    Parameters
    ----------
    suite_res : SuiteResolution

    Returns
    -------
    set of (type_, kind, rank)
    """
    combos: Set[Tuple[str, str, int]] = set()
    for rg in suite_res.groups:
        for items in rg.phase_calls.values():
            for rc in iter_phase_calls(items):
                for arg in rc.args:
                    if arg.ptr_name:
                        combos.add(_ptr_type_for_arg(arg))
    return combos


########################################################################
# Module generator
########################################################################

def _fortran_type_str_simple(type_: str, kind: str) -> str:
    """Minimal Fortran type-clause builder (intrinsics only)."""
    t = type_.strip()
    if kind:
        if t.lower().startswith('character'):
            return 'character({})'.format(kind)
        return '{}(kind={})'.format(t, kind)
    return t


def _dim_spec(rank: int) -> str:
    """Return the deferred-shape dimension specifier for a *rank*-dimensional pointer.

    >>> _dim_spec(0)
    ''
    >>> _dim_spec(1)
    '(:)'
    >>> _dim_spec(2)
    '(:,:)'
    """
    if rank == 0:
        return ''
    return '({})'.format(','.join([':'] * rank))


def _generate_suite_types(
    suite_name: str,
    combos: Set[Tuple[str, str, int]],
) -> List[str]:
    """Generate the Fortran source lines for the suite types module.

    Parameters
    ----------
    suite_name : str
    combos : set of (type_, kind, rank)

    Returns
    -------
    list of str (without trailing newlines)
    """
    mod_name = 'ccpp_{}_types'.format(suite_name)
    lines: List[str] = []

    lines.append(
        '! {}.F90 -- generated by ccpp_capgen_ng, do not edit'.format(mod_name)
    )
    lines.append('module {}'.format(mod_name))
    lines.append('')

    # USE ccpp_kinds for any kind parameters referenced in pointer-wrapper
    # type declarations (e.g. ``real(kind=kind_phys)``).
    kind_names = sorted({
        kind for _t, kind, _r in combos
        if kind and not kind.startswith('len=')
    })
    if kind_names:
        lines.append(
            '{}use ccpp_kinds, only: {}'.format(_INDENT, ', '.join(kind_names))
        )
        lines.append('')

    lines.append('{}implicit none'.format(_INDENT))
    lines.append('{}private'.format(_INDENT))
    lines.append('')

    sorted_combos = sorted(combos)

    for type_, kind, rank in sorted_combos:
        tname = _ptr_type_name(type_, kind, rank)
        lines.append('{}public :: {}'.format(_INDENT, tname))

    lines.append('')

    for type_, kind, rank in sorted_combos:
        tname  = _ptr_type_name(type_, kind, rank)
        ftype  = _fortran_type_str_simple(type_, kind)
        dimspec = _dim_spec(rank)
        lines.append('{}type :: {}'.format(_INDENT, tname))
        lines.append(
            '{}  {}, pointer :: ptr{} => null()'.format(_INDENT, ftype, dimspec)
        )
        lines.append('{}end type {}'.format(_INDENT, tname))
        lines.append('')

    lines.append('end module {}'.format(mod_name))
    return lines


########################################################################
# Public API
########################################################################

def write_suite_types(
    suite_name: str,
    suite_res: SuiteResolution,
    output_root: str,
) -> Optional[str]:
    """Write the suite types module to *output_root*.

    Does nothing and returns ``None`` when the suite has no optional arguments.

    Parameters
    ----------
    suite_name : str
    suite_res : SuiteResolution
    output_root : str
        Output directory (created if absent).

    Returns
    -------
    str or None
        Absolute path of the written file, or ``None`` if nothing was written.
    """
    combos = _collect_ptr_type_combos(suite_res)
    if not combos:
        return None

    os.makedirs(output_root, exist_ok=True)
    filename = 'ccpp_{}_types.F90'.format(suite_name)
    out_path = os.path.join(output_root, filename)

    lines = _generate_suite_types(suite_name, combos)
    with open(out_path, 'w', encoding='utf-8') as fh:
        fh.write('\n'.join(lines) + '\n')
    return out_path
