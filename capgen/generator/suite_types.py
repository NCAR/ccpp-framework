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

import logging
import os
import re
from typing import Dict, List, Optional, Set, Tuple

from metadata.parse_tools import CCPPError, open_if_changed
from generator.suite_resolver import SuiteResolution, iter_phase_calls

_INDENT = '  '

# Fortran intrinsic types; anything else is treated as a DDT (or an
# ``external:<module>:<typename>`` reference, handled separately).
_INTRINSICS = frozenset({
    'real', 'integer', 'character', 'logical', 'complex', 'double precision'
})


def _is_intrinsic(type_: str) -> bool:
    return type_.strip().lower() in _INTRINSICS


def _is_external(type_: str) -> bool:
    return type_.strip().lower().startswith('external:')


def _split_external(type_: str) -> Tuple[str, str]:
    """Parse ``external:<module>:<typename>`` into ``(module, typename)``."""
    parts = type_.strip().split(':', 2)
    if len(parts) != 3:
        raise CCPPError(
            "Malformed external type spec '{}'; expected "
            "'external:<module>:<typename>'".format(type_)
        )
    return parts[1], parts[2]


########################################################################
# Type-name helpers
########################################################################

def _ptr_type_name(
    type_: str,
    kind: str,
    rank: int,
    context: str = '',
) -> str:
    """Return the Fortran derived-type name for a pointer wrapper.

    Parameters
    ----------
    type_ : str
        Fortran intrinsic type (e.g. ``'real'``, ``'integer'``) or DDT
        type name (e.g. ``'cmpfsw_type'``).  External types
        (``'external:<module>:<typename>'``) are reduced to just
        ``<typename>`` for the wrapper name.
    kind : str
        Kind parameter (e.g. ``'kind_phys'``), or ``''`` if none.
    rank : int
        Number of array dimensions (0 = scalar).
    context : str, optional
        Free-form prefix passed through to :func:`_sanitize_len_suffix`
        and prepended to any error message raised from there.  Callers
        identify the offending scheme + argument here so the user can
        locate the metadata block to fix; see
        :func:`_ptr_type_name_for_arg`.

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
    >>> _ptr_type_name('cmpfsw_type', '', 1)
    'cmpfsw_type_rank1_ptr_type'
    >>> _ptr_type_name('external:mpi_f08:mpi_comm', '', 0)
    'mpi_comm_rank0_ptr_type'
    >>> _ptr_type_name('character', 'len=10', 1)
    'character_len10_rank1_ptr_type'
    >>> _ptr_type_name('character', 'len=3', 1)
    'character_len3_rank1_ptr_type'
    """
    if _is_external(type_):
        _, typename = _split_external(type_)
        name = typename
    else:
        name = type_
    parts = [name]
    if kind:
        if kind.startswith('len='):
            # Different character lengths require *different* wrapper
            # types — a DDT component can't be ``character(len=*)``, so
            # the wrapper name must encode the length spec.  Sanitise
            # the suffix because raw lengths like ``:`` (deferred) or
            # ``*`` (assumed) aren't valid Fortran identifier chars,
            # and would otherwise produce illegal type names like
            # ``character_len:_rank1_ptr_type`` that the compiler
            # rejects.
            len_spec = kind[len('len='):].strip()
            parts.append('len' + _sanitize_len_suffix(len_spec, context=context))
        else:
            parts.append(kind)
    parts.append('rank{}'.format(rank))
    parts.append('ptr_type')
    return '_'.join(parts)


def _sanitize_len_suffix(len_spec: str, context: str = '') -> str:
    """Return a Fortran-identifier-safe suffix for a ``character(len=…)`` spec.

    Pointer-wrapper type names embed the length specifier, e.g.
    ``character_len10_rank1_ptr_type``.  Raw length specs include
    forms that aren't valid Fortran identifier characters:

    * ``len=N`` (positive integer literal) — already safe.
    * ``len=:`` (deferred length, paired with ``pointer`` / ``allocatable``) —
      replaced with ``_deferred``.
    * ``len=*`` (assumed length) — REJECTED: assumed-length is not
      legal as a DDT component spec.  Raises :class:`CCPPError`.
    * ``len=NAME`` (Fortran parameter or constant symbol) — kept verbatim
      when it is already a valid identifier.

    Anything that doesn't fit those forms raises ``CCPPError`` rather
    than silently producing an illegal Fortran identifier.

    *context* — when non-empty, prefixed to every error message as
    ``"<context>: ..."``.  Callers identify the offending scheme +
    argument so the user can locate the metadata block to fix.
    """
    prefix = '{}: '.format(context) if context else ''
    spec = len_spec.strip()
    if not spec:
        raise CCPPError(
            "{}Empty character length specifier 'len=' in pointer-wrapper "
            "type name construction; expected an integer literal, "
            "a parameter name, or ':' for deferred length.".format(prefix)
        )
    if spec == ':':
        return '_deferred'
    if spec == '*':
        raise CCPPError(
            "{}character(len=*) cannot appear as a DDT component, so "
            "capgen cannot generate a pointer-wrapper type for it.  "
            "Use a concrete length, a parameter constant, or 'len=:' "
            "(deferred length, paired with allocatable / pointer) "
            "in the metadata instead.".format(prefix)
        )
    # Plain integer literal (digits) or Fortran identifier — accept verbatim.
    if spec.isdigit() or _IDENT_RE.match(spec):
        return spec
    raise CCPPError(
        "{}Cannot derive a Fortran-identifier-safe pointer-wrapper type "
        "name from 'len={}'.  Expected an integer literal, a parameter "
        "identifier, or ':' (deferred length).".format(prefix, spec)
    )


_IDENT_RE = re.compile(r'^[A-Za-z_][A-Za-z0-9_]*$')


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

    Narrow override for ``character(len=*)``: the resolver explicitly
    accepts scheme ``kind=len=*`` paired with host ``kind=len=N`` (or
    ``len=:``) as compatible and emits no kind transform — so we are
    always in Case 2 here, and the pointer points at host data with a
    concrete length.  The DDT-component rules forbid ``len=*``, but the
    host's concrete kind is usable verbatim, so fall through to the
    host kind in that one situation.  All other types keep the
    scheme-wins precedence — Case 4 transform temps need the scheme
    kind, and ``character`` is the only intrinsic where ``len=*``
    survives the resolver as an unconverted compatibility.
    """
    if arg.host_entry is not None:
        type_ = arg.host_entry.type
    else:
        type_ = arg.suite_var.type_
    host_kind = (arg.host_entry.kind if arg.host_entry
                 else arg.suite_var.kind)
    if type_ == 'character' and arg.kind_scheme == 'len=*':
        kind = host_kind
    else:
        kind = arg.kind_scheme or host_kind
    rank  = _ptr_rank(arg)
    return type_, kind, rank


def _ptr_type_name_for_arg(arg, scheme_name: str) -> str:
    """Build the pointer-wrapper type name for *arg* with rich error context.

    Resolves ``(type_, kind, rank)`` via :func:`_ptr_type_for_arg`, then
    delegates to :func:`_ptr_type_name` passing a *context* string of
    the form ``"scheme '<scheme>', optional argument [<local>]
    (standard_name=<std>, intent=<int>)"``.  Any :class:`CCPPError` from
    the name-construction path then carries enough information for the
    user to find the offending metadata block without grepping.

    Use this at every site that calls :func:`_ptr_type_name` on a
    resolved scheme argument.  Bare :func:`_ptr_type_name` calls are
    only appropriate when no per-argument context is available (e.g.
    iterating the already-validated combo set in
    :func:`_generate_suite_types`).
    """
    type_, kind, rank = _ptr_type_for_arg(arg)
    context = (
        "scheme '{}', optional argument [{}] "
        "(standard_name={}, intent={})".format(
            scheme_name, arg.scheme_local_name,
            arg.standard_name, arg.intent,
        )
    )
    return _ptr_type_name(type_, kind, rank, context=context)


########################################################################
# Collection helpers
########################################################################

def _collect_ptr_type_combos(
    suite_res: SuiteResolution,
) -> Set[Tuple[str, str, int]]:
    """Collect unique (type, kind, rank) tuples needed by optional args.

    Validates each combo eagerly via :func:`_ptr_type_name_for_arg` so
    that any unsupported shape (notably ``character(len=*)``) raises a
    :class:`CCPPError` carrying the offending scheme + argument name
    here, rather than later from the bare-tuple loop in
    :func:`_generate_suite_types` (which would have nothing useful to
    say to the user).  The constructed name itself is discarded — only
    the validation matters at this point.

    Parameters
    ----------
    suite_res : SuiteResolution

    Returns
    -------
    set of (type_, kind, rank)
    """
    combos: Set[Tuple[str, str, int]] = set()
    for resolved_group in suite_res.groups:
        for items in resolved_group.phase_calls.values():
            for resolved_call in iter_phase_calls(items):
                for arg in resolved_call.args:
                    if arg.ptr_name:
                        _ptr_type_name_for_arg(arg, resolved_call.scheme_name)
                        combos.add(_ptr_type_for_arg(arg))
    return combos


########################################################################
# Module generator
########################################################################

def _fortran_type_str_simple(type_: str, kind: str) -> str:
    """Build a Fortran type-clause for a pointer-wrapper declaration.

    Handles three categories:

    * **Intrinsics** (``real``, ``integer``, ``character``, ...) — emit
      with optional ``(kind=...)`` (or ``(len=...)`` for character).
    * **DDT types** (anything not an intrinsic and not ``external:`` —
      e.g. ``cmpfsw_type``) — wrap in ``type(...)`` so the declaration
      is a syntactically valid Fortran derived-type reference.  Kind is
      not meaningful for DDTs.
    * **External types** (``external:<module>:<typename>``) — emit
      ``type(<typename>)``.  The defining module is USE'd separately
      (see :func:`_collect_ddt_uses`).

    >>> _fortran_type_str_simple('real', 'kind_phys')
    'real(kind=kind_phys)'
    >>> _fortran_type_str_simple('integer', '')
    'integer'
    >>> _fortran_type_str_simple('character', 'len=512')
    'character(len=512)'
    >>> _fortran_type_str_simple('cmpfsw_type', '')
    'type(cmpfsw_type)'
    >>> _fortran_type_str_simple('external:mpi_f08:mpi_comm', '')
    'type(mpi_comm)'
    """
    t = type_.strip()
    if _is_external(t):
        _, typename = _split_external(t)
        return 'type({})'.format(typename)
    if not _is_intrinsic(t):
        # DDT — Fortran requires the type(...) wrapper.  Kind is not
        # meaningful here; drop it.
        return 'type({})'.format(t)
    if kind:
        if t.lower().startswith('character'):
            return 'character({})'.format(kind)
        return '{}(kind={})'.format(t, kind)
    return t


def _collect_ddt_uses(
    combos: Set[Tuple[str, str, int]],
    ddt_module_map: Optional[Dict[str, str]],
) -> Dict[str, Set[str]]:
    """Group DDT and external types referenced in *combos* by USE module.

    Intrinsics are skipped (they need no USE).  DDT types are looked up
    in *ddt_module_map* (built by
    :func:`metadata.variable_resolver.build_ddt_module_map`); external
    types parse their module out of the ``external:<module>:<typename>``
    prefix.

    Returns
    -------
    dict mapping ``module_name -> {typename, ...}``.

    Raises
    ------
    CCPPError
        If a DDT referenced by a pointer wrapper is absent from
        *ddt_module_map* — the generator can't emit a valid USE for it.
    """
    uses: Dict[str, Set[str]] = {}
    for type_, _kind, _rank in combos:
        t = type_.strip()
        if _is_intrinsic(t):
            continue
        if _is_external(t):
            mod, typename = _split_external(t)
            uses.setdefault(mod, set()).add(typename)
            continue
        # DDT — look up the defining Fortran module.
        if ddt_module_map is None or t not in ddt_module_map:
            raise CCPPError(
                "Pointer wrapper needs DDT '{}' but its defining Fortran "
                "module is unknown. Declare it explicitly with "
                "'module_name = <module>' in the '{}' DDT's "
                "[ccpp-table-properties], or co-locate the DDT table with a "
                "scheme/host/control table in the same .meta file.".format(
                    t, t,
                )
            )
        uses.setdefault(ddt_module_map[t], set()).add(t)
    return uses


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
    ddt_module_map: Optional[Dict[str, str]] = None,
) -> List[str]:
    """Generate the Fortran source lines for the suite types module.

    Parameters
    ----------
    suite_name : str
    combos : set of (type_, kind, rank)
    ddt_module_map : dict, optional
        DDT type name → defining Fortran module.  Required when any
        ``combos`` entry's *type_* is a DDT — the module is USE'd so
        the ``type(<ddt>)`` reference resolves.

    Returns
    -------
    list of str (without trailing newlines)
    """
    mod_name = 'ccpp_{}_types'.format(suite_name)
    lines: List[str] = []

    lines.append(
        '! {}.F90 -- generated by ccpp_capgen, do not edit'.format(mod_name)
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

    # USE the defining module for every DDT (or external) type the
    # pointer wrappers reference, so ``type(<name>)`` resolves.
    ddt_uses = _collect_ddt_uses(combos, ddt_module_map)
    for mod in sorted(ddt_uses):
        symbols = ', '.join(sorted(ddt_uses[mod]))
        lines.append('{}use {}, only: {}'.format(_INDENT, mod, symbols))
    if kind_names or ddt_uses:
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
    ddt_module_map: Optional[Dict[str, str]] = None,
    logger: Optional[logging.Logger] = None,
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

    lines = _generate_suite_types(suite_name, combos, ddt_module_map)
    with open_if_changed(out_path, logger=logger) as fh:
        fh.write('\n'.join(lines) + '\n')
    return out_path
