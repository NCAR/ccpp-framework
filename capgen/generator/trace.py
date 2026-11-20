"""Trace-emission helpers shared by all cap generators.

A generated cap module carries a compile-time gate::

    use, intrinsic :: iso_fortran_env, only: error_unit
    logical, parameter :: trace = .false.

and every cap subroutine that has at least one ``intent(in)``/
``intent(inout)`` control dummy emits a guarded write as the very first
line of its body::

    if (trace) write(error_unit, '(a,a,a,a,1x,i0)') &
        'CCPP TRACE <sub_name>:', &
        ' <local1>=', trim(<local1>), &
        ' <local2>=', <local2>, &
        ...

The format string is built per-call: each character item contributes an
``a`` descriptor (the label literal and any ``trim()``-wrapped value),
and each integer item contributes ``1x,i0`` so the value is printed
flush against a single space separator rather than the wide default
field of list-directed I/O.

Two effects:

1. With ``trace = .false.`` (the default) the compiler eliminates the
   write at the dead branch, but every control dummy is *syntactically*
   referenced inside the dead block.  This silences "unused dummy
   argument" warnings on strict compilers (Intel oneAPI in particular)
   without any runtime cost.
2. Flipping the parameter to ``.true.`` -- either via the ``--trace`` CLI
   flag at generation time, or by hand-editing one generated file --
   turns the cap into a self-describing trace, useful for diagnosing
   call-chain problems in a host integration.

The helpers below are pure (no I/O, no side effects on the caller) and
return lists of Fortran source lines with no trailing newlines.
"""

from typing import Iterable, List, Optional


def emit_module_gate(trace_default: bool, indent: str) -> List[str]:
    """Return the module-scope ``trace`` parameter declaration lines.

    The caller is responsible for ensuring ``use, intrinsic :: iso_fortran_env,
    only: error_unit`` is present in the module USE list (see
    :func:`ensure_error_unit_use`).  This helper only emits the
    ``logical, parameter`` line so it can be placed alongside other
    module-scope parameters.

    Parameters
    ----------
    trace_default : bool
        ``True`` -> emit ``logical, parameter :: trace = .true.``;
        ``False`` -> ``.false.``.
    indent : str
        Leading whitespace for each emitted line.

    Returns
    -------
    list of str
        One line, no trailing newline.

    Examples
    --------
    >>> emit_module_gate(False, '  ')
    ['  logical, parameter :: trace = .false.']
    >>> emit_module_gate(True, '  ')
    ['  logical, parameter :: trace = .true.']
    """
    value = '.true.' if trace_default else '.false.'
    return ['{}logical, parameter :: trace = {}'.format(indent, value)]


def ensure_error_unit_use(use_lines: List[str], indent: str) -> List[str]:
    """Insert an ``iso_fortran_env`` USE for ``error_unit`` if not present.

    The trace block writes to ``error_unit``, which must be visible inside
    each cap module.  Callers that already build a USE list pass it in;
    this helper appends the required line iff no existing line references
    ``error_unit`` (matched as a whole word).

    Parameters
    ----------
    use_lines : list of str
        Existing module-level USE lines (modified in place and returned).
    indent : str
        Leading whitespace for the emitted line.

    Returns
    -------
    list of str
        The same list, possibly with one extra line appended.

    Examples
    --------
    >>> ensure_error_unit_use([], '  ')
    ['  use, intrinsic :: iso_fortran_env, only: error_unit']
    >>> ensure_error_unit_use(['  use foo, only: bar'], '  ')
    ['  use foo, only: bar', '  use, intrinsic :: iso_fortran_env, only: error_unit']

    Idempotent when ``error_unit`` is already there:

    >>> ensure_error_unit_use(
    ...     ['  use, intrinsic :: iso_fortran_env, only: error_unit'], '  '
    ... )
    ['  use, intrinsic :: iso_fortran_env, only: error_unit']
    """
    for line in use_lines:
        # Whole-word match so we don't false-positive on a substring.
        tokens = line.replace(',', ' ').replace(':', ' ').split()
        if 'error_unit' in tokens:
            return use_lines
    use_lines.append(
        '{}use, intrinsic :: iso_fortran_env, only: error_unit'.format(indent)
    )
    return use_lines


def emit_trace_block(
    sub_name: str,
    ctrl_entries: Iterable,
    indent: str,
    instance_local: Optional[str] = None,
    extra_in_names: Optional[Iterable[str]] = None,
) -> List[str]:
    """Return the gated trace ``write`` lines for one cap subroutine.

    The trace lists every control dummy whose intent is ``in`` or
    ``inout`` (i.e. the dummies that a strict compiler would otherwise
    flag as unused).  ``intent(out)`` dummies (``ccpp_error_code`` /
    ``ccpp_error_message``) are excluded so the write can sit at the
    very first body line, before any initialisation, with no risk of
    reading uninitialised storage.

    Parameters
    ----------
    sub_name : str
        Fully-qualified Fortran name of the subroutine being traced;
        emitted verbatim into the trace string for grep-ability.
    ctrl_entries : iterable of HostVarEntry
        Control-variable dummies in the subroutine signature (any order;
        the helper preserves it).
    indent : str
        Leading whitespace for the ``if`` and continuation lines.
    instance_local : str, optional
        Local name of ``instance_number`` when it is not already in
        *ctrl_entries* (some lifecycle routines pass it separately).
    extra_in_names : iterable of str, optional
        Extra dummy local names to include unconditionally (treated as
        ``intent(in)`` integers).  Used for routines whose signatures
        carry non-control intent(in) dummies that the compiler may also
        flag as unused (e.g. ``number_of_instances`` in state_alloc).

    Returns
    -------
    list of str
        Lines forming a single ``if (trace) write(error_unit, *) ...``
        continuation block.  Empty when there is nothing to print.
    """
    from generator.group_cap import _ctrl_intent_for  # avoid import cycle at module load

    # Build the (local_name, is_character) list in signature order.
    items: List = []
    seen = set()
    for entry in ctrl_entries:
        if _ctrl_intent_for(entry.standard_name) == 'out':
            continue
        if entry.local_name in seen:
            continue
        seen.add(entry.local_name)
        is_char = entry.type.strip().lower() == 'character'
        items.append((entry.local_name, is_char))
    if instance_local and instance_local not in seen:
        seen.add(instance_local)
        items.append((instance_local, False))
    if extra_in_names:
        for name in extra_in_names:
            if name not in seen:
                seen.add(name)
                items.append((name, False))

    if not items:
        return []

    # Build a per-call format: one ``a`` for the trace name, then for each
    # item one ``a`` for the ``' label='`` literal plus ``a`` (character)
    # or ``1x,i0`` (integer) for the value.
    fmt_parts: List[str] = ['a']
    for (_, is_char) in items:
        fmt_parts.append('a')
        fmt_parts.append('a' if is_char else '1x,i0')
    fmt = "'({})'".format(','.join(fmt_parts))

    lines: List[str] = []
    lines.append('{}if (trace) write(error_unit, {}) &'.format(indent, fmt))
    lines.append("{}    'CCPP TRACE {}:', &".format(indent, sub_name))
    for i, (local_name, is_char) in enumerate(items):
        expr = 'trim({})'.format(local_name) if is_char else local_name
        sep = ', &' if i < len(items) - 1 else ''
        lines.append(
            "{}    ' {}=', {}{}".format(indent, local_name, expr, sep)
        )
    return lines
