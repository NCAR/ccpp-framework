#!/usr/bin/env python3

"""ccpp_validator — validate Fortran source files against CCPP scheme metadata.

For each scheme phase declared in a ``.meta`` file this tool checks that the
corresponding Fortran subroutine:

1. **Exists** in the Fortran source tree.
2. Has the **same number of dummy arguments** as declared in the metadata.
3. The dummy-argument **names match** the ``local_name`` values in the metadata
   (order-insensitive).
4. For every dummy argument present in both sides, the **per-arg attributes**
   agree: ``intent``, ``type``, ``kind``, and number of dimensions (rank).
   ``character`` length must be declared CONSISTENTLY — the metadata mirrors
   the Fortran exactly, so ``len=*`` matches only ``len=*`` and ``len=N`` only
   the identical ``len=N`` (no wildcarding).  Old-style F77 forms
   (``character*64``, ``character*(*)``, ``c*5``) are normalised to the
   ``len=`` form before comparison.  Additionally, host / DDT metadata passed
   via ``--host-files`` *defines* its character storage, so ``len=*`` is
   rejected there outright (a concrete ``len=N`` required); see
   :func:`_check_definition_character_lengths`.  Control tables are exempt.

Asymmetric treatment of ``optional``:

* Fortran-declared optional argument **absent** from metadata → silently
  allowed (the cap never passes it); emits a ``logger.warning``.
* Fortran-declared optional argument **present** in metadata as
  ``optional=False`` → silently allowed (the cap always passes it, which
  is a valid subset of the Fortran contract); emits a ``logger.warning``.
* Metadata declares ``optional=True`` but Fortran does **not** carry the
  ``optional`` attribute → **error** (the cap-side ``present()`` check
  would be invalid on a Fortran-required dummy).

The tool does *not* compare dimension *bounds* across sides — it only
checks that rank matches.  Comparing standard-name dimension references
against Fortran local-name dimensions would require loading host metadata
too; that's a separate feature.

Usage
-----
::

    ccpp_validator.py \\
        --scheme-files  scheme1.meta,scheme2.meta \\
        --source-files  scheme1.F90,scheme2.F90   \\
        [--verbose]

Exit codes
----------
0 — all checks passed
1 — one or more validation errors found
2 — internal / usage error
"""

import argparse
import logging
import os
import re
import sys
from typing import Dict, List, NamedTuple, Optional, Set, Tuple

# Ensure the capgen package is importable when invoked directly.
_SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
_PACKAGE_DIR = os.path.dirname(_SCRIPT_DIR)
if _PACKAGE_DIR not in sys.path:
    sys.path.insert(0, _PACKAGE_DIR)

from metadata.parse_tools import CCPPError, init_log, set_log_level
from metadata.variable_resolver import SchemeStore
from metadata.metadata_table import parse_metadata_file

_LOGGER = init_log('ccpp_validator')

# ---------------------------------------------------------------------------
# Fortran source parsing helpers
# ---------------------------------------------------------------------------

# Matches the start of a subroutine definition (case-insensitive, optional
# prefixes recursive/pure/elemental).
_SUB_RE = re.compile(
    r'(?i)\s*(?:(?:recursive|pure|elemental|impure)\s+)*'
    r'subroutine\s+(\w+)\s*(?:\(([^)]*)\))?'
)
# Matches `end subroutine [name]` (case-insensitive).  Bare ``end`` is not
# detected — CCPP scheme code consistently uses the explicit form.
_END_SUB_RE = re.compile(r'(?i)^\s*end\s*subroutine\b')
_CONT_RE = re.compile(r'&\s*(?:!.*)?$')  # Fortran line continuation
_COMMENT_RE = re.compile(r'!.*$')
# Fixed-form continuation marker.  F77 / fixed-form Fortran requires a
# non-blank character in column 6 of a continuation line; CCPP code
# conventionally uses ``&`` for this and pairs it with a trailing ``&``
# on the prior line for portability with free-form parsers.  Only
# applied when we know we are mid-continuation (the buffer is non-empty).
_LEAD_CONT_RE = re.compile(r'^\s*&\s?')
# Matches an identifier-character anywhere in a string.  Used by the
# decoration-repair branch to distinguish "stray punctuation past a
# trailing ``&``" (safe to drop) from "real tokens past a ``&``"
# (leave alone so the parser surfaces a real error).
_IDENT_CHAR_RE = re.compile(r'[A-Za-z_0-9]')


class _ArgAttrs(NamedTuple):
    """Per-dummy-argument attributes parsed from a Fortran type-decl line.

    All string fields are lowercased and stripped.  Missing / unknown is
    represented by an empty string (or ``False`` for ``optional``, ``0``
    for ``rank``).

    Attributes
    ----------
    type_ : str
        Intrinsic type (``'real'``, ``'integer'``, ``'logical'``,
        ``'complex'``, ``'character'``) or a derived-type spec
        (``'type(my_type)'``, ``'class(other)'``).  Empty for args
        not declared in the body (parser missed the decl).
    kind_ : str
        Kind selector.  For numeric types this is the kind name
        (``'kind_phys'``, ``'8'``, ``'int64'``).  For character it is
        the length selector (``'len=10'``, ``'len=*'``, ``'len=:'``).
        Empty when no selector was present.
    intent : str
        ``'in'`` / ``'out'`` / ``'inout'``, or ``''`` when no intent was
        declared (treated as INOUT by Fortran, but for validation we
        prefer to flag the absence explicitly).
    optional : bool
        True iff the type-decl line carried the ``optional`` attribute.
    rank : int
        Number of dimensions.  Computed from ``dimension(...)`` on the
        line, or from ``var(:,:,...)``-style trailing parens on the
        variable token.  ``0`` for scalar.
    """
    type_:    str
    kind_:    str
    intent:   str
    optional: bool
    rank:     int


class _SubSig(NamedTuple):
    """Parsed signature of one Fortran subroutine.

    Attributes
    ----------
    args : list of str
        Lowercase dummy-argument names in declaration order.
    optional : set of str
        Subset of *args* declared with the ``optional`` attribute in the
        subroutine body.  These args may be absent from the metadata
        without producing a validation error — they will simply never be
        passed at the cap call site.
    attrs : dict
        Mapping of lowercase arg name to :class:`_ArgAttrs`.  Args
        whose type-decl line couldn't be parsed are absent from the
        dict; ``_validate_arg_attributes`` skips per-attribute checks
        for those args (the name-set check still applies).
    """
    args:     List[str]
    optional: Set[str]
    attrs:    Dict[str, '_ArgAttrs']


def _paren_aware_split(s: str, sep: str) -> List[str]:
    """Split *s* on *sep*, ignoring separators inside balanced parentheses.

    Examples
    --------
    >>> _paren_aware_split('integer, optional, intent(in)', ',')
    ['integer', ' optional', ' intent(in)']
    >>> _paren_aware_split('x, y(:,:), z', ',')
    ['x', ' y(:,:)', ' z']
    """
    result: List[str] = []
    depth = 0
    buf = ''
    for ch in s:
        if ch == '(':
            depth += 1
            buf += ch
        elif ch == ')':
            depth -= 1
            buf += ch
        elif ch == sep and depth == 0:
            result.append(buf)
            buf = ''
        else:
            buf += ch
    if buf:
        result.append(buf)
    return result


_INTENT_RE     = re.compile(r'(?i)^intent\s*\(\s*(in\s*out|inout|in|out)\s*\)\s*$')
_DIM_ATTR_RE   = re.compile(r'(?i)^dimension\s*\(\s*(.*?)\s*\)\s*$')
_TYPE_SPEC_RE  = re.compile(
    r'(?i)^\s*(real|integer|logical|complex|character|double\s*precision'
    r'|type\s*\([^)]*\)|class\s*\([^)]*\))\s*(\(.*\))?\s*$'
)
_KIND_SELECTOR_RE = re.compile(r'(?i)^\(\s*(.*?)\s*\)$')


def _split_type_spec(spec: str) -> "Tuple[str, str]":
    """Split a Fortran type spec into ``(type, kind)``.

    The type is lowercased; the kind selector is left in its raw form
    (lowercased, whitespace stripped).  Returns ``('', '')`` if *spec*
    isn't a recognised type spec.

    Examples
    --------
    >>> _split_type_spec('real')
    ('real', '')
    >>> _split_type_spec('real(kind=kind_phys)')
    ('real', 'kind_phys')
    >>> _split_type_spec('real(kind_phys)')
    ('real', 'kind_phys')
    >>> _split_type_spec('real(8)')
    ('real', '8')
    >>> _split_type_spec('integer(int64)')
    ('integer', 'int64')
    >>> _split_type_spec('character(len=10)')
    ('character', 'len=10')
    >>> _split_type_spec('character(len=*)')
    ('character', 'len=*')
    >>> _split_type_spec('character(*)')
    ('character', 'len=*')
    >>> _split_type_spec('character')
    ('character', '')
    >>> _split_type_spec('character*64')
    ('character', 'len=64')
    >>> _split_type_spec('character*(*)')
    ('character', 'len=*')
    >>> _split_type_spec('character*(80)')
    ('character', 'len=80')
    >>> _split_type_spec('type(my_t)')
    ('type(my_t)', '')
    >>> _split_type_spec('double precision')
    ('double precision', '')
    >>> _split_type_spec('not_a_type')
    ('', '')
    """
    spec = spec.strip()
    # Old-style (F77) character length given with ``*`` instead of a modern
    # ``(len=...)`` selector:
    #   character*64    -> len=64       character*(*)   -> len=*
    #   character*(80)  -> len=80       character*(CL)  -> len=cl  (named)
    m_old = re.match(r'(?i)^character\s*\*\s*(.+)$', spec)
    if m_old is not None:
        length = m_old.group(1).strip()
        paren = re.match(r'^\(\s*(.*?)\s*\)$', length)   # peel one ( ) layer
        if paren is not None:
            length = paren.group(1).strip()
        if length == '*':
            return ('character', 'len=*')
        return ('character', 'len={}'.format(length.lower()))
    m = _TYPE_SPEC_RE.match(spec)
    if m is None:
        return ('', '')
    type_raw = m.group(1).lower()
    kind_paren = m.group(2) or ''
    # Normalise whitespace inside "double  precision".
    type_ = re.sub(r'\s+', ' ', type_raw)
    if type_.startswith('type(') or type_.startswith('class('):
        # Strip whitespace inside the parens.
        type_ = re.sub(r'\s+', '', type_)
        return (type_, '')
    if not kind_paren:
        return (type_, '')
    inner_match = _KIND_SELECTOR_RE.match(kind_paren.strip())
    if inner_match is None:
        return (type_, '')
    inner = inner_match.group(1).strip()
    if type_ == 'character':
        # character has its own selector grammar.  Accept:
        #   *           -> len=*
        #   len=...     -> len=...
        #   <int>       -> len=<int>
        #   len=...,kind=...  -> use the len= portion
        if inner == '*':
            return ('character', 'len=*')
        if inner.lower().startswith('len='):
            # Strip trailing ",kind=..." if present.
            len_part = inner.split(',')[0].strip()
            return ('character', len_part.lower())
        if re.match(r'^\d+$', inner) or inner == ':':
            return ('character', 'len={}'.format(inner))
        # Anything else: store raw, lowercased.
        return ('character', inner.lower())
    # Numeric types: accept ``kind=<x>`` or bare ``<x>``.
    if inner.lower().startswith('kind='):
        inner = inner[len('kind='):].strip()
    return (type_, inner.lower())


def _parse_decl_line(line: str) -> Dict[str, _ArgAttrs]:
    """Parse a Fortran type-declaration line into per-name attributes.

    Returns a (possibly empty) mapping ``{lower_name: _ArgAttrs}``.  Lines
    that aren't type declarations (no ``::``) or whose type spec doesn't
    parse return ``{}``.

    Examples
    --------
    >>> attrs = _parse_decl_line('integer, intent(in) :: im')
    >>> attrs['im']
    _ArgAttrs(type_='integer', kind_='', intent='in', optional=False, rank=0)
    >>> attrs = _parse_decl_line('real(kind=kind_phys), intent(inout) :: temp(:,:)')
    >>> attrs['temp']
    _ArgAttrs(type_='real', kind_='kind_phys', intent='inout', optional=False, rank=2)
    >>> attrs = _parse_decl_line('character(len=*), intent(out) :: errmsg')
    >>> attrs['errmsg']
    _ArgAttrs(type_='character', kind_='len=*', intent='out', optional=False, rank=0)
    >>> attrs = _parse_decl_line('real, optional, intent(in), dimension(:) :: a, b(:,:), c')
    >>> sorted(attrs.items())
    [('a', _ArgAttrs(type_='real', kind_='', intent='in', optional=True, rank=1)), ('b', _ArgAttrs(type_='real', kind_='', intent='in', optional=True, rank=2)), ('c', _ArgAttrs(type_='real', kind_='', intent='in', optional=True, rank=1))]
    >>> _parse_decl_line('  ! comment :: not a decl')
    {}
    >>> _parse_decl_line('integer :: only_local')
    {'only_local': _ArgAttrs(type_='integer', kind_='', intent='', optional=False, rank=0)}
    >>> _parse_decl_line('character*256, intent(out) :: scheme_name')['scheme_name']
    _ArgAttrs(type_='character', kind_='len=256', intent='out', optional=False, rank=0)
    >>> sorted(_parse_decl_line('character :: c*5, d(10)*8').items())
    [('c', _ArgAttrs(type_='character', kind_='len=5', intent='', optional=False, rank=0)), ('d', _ArgAttrs(type_='character', kind_='len=8', intent='', optional=False, rank=1))]
    """
    line = _COMMENT_RE.sub('', line)
    if '::' not in line:
        return {}
    before, _, after = line.partition('::')
    tokens = _paren_aware_split(before, ',')
    if not tokens:
        return {}
    type_, kind_ = _split_type_spec(tokens[0])
    if not type_:
        return {}
    intent = ''
    optional = False
    line_rank = 0
    for tok in tokens[1:]:
        t = tok.strip()
        tl = t.lower()
        if tl == 'optional':
            optional = True
            continue
        m = _INTENT_RE.match(t)
        if m:
            iv = m.group(1).lower().replace(' ', '')
            intent = iv  # 'in' / 'out' / 'inout'
            continue
        m = _DIM_ATTR_RE.match(t)
        if m:
            line_rank = len(_paren_aware_split(m.group(1), ','))
            continue
        # Anything else (allocatable, pointer, target, parameter, save,
        # public, private, contiguous, asynchronous, volatile, value) is
        # ignored — we only validate the attrs metadata declares.
    result: Dict[str, _ArgAttrs] = {}
    for var_tok in _paren_aware_split(after, ','):
        var_tok = var_tok.strip()
        if not var_tok:
            continue
        # Strip any ``= <init>`` (or ``=> <target>``) clause before the
        # name regex.  A Fortran array initialiser may carry a full
        # ``(/ ..., ..., ... /)`` constructor whose inner commas would
        # otherwise be gobbled by the regex's greedy parens-matcher and
        # miscounted as rank-N entries.  ``=`` characters inside other
        # parenthesised sub-expressions (e.g. ``::x = (a==b)``) live at
        # depth > 0 and are skipped.
        var_tok = _strip_initialiser(var_tok).rstrip()
        # Old-style (F77) per-entity character length: ``c*5`` / ``d(10)*8``
        # / ``s*(*)``.  A trailing ``*<length>`` overrides the type-spec
        # length for THIS entity only.
        entity_kind = None
        m_star = re.match(
            r'(?i)^(.*?)\s*\*\s*(\(\s*\*\s*\)|\(\s*\w+\s*\)|\d+|\*|\w+)\s*$',
            var_tok,
        )
        if m_star is not None:
            var_tok = m_star.group(1).strip()
            length = m_star.group(2).strip()
            paren = re.match(r'^\(\s*(.*?)\s*\)$', length)
            if paren is not None:
                length = paren.group(1).strip()
            entity_kind = 'len=*' if length == '*' else 'len={}'.format(length.lower())
        name_match = re.match(r'(\w+)\s*(\((.*)\))?\s*$', var_tok)
        if name_match is None:
            continue
        name = name_match.group(1).lower()
        inner = name_match.group(3)
        if inner is not None:
            rank = len(_paren_aware_split(inner, ','))
        else:
            rank = line_rank
        result[name] = _ArgAttrs(
            type_=type_,
            kind_=entity_kind if entity_kind is not None else kind_,
            intent=intent, optional=optional, rank=rank,
        )
    return result


def _strip_initialiser(var_tok: str) -> str:
    """Return *var_tok* with any trailing ``= <init>`` removed.

    Paren-aware: only an ``=`` (or the first ``=`` of ``=>``) at nesting
    depth zero is treated as the start of an initialiser.  ``=``
    characters inside parenthesised sub-expressions (such as array
    initialisers ``(/ ... /)`` or default expressions ``(a == b)``)
    are skipped.

    Examples
    --------
    >>> _strip_initialiser('foo')
    'foo'
    >>> _strip_initialiser('foo(:)')
    'foo(:)'
    >>> _strip_initialiser('p => null()')
    'p '
    >>> _strip_initialiser('arr(n) = (/ 1, 2, 3 /)')
    'arr(n) '
    >>> _strip_initialiser('std_name_array(num_consts) = (/ '
    ...                    "'a', 'b', 'c' /)")
    'std_name_array(num_consts) '
    """
    depth = 0
    for i, ch in enumerate(var_tok):
        if ch == '(':
            depth += 1
        elif ch == ')':
            depth -= 1
        elif ch == '=' and depth == 0:
            return var_tok[:i]
    return var_tok


def _join_continuation(
    lines: List[str],
    filename: Optional[str] = None,
) -> List[str]:
    """Join Fortran continuation lines (ending with ``&``) into single logical lines.

    Handles four continuation conventions seen in real CCPP physics code:

    * **Free-form**: ``&`` only at the trailing end of the prior line.
    * **Dual-form**: ``&`` at the trailing end of the prior line *and*
      at column 6 of the next line.  In this case the leading ``&`` is
      part of the continuation marker, not the continued expression,
      and is stripped before the next line is appended to the buffer.
    * **Fixed-form leading-only**: NO trailing ``&`` on the prior line,
      but a ``&`` (or any non-blank) at column 6 of the next line.  F77
      / fixed-form Fortran treats this as a continuation; CCPP physics
      occasionally relies on it (e.g. ``sfc_sice.f``'s ``sfc_sice_run``
      signature, where the line before the closing ``)`` has no trailing
      ``&``).  Detected by look-ahead at the next non-blank, non-comment
      line.
    * **Decorated trailing ``&``** (repair): a ``&`` near the end of the
      line is followed by stray non-identifier characters (commas,
      parens, whitespace) — typically a typo or hand-edit artefact that
      compilers silently ignore because it lives past column 72 in
      strict fixed-form mode.  When the next line's column-6 ``&``
      already proves we are mid-continuation, treat the last ``&`` as
      the continuation marker, discard the decoration, and emit a
      ``logger.warning`` naming *filename* so the user knows their
      source has decoration past the statement end.

    Parameters
    ----------
    lines : list of str
        Source lines, each ending in ``\\n`` (as from ``splitlines(keepends=True)``).
    filename : str, optional
        Source path used only in the decoration-repair warning message.
        Defaults to ``<unknown>`` when not supplied.

    Examples
    --------
    >>> _join_continuation(['  foo &\\n', '  bar\\n', '  baz\\n'])
    ['  foo   bar', '  baz']
    >>> _join_continuation(['  foo &\\n', '     &  bar\\n', '  baz\\n'])
    ['  foo  bar', '  baz']
    >>> _join_continuation(['  foo &\\n', '     &  bar\\n',
    ...                     '     &   )\\n', '  baz\\n'])
    ['  foo  bar  )', '  baz']
    """
    # First pass: normalise each line — strip trailing newlines and any
    # inline ``!`` comment.  Keep blank/comment-only lines as ``''`` so
    # we can skip them when buffering and still use their position for
    # look-ahead.
    norm: List[str] = []
    for raw in lines:
        line = raw.rstrip('\n').rstrip('\r')
        norm.append(_COMMENT_RE.sub('', line))

    def _next_starts_with_lead_cont(start_idx: int) -> bool:
        """True iff the next non-blank, non-comment line begins with a
        leading ``&`` (fixed-form column-6 continuation marker)."""
        j = start_idx + 1
        while j < len(norm) and not norm[j].strip():
            j += 1
        return j < len(norm) and bool(_LEAD_CONT_RE.match(norm[j]))

    result: List[str] = []
    buf = ''
    for i, stripped in enumerate(norm):
        if buf and not stripped.strip():
            # Mid-continuation, and this line is blank or a pure
            # comment.  Fortran 90+ permits such lines interleaved
            # between continuation lines without ending the logical
            # line — skip and keep accumulating.
            continue
        if buf:
            # Mid-continuation: drop a leading ``&`` (fixed-form
            # column-6 marker) so it doesn't end up glued into the
            # continued expression.  No-op on free-form code.
            stripped = _LEAD_CONT_RE.sub('', stripped, count=1)
        has_trailing = bool(_CONT_RE.search(stripped))
        if has_trailing:
            buf += _CONT_RE.sub('', stripped)
            continue
        # No trailing ``&`` — but a fixed-form continuation may still
        # be implied by the next line's column-6 ``&``.  If so, keep
        # buffering rather than flushing, and try the decoration-repair
        # in case the trailing ``&`` was decorated with stray punctuation
        # that lives past column 72 (compilers silently drop it; we'd
        # otherwise glue it into the joined statement).
        if _next_starts_with_lead_cont(i):
            stripped = _repair_decorated_trailing_amp(stripped, filename, i + 1)
            buf += stripped
            continue
        buf += stripped
        result.append(buf)
        buf = ''
    if buf:
        result.append(buf)
    return result


def _repair_decorated_trailing_amp(
    line: str,
    filename: Optional[str],
    line_no: int,
) -> str:
    """Strip a decorated trailing ``&`` from *line*.

    Called from the fixed-form look-ahead branch of
    :func:`_join_continuation`, where the next line's column-6 ``&`` has
    already established that we are mid-continuation.  If *line*
    contains a ``&`` followed only by non-identifier characters
    (commas, parens, semicolons, whitespace), the ``&`` is the
    decorated continuation marker — drop everything from it onward and
    emit a single ``WARNING`` so the user sees that their source has
    decoration the compiler is silently ignoring.

    If *line* contains no ``&`` (true fixed-form-leading-only
    continuation), or if any token past the last ``&`` looks like a
    real Fortran identifier, the line is returned unchanged so the
    parser can surface a real error.
    """
    amp_idx = line.rfind('&')
    if amp_idx < 0:
        return line
    trailing = line[amp_idx + 1:].strip()
    if not trailing:
        # ``_CONT_RE`` should have caught this; defensive no-op.
        return line
    if _IDENT_CHAR_RE.search(trailing):
        return line
    _LOGGER.warning(
        "%s:%d: dropping decoration past trailing '&' (%r); "
        "compiler silently ignores this but the parser would otherwise "
        "glue it into the statement",
        filename or '<unknown>', line_no, line[amp_idx:],
    )
    return line[:amp_idx]


def _parse_subroutines(
    source: str,
    filename: Optional[str] = None,
) -> Dict[str, _SubSig]:
    """Extract subroutine signatures from Fortran *source*.

    Returns a mapping ``{subroutine_name_lower: _SubSig}`` where each
    :class:`_SubSig` carries the dummy-argument list (in declaration
    order) and the subset of those args declared with the ``optional``
    attribute in the subroutine body.

    Only the first definition of each subroutine name is recorded
    (Fortran does not allow overloading at the subroutine level).
    Subroutine and argument names are lowercased for case-insensitive
    comparison.

    Optional-attribute detection scans body lines for type-declaration
    lines of the form ``<type>, ..., optional, ... :: <name>[, <name>]...``
    while a tracking session is open for that subroutine.  Optional
    declarations inside a *nested* subroutine with the same name as an
    outer subroutine are attributed to the inner sub (which is then
    discarded by the first-occurrence-wins rule); optional declarations
    inside a nested sub with a *different* name are correctly
    attributed to that nested sub.

    Examples
    --------
    >>> src = 'subroutine foo(a, b, c)\\n  integer, intent(in) :: a, b, c\\nend subroutine foo\\n'
    >>> result = _parse_subroutines(src)
    >>> result['foo'].args
    ['a', 'b', 'c']
    >>> sorted(result['foo'].optional)
    []
    >>> src2 = 'subroutine bar()\\nend subroutine bar\\n'
    >>> _parse_subroutines(src2)['bar'].args
    []
    >>> src3 = ('subroutine baz(x, y, z)\\n'
    ...         '  integer, intent(in) :: x\\n'
    ...         '  integer, optional, intent(in) :: y\\n'
    ...         '  integer, intent(out), optional :: z\\n'
    ...         'end subroutine baz\\n')
    >>> sig = _parse_subroutines(src3)['baz']
    >>> sig.args
    ['x', 'y', 'z']
    >>> sorted(sig.optional)
    ['y', 'z']
    >>> sig.attrs['x'].intent, sig.attrs['x'].type_
    ('in', 'integer')
    >>> sig.attrs['y'].optional
    True
    """
    logical = _join_continuation(
        source.splitlines(keepends=True), filename=filename,
    )
    args_by_name:     Dict[str, List[str]]            = {}
    optional_by_name: Dict[str, Set[str]]             = {}
    attrs_by_name:    Dict[str, Dict[str, _ArgAttrs]] = {}
    # Stack of names whose body we are currently scanning.  Each entry is
    # the recorded name (for which we collect attrs) or ``None`` when
    # this is a duplicate-name sub whose body should be skipped (its
    # args were already discarded).
    stack: List[Optional[str]] = []

    for line in logical:
        m = _SUB_RE.match(line)
        if m:
            name = m.group(1).lower()
            arglist_raw = m.group(2) or ''
            args = [a.strip().lower() for a in arglist_raw.split(',')
                    if a.strip()]
            if name not in args_by_name:
                args_by_name[name] = args
                optional_by_name[name] = set()
                attrs_by_name[name] = {}
                stack.append(name)
            else:
                stack.append(None)  # duplicate: ignore
            continue
        if _END_SUB_RE.match(line):
            if stack:
                stack.pop()
            continue
        if stack and stack[-1] is not None:
            tracked = stack[-1]
            arg_set = set(args_by_name[tracked])
            for var_name, attrs in _parse_decl_line(line).items():
                if var_name not in arg_set:
                    continue
                # First decl line wins (Fortran disallows redeclaration,
                # so this only matters for malformed input).
                if var_name not in attrs_by_name[tracked]:
                    attrs_by_name[tracked][var_name] = attrs
                if attrs.optional:
                    optional_by_name[tracked].add(var_name)

    return {
        name: _SubSig(args=args_by_name[name],
                      optional=optional_by_name[name],
                      attrs=attrs_by_name[name])
        for name in args_by_name
    }


def _load_source_tree(source_files: List[str]) -> Dict[str, _SubSig]:
    """Read all Fortran source files and return a merged subroutine dict.

    Parameters
    ----------
    source_files : list of str
        Paths to ``.F90`` / ``.f90`` files.

    Returns
    -------
    dict
        Merged ``{subroutine_name_lower: _SubSig}``; first occurrence
        wins if the same name appears in multiple files.
    """
    merged: Dict[str, _SubSig] = {}
    for fpath in source_files:
        with open(fpath) as fh:
            src = fh.read()
        for name, sig in _parse_subroutines(src, filename=fpath).items():
            if name not in merged:
                merged[name] = sig
    return merged


# ---------------------------------------------------------------------------
# Module-level and derived-type parsing (for host / DDT validation)
# ---------------------------------------------------------------------------

# Matches the start of a module definition (case-insensitive).  Excludes
# ``module procedure`` so we don't mistake interface-block lines for
# module headers.
_MODULE_RE = re.compile(r'(?i)^\s*module\s+(?!procedure\b)(\w+)\s*$')
_END_MODULE_RE = re.compile(r'(?i)^\s*end\s*module\b')

# Matches the start of a derived-type definition.  Accepts the modern
# ``type :: name`` form, the older ``type, <attrs> :: name`` form, and
# the bare ``type name`` form.  Excludes ``type(x) ::`` declarations
# (those are variable decls of a type) by requiring no opening paren
# before the name on the type-defining form.
_TYPE_DEF_RE = re.compile(
    r'(?i)^\s*type(?:\s*,\s*[^:]+)?\s*::\s*(\w+)\s*$'
)
_TYPE_DEF_BARE_RE = re.compile(r'(?i)^\s*type\s+(\w+)\s*$')
_END_TYPE_RE = re.compile(r'(?i)^\s*end\s*type\b')

# Matches ``contains`` at module / type-block scope, used to recognise the
# boundary between module-level decls and the module's subroutines (we
# stop collecting module-level vars at the first ``contains``).  Also
# applies inside a derived type with type-bound procedures.
_CONTAINS_RE = re.compile(r'(?i)^\s*contains\s*$')


class _ModuleSig(NamedTuple):
    """Parsed module-level declarations from one Fortran module.

    Attributes
    ----------
    vars : dict
        ``{lower_name: _ArgAttrs}`` for every module-level variable
        declaration above the ``contains`` line (or end of module).
        Used to validate ``type = host`` table entries.
    ddts : dict
        ``{lower_type_name: {lower_component_name: _ArgAttrs}}`` for
        every ``type :: X ... end type X`` block at module scope.
        Used to validate ``type = ddt`` table entries.
    """
    vars: Dict[str, '_ArgAttrs']
    ddts: Dict[str, Dict[str, '_ArgAttrs']]


def _parse_modules(
    source: str,
    filename: Optional[str] = None,
) -> Dict[str, _ModuleSig]:
    """Extract module-level variable decls and derived-type definitions.

    Walks *source* line by line tracking three nested contexts: module,
    derived-type block, and subroutine body.  Module-level variable
    declarations are collected only at module scope above ``contains``;
    derived-type components are collected only inside a
    ``type :: X ... end type X`` block; subroutine local decls are
    ignored.

    The first definition of each module / type wins (Fortran does not
    permit redefinition; this only matters for malformed input).

    Examples
    --------
    >>> src = ('module my_mod\\n'
    ...        '  use kinds, only: kind_phys\\n'
    ...        '  integer :: nlev\\n'
    ...        '  real(kind=kind_phys) :: cp\\n'
    ...        '  type :: phys_t\\n'
    ...        '    real(kind=kind_phys) :: tk(:,:)\\n'
    ...        '    integer :: nlay\\n'
    ...        '  end type phys_t\\n'
    ...        'contains\\n'
    ...        '  subroutine helper(x)\\n'
    ...        '    integer, intent(in) :: x\\n'
    ...        '    real :: local\\n'
    ...        '  end subroutine helper\\n'
    ...        'end module my_mod\\n')
    >>> mods = _parse_modules(src)
    >>> sorted(mods.keys())
    ['my_mod']
    >>> sorted(mods['my_mod'].vars.keys())
    ['cp', 'nlev']
    >>> mods['my_mod'].vars['cp'].type_
    'real'
    >>> mods['my_mod'].vars['cp'].kind_
    'kind_phys'
    >>> sorted(mods['my_mod'].ddts.keys())
    ['phys_t']
    >>> sorted(mods['my_mod'].ddts['phys_t'].keys())
    ['nlay', 'tk']
    >>> mods['my_mod'].ddts['phys_t']['tk'].rank
    2
    >>> 'local' in mods['my_mod'].vars
    False
    """
    logical = _join_continuation(
        source.splitlines(keepends=True), filename=filename,
    )

    modules: Dict[str, _ModuleSig] = {}
    # Active module context (None outside of any module).
    cur_mod_name: Optional[str] = None
    cur_mod_vars: Dict[str, _ArgAttrs] = {}
    cur_mod_ddts: Dict[str, Dict[str, _ArgAttrs]] = {}
    # Active derived-type block context inside the current module.
    cur_type_name: Optional[str] = None
    cur_type_comps: Dict[str, _ArgAttrs] = {}
    # Depth of subroutine / function nesting inside the current module.
    # Decls inside a subroutine are local variables, not module-level
    # state, and must be skipped.
    sub_depth: int = 0
    # Once a ``contains`` is seen at module scope, module-level decls
    # are done — the rest of the module is type-bound and subroutine
    # bodies.  We still need to track sub_depth to find the matching
    # ``end module``.
    past_module_contains: bool = False

    for line in logical:
        # Module header / footer.
        m = _MODULE_RE.match(line)
        if m and cur_mod_name is None:
            cur_mod_name = m.group(1).lower()
            cur_mod_vars = {}
            cur_mod_ddts = {}
            cur_type_name = None
            cur_type_comps = {}
            sub_depth = 0
            past_module_contains = False
            continue
        if _END_MODULE_RE.match(line) and cur_mod_name is not None:
            if cur_mod_name not in modules:
                modules[cur_mod_name] = _ModuleSig(
                    vars=cur_mod_vars, ddts=cur_mod_ddts,
                )
            cur_mod_name = None
            continue
        if cur_mod_name is None:
            # Free-floating decls outside any module are not part of the
            # host-validation surface.  CCPP host code conventionally
            # lives inside modules.
            continue

        # Track subroutine / function nesting so we can skip local
        # declarations.  Use _SUB_RE for subroutines; functions are
        # less common in CCPP host code but handled symmetrically.
        if _SUB_RE.match(line) or re.match(r'(?i)\s*(?:(?:recursive|pure|elemental|impure)\s+)*(?:(?:real|integer|logical|complex|character|double\s*precision|type\s*\([^)]+\))\s+)?function\s+\w+', line):
            sub_depth += 1
            continue
        if _END_SUB_RE.match(line) or re.match(r'(?i)^\s*end\s*function\b', line):
            if sub_depth > 0:
                sub_depth -= 1
            continue
        if sub_depth > 0:
            continue

        # Derived-type block boundaries (only honoured at module scope,
        # never inside a subroutine body).
        if cur_type_name is None:
            m = _TYPE_DEF_RE.match(line) or _TYPE_DEF_BARE_RE.match(line)
            if m:
                cur_type_name = m.group(1).lower()
                cur_type_comps = {}
                continue
        else:
            if _END_TYPE_RE.match(line):
                if cur_type_name not in cur_mod_ddts:
                    cur_mod_ddts[cur_type_name] = cur_type_comps
                cur_type_name = None
                cur_type_comps = {}
                continue
            if _CONTAINS_RE.match(line):
                # Type-bound procedures follow; no more components.
                continue
            # Inside a type block: every parsed decl is a component.
            for name, attrs in _parse_decl_line(line).items():
                if name not in cur_type_comps:
                    cur_type_comps[name] = attrs
            continue

        # Module scope: check for ``contains`` boundary and otherwise
        # collect module-level variable decls.
        if _CONTAINS_RE.match(line):
            past_module_contains = True
            continue
        if past_module_contains:
            continue
        for name, attrs in _parse_decl_line(line).items():
            if name not in cur_mod_vars:
                cur_mod_vars[name] = attrs

    return modules


def _load_modules_tree(
    source_files: List[str],
) -> Tuple[Dict[str, _ModuleSig], Dict[str, Dict[str, _ArgAttrs]]]:
    """Read all Fortran source files and return module + global DDT dicts.

    Returns a tuple ``(modules, ddt_index)``:

    * ``modules`` — ``{module_name_lower: _ModuleSig}``.  First occurrence
      wins if the same module name appears in multiple files.
    * ``ddt_index`` — flat ``{type_name_lower: {component_name_lower:
      _ArgAttrs}}`` mapping derived-type names to their component dicts,
      collected across every parsed module.  Used to resolve
      ``type(name) :: var`` declarations whose underlying type lives in
      a different module than the variable.  First occurrence wins.

    Parameters
    ----------
    source_files : list of str
        Paths to ``.F90`` / ``.f90`` files.
    """
    modules: Dict[str, _ModuleSig] = {}
    ddt_index: Dict[str, Dict[str, _ArgAttrs]] = {}
    for fpath in source_files:
        with open(fpath) as fh:
            src = fh.read()
        for name, sig in _parse_modules(src, filename=fpath).items():
            if name not in modules:
                modules[name] = sig
            for ddt_name, comps in sig.ddts.items():
                if ddt_name not in ddt_index:
                    ddt_index[ddt_name] = comps
    return modules, ddt_index


# ---------------------------------------------------------------------------
# Validation logic
# ---------------------------------------------------------------------------

def _validate_scheme(
    scheme_name: str,
    scheme_store: SchemeStore,
    subroutine_tree: Dict[str, _SubSig],
    logger: logging.Logger,
) -> List[str]:
    """Validate one scheme against *subroutine_tree*.

    Optional Fortran-only args (declared with the ``optional`` attribute
    in the body and absent from the scheme metadata) are silently allowed
    — they will never be passed at the cap call site, so the host need
    not declare or provide them.  Non-optional Fortran args missing from
    the metadata, and metadata args missing from Fortran, remain hard
    errors.

    Parameters
    ----------
    scheme_name : str
    scheme_store : SchemeStore
    subroutine_tree : dict
    logger : Logger

    Returns
    -------
    list of str
        Error messages (empty if all checks passed).
    """
    errors: List[str] = []
    for phase in scheme_store.phases_for(scheme_name):
        sub_name = '{}_{}'.format(scheme_name, phase).lower()
        meta_vars = scheme_store.variables_for(scheme_name, phase) or []

        logger.debug("Checking %s (phase=%s, sub=%s)", scheme_name, phase, sub_name)

        if sub_name not in subroutine_tree:
            errors.append(
                "Subroutine '{}' declared in metadata (scheme '{}', phase '{}') "
                "not found in any source file.".format(sub_name, scheme_name, phase)
            )
            continue

        sig = subroutine_tree[sub_name]
        fort_args: List[str] = sig.args
        fort_optional: Set[str] = sig.optional
        meta_local_names: List[str] = [v.local_name.lower() for v in meta_vars]

        meta_set: Set[str] = set(meta_local_names)
        fort_set: Set[str] = set(fort_args)
        # Optional Fortran args that are absent from the metadata are
        # silently allowed — never passed at the call site.
        fort_only_optional: Set[str] = (fort_set - meta_set) & fort_optional
        # Effective Fortran arg count for the count-mismatch check
        # excludes those silent optional-only-in-Fortran args.
        effective_fort_count = len(fort_args) - len(fort_only_optional)

        if len(meta_local_names) != effective_fort_count:
            extra = ''
            if fort_only_optional:
                extra = ' (plus {} optional-only-in-Fortran args silently ' \
                        'allowed: {})'.format(
                            len(fort_only_optional),
                            sorted(fort_only_optional),
                        )
            # Degenerate-parse hint: if the Fortran subroutine was
            # found but yielded zero args while metadata declares
            # many, the parser almost certainly failed on the
            # signature — most often because the file uses a
            # continuation style (or other Fortran dialect feature)
            # not handled by ``_join_continuation``.  Flag it so the
            # error trace points at the actual cause instead of a
            # spurious "every metadata arg is missing" diff.
            if len(fort_args) == 0 and len(meta_local_names) > 0:
                extra += (
                    "  HINT: the Fortran signature parser found the "
                    "subroutine but extracted zero arguments.  This is "
                    "almost always a parser bug, not a real mismatch — "
                    "common causes are unsupported continuation styles "
                    "or unusual signature syntax.  Check the .F90 file "
                    "for the subroutine declaration and report a "
                    "validator bug if the signature looks normal."
                )
            errors.append(
                "Argument count mismatch for '{}': "
                "metadata declares {} args {}, "
                "Fortran declares {} required args.{}".format(
                    sub_name,
                    len(meta_local_names), meta_local_names,
                    effective_fort_count, extra,
                )
            )

        only_meta = meta_set - fort_set
        only_fort_required = (fort_set - meta_set) - fort_optional
        if only_meta:
            errors.append(
                "Arguments in metadata but not Fortran for '{}': {}".format(
                    sub_name, sorted(only_meta)
                )
            )
        if only_fort_required:
            errors.append(
                "Non-optional arguments in Fortran but not metadata for '{}': {}".format(
                    sub_name, sorted(only_fort_required)
                )
            )
        # Per-arg attribute checks for args present in BOTH sides.
        meta_by_name = {v.local_name.lower(): v for v in meta_vars}
        for name in sorted(meta_set & fort_set):
            fattrs = sig.attrs.get(name)
            if fattrs is None:
                # Decl line failed to parse; skip attribute checks for
                # this arg.  Name-set check already covered presence.
                continue
            mvar = meta_by_name[name]
            errors.extend(
                _check_arg_attributes(sub_name, name, mvar, fattrs)
            )
            # Optional flag — asymmetric:
            #  - metadata says optional, Fortran doesn't → hard error
            #    (cap may pass a missing arg, but Fortran requires it).
            #  - Fortran says optional, metadata doesn't → warning
            #    (cap always passes it; that's a valid subset of the
            #    Fortran contract, but the metadata writer may have
            #    intended to mark it optional).
            if mvar.optional and not fattrs.optional:
                errors.append(
                    "Arg '{}' on '{}': metadata declares optional=True "
                    "but Fortran does not carry the 'optional' attribute "
                    "(cap-side present() checks would be invalid)".format(
                        name, sub_name,
                    )
                )
            elif fattrs.optional and not mvar.optional:
                logger.warning(
                    "Fortran argument '%s' on subroutine '%s' is "
                    "declared optional but metadata does not mark it "
                    "optional; cap will always pass it",
                    name, sub_name,
                )
        # Fortran-only optional args (absent from metadata entirely):
        # silently allowed but worth a heads-up — the host won't see
        # them and the metadata writer may have meant to declare them.
        for name in sorted(fort_only_optional):
            logger.warning(
                "Optional Fortran argument '%s' on subroutine '%s' is "
                "absent from metadata; it will never be passed at the "
                "call site",
                name, sub_name,
            )
    return errors


_EXTERNAL_TYPE_PREFIX_RE = re.compile(r'(?i)^external\s*:\s*[^:]+\s*:\s*')
_DDT_WRAPPER_RE          = re.compile(r'(?i)^(?:type|class)\s*\(\s*(.+?)\s*\)\s*$')


def _normalize_type_for_comparison(type_str: str) -> str:
    """Return a comparison-friendly form of a CCPP type string.

    Rules:

    * Lowercase, whitespace collapsed.
    * ``type(name)`` / ``class(name)`` wrapper → bare ``name``.
    * ``external:<module>:<typename>`` → bare ``typename`` (the module
      part is metadata-only; Fortran uses the bare type name once the
      module is brought in via a ``use`` clause).
    * ``doubleprecision`` → ``double precision``.
    * Anything else is returned as-is (intrinsics, DDT names, etc.).

    With this normalisation, a metadata declaration ``type = ty_rad_lw``
    matches a Fortran ``type(ty_rad_lw)`` dummy, and a metadata
    ``type = external:mpi_f08:mpi_comm`` matches a Fortran
    ``type(mpi_comm)`` dummy.  Intrinsic comparisons (``real`` vs
    ``real``) are unaffected.

    Examples
    --------
    >>> _normalize_type_for_comparison('real')
    'real'
    >>> _normalize_type_for_comparison('REAL')
    'real'
    >>> _normalize_type_for_comparison('double precision')
    'double precision'
    >>> _normalize_type_for_comparison('doubleprecision')
    'double precision'
    >>> _normalize_type_for_comparison('ty_rad_lw')
    'ty_rad_lw'
    >>> _normalize_type_for_comparison('type(ty_rad_lw)')
    'ty_rad_lw'
    >>> _normalize_type_for_comparison('Type( Ty_Rad_LW )')
    'ty_rad_lw'
    >>> _normalize_type_for_comparison('class(ty_rad_lw)')
    'ty_rad_lw'
    >>> _normalize_type_for_comparison('external:mpi_f08:mpi_comm')
    'mpi_comm'
    >>> _normalize_type_for_comparison('external : esmf_mod : esmf_clock')
    'esmf_clock'
    """
    s = type_str.strip().lower()
    s = re.sub(r'\s+', ' ', s)
    s = _EXTERNAL_TYPE_PREFIX_RE.sub('', s)
    m = _DDT_WRAPPER_RE.match(s)
    if m:
        s = m.group(1).strip()
    if s == 'doubleprecision':
        s = 'double precision'
    return s


def _check_arg_attributes(
    sub_name:  str,
    arg_name:  str,
    meta_var,
    fort:      _ArgAttrs,
) -> List[str]:
    """Compare per-attribute consistency for one dummy argument.

    Compared attributes: ``intent``, ``type``, ``kind``, dimension
    *rank* (number of dims).  ``character`` length must be declared
    CONSISTENTLY: the metadata mirrors the Fortran exactly -- ``len=*``
    matches only ``len=*`` and ``len=N`` only the identical ``len=N`` (no
    wildcarding).  The ``optional`` attribute is checked at the call site
    in :func:`_validate_scheme` because one direction emits a warning
    (logger-dependent) rather than an error.

    Returns a list of error message strings (empty on full match).
    """
    errs: List[str] = []
    prefix = "Arg '{}' on '{}': ".format(arg_name, sub_name)

    # intent — only check when the metadata actually declares one (it's
    # required for scheme vars but we don't reach this helper for
    # non-scheme tables anyway).
    meta_intent = (meta_var.intent or '').lower()
    if meta_intent and fort.intent and meta_intent != fort.intent:
        errs.append(
            prefix + "intent mismatch (metadata={!r}, Fortran={!r})".format(
                meta_intent, fort.intent,
            )
        )
    elif meta_intent and not fort.intent:
        errs.append(
            prefix + "intent declared as {!r} in metadata but absent "
            "from Fortran declaration".format(meta_intent)
        )

    # type — case-insensitive, with normalisation that puts intrinsic,
    # DDT, and external types on equal footing.  See
    # :func:`_normalize_type_for_comparison` for the rules.
    meta_type = (meta_var.type or '').strip()
    if meta_type and fort.type_:
        meta_norm = _normalize_type_for_comparison(meta_type)
        fort_norm = _normalize_type_for_comparison(fort.type_)
        if meta_norm != fort_norm:
            errs.append(
                prefix + "type mismatch (metadata={!r}, Fortran={!r})".format(
                    meta_var.type, fort.type_,
                )
            )

    # kind — case-insensitive.  Empty matches empty.  character has
    # the ``len=*`` wildcard on either side.
    meta_kind = (meta_var.kind or '').strip().lower()
    fort_kind = fort.kind_
    if meta_type == 'character' or fort.type_ == 'character':
        # Character length must be CONSISTENT between metadata and Fortran:
        # the metadata mirrors the declaration, it does not loosely match it.
        # ``len=*`` matches only ``len=*`` (assumed length on one side vs a
        # concrete length on the other is a real inconsistency), and ``len=N``
        # matches only the identical ``len=N``.
        if meta_kind != fort_kind:
            errs.append(
                prefix + "character length mismatch "
                "(metadata={!r}, Fortran={!r}); the metadata kind must mirror "
                "the Fortran declaration exactly -- len=* only matches len=*, "
                "len=N only matches the same len=N".format(meta_kind, fort_kind)
            )
    else:
        if meta_kind != fort_kind:
            errs.append(
                prefix + "kind mismatch (metadata={!r}, Fortran={!r})".format(
                    meta_kind or '<none>', fort_kind or '<none>',
                )
            )

    # rank — number of dimensions.  When the metadata ``local_name``
    # carries a subscript (sliced array entry such as
    # ``q(:,:,index_of_water_vapor)``), the metadata's ``dimensions``
    # list describes the *view* after slicing, not the underlying
    # Fortran rank.  ``_expected_fort_rank`` resolves this: it returns
    # the subscript width when a subscript is present, otherwise
    # ``len(meta_var.dimensions)``.
    meta_dims = list(meta_var.dimensions or [])
    expected_rank = _expected_fort_rank(meta_var.local_name, meta_dims)
    if expected_rank != fort.rank:
        errs.append(
            prefix + "rank mismatch (metadata implies Fortran rank {} "
            "from local_name '{}' and dimensions {}, Fortran declares "
            "rank {})".format(
                expected_rank, meta_var.local_name, meta_dims, fort.rank,
            )
        )

    return errs


def _base_local_name(local_name: str) -> str:
    """Return the bare Fortran identifier from a metadata ``local_name``.

    Host / DDT metadata occasionally carries subscripted ``local_name``
    values (sliced array entries) — the matching Fortran decl carries the
    bare identifier, so strip the subscript before lookup.  Lowercase for
    case-insensitive comparison.

    Examples
    --------
    >>> _base_local_name('cp')
    'cp'
    >>> _base_local_name('Phys_State')
    'phys_state'
    >>> _base_local_name('tk(:,:)')
    'tk'
    >>> _base_local_name('dqdt(:,:,index_of_cloud)')
    'dqdt'
    """
    name = local_name.strip()
    if '(' in name:
        name = name.split('(', 1)[0]
    return name.lower()


def _expected_fort_rank(local_name: str, meta_dims: List[str]) -> int:
    """Return the Fortran rank implied by a metadata ``local_name``.

    Sliced metadata local names (``q(:,:,index_of_X)``) express a
    reduced-rank *view* of a higher-rank Fortran component: every
    subscript entry consumes one dimension of the underlying Fortran
    variable, but only ``:`` entries survive into the resulting view's
    rank.  The metadata's ``dimensions =`` list describes the view, not
    the underlying variable — so the expected Fortran rank equals the
    total number of subscript entries, not ``len(meta_dims)``.

    For bare local names (no subscript), Fortran rank simply equals the
    metadata-declared dimension count.

    Examples
    --------
    >>> _expected_fort_rank('cp', [])
    0
    >>> _expected_fort_rank('tk', ['horizontal_dimension',
    ...                            'vertical_layer_dimension'])
    2
    >>> _expected_fort_rank('q(:,:,index_of_water_vapor)',
    ...                     ['horizontal_dimension',
    ...                      'vertical_layer_dimension'])
    3
    >>> _expected_fort_rank('q(:)', ['horizontal_dimension'])
    1
    >>> _expected_fort_rank('q(:,:,:)', ['horizontal_dimension',
    ...                                  'vertical_layer_dimension',
    ...                                  'number_of_tracers'])
    3
    """
    name = local_name.strip()
    if '(' not in name:
        return len(meta_dims)
    inner = name.split('(', 1)[1]
    inner = inner.rsplit(')', 1)[0]
    # Each subscript entry consumes one Fortran dimension; paren-aware
    # split protects nested expressions like ``my(:,foo(a,b),:)`` from
    # being miscounted (none of the in-tree fixtures use that today, but
    # the parser permits it).
    return len(_paren_aware_split(inner, ','))


def _validate_host_table(
    table,
    modules_tree: Dict[str, _ModuleSig],
    logger: logging.Logger,
) -> List[str]:
    """Validate a ``type = host`` metadata table against its Fortran module.

    The Fortran module name is taken from ``table.module_name`` when set,
    otherwise from ``table.table_name`` (the .meta convention).  For each
    metadata variable, look up the matching module-level decl by base
    local-name and reuse :func:`_check_arg_attributes` for the per-attr
    checks (intent is silently ignored since host vars carry no intent).

    Returns a list of error message strings (empty on full match).  When
    the named module is missing entirely, a single "module not found"
    error is returned and per-variable checks are skipped.
    """
    errors: List[str] = []
    mod_name = (table.module_name or table.table_name).lower()
    sig = modules_tree.get(mod_name)
    if sig is None:
        errors.append(
            "Host module '{}' (from table '{}' in '{}') not found in any "
            "source file.".format(mod_name, table.table_name, table.file_path)
        )
        return errors

    logger.debug(
        "Checking host table '%s' against module '%s' (%d module-level vars)",
        table.table_name, mod_name, len(sig.vars),
    )

    for mvar in table.variables():
        base = _base_local_name(mvar.local_name)
        fattrs = sig.vars.get(base)
        if fattrs is None:
            errors.append(
                "Host variable '{}' (standard_name '{}') declared in "
                "metadata table '{}' not found as a module-level "
                "declaration in Fortran module '{}'.".format(
                    mvar.local_name, mvar.standard_name,
                    table.table_name, mod_name,
                )
            )
            continue
        errors.extend(
            _check_arg_attributes(mod_name, base, mvar, fattrs)
        )
    return errors


def _validate_ddt_table(
    table,
    ddt_index: Dict[str, Dict[str, _ArgAttrs]],
    logger: logging.Logger,
) -> List[str]:
    """Validate a ``type = ddt`` metadata table against its Fortran type.

    The DDT name is ``table.table_name`` (the .meta convention: the table
    name is the Fortran type name).  The matching ``type :: X ... end
    type X`` block may live in any parsed module — looked up via the flat
    *ddt_index* built by :func:`_load_modules_tree`.  For each metadata
    component, match against the type block by base local-name and reuse
    :func:`_check_arg_attributes` for per-attr checks.

    Returns a list of error message strings (empty on full match).
    """
    errors: List[str] = []
    ddt_name = table.table_name.lower()
    comps = ddt_index.get(ddt_name)
    if comps is None:
        errors.append(
            "DDT '{}' (table in '{}') not found as a derived-type definition "
            "in any source file.".format(ddt_name, table.file_path)
        )
        return errors

    logger.debug(
        "Checking DDT table '%s' (%d components in Fortran)",
        ddt_name, len(comps),
    )

    for mvar in table.variables():
        base = _base_local_name(mvar.local_name)
        fattrs = comps.get(base)
        if fattrs is None:
            errors.append(
                "DDT component '{}' (standard_name '{}') declared in "
                "metadata table '{}' not found as a component of Fortran "
                "type '{}'.".format(
                    mvar.local_name, mvar.standard_name,
                    table.table_name, ddt_name,
                )
            )
            continue
        errors.extend(
            _check_arg_attributes(ddt_name, base, mvar, fattrs)
        )
    return errors


def _check_definition_character_lengths(table) -> List[str]:
    """Reject ``character ... kind = len=*`` in a definition-site table.

    Host and DDT metadata *define* the storage for their character
    variables (a host module variable / a derived-type component), so an
    assumed length (``len=*``) is illegal there: it is valid only on a
    dummy argument (a scheme arg, or a control/lifecycle variable) where
    the storage is supplied by the caller.  Apply this only to ``host`` /
    ``ddt`` tables -- ``control`` tables are exempt.  This mirrors the
    generator's ``build_flat_host_dict`` guard and is checked independently
    of the Fortran-vs-metadata comparison (a ``character(len=*)`` host decl
    is invalid Fortran in its own right and would never be found).

    Returns a list of error message strings (empty when none offend).
    """
    errors: List[str] = []
    for mvar in table.variables():
        if ((mvar.type or '').strip().lower() == 'character'
                and (mvar.kind or '').strip().lower() == 'len=*'):
            errors.append(
                "Character variable '{}' (standard_name '{}') in {} table "
                "'{}' declares kind='len=*'; host and DDT metadata must give "
                "character variables a concrete length (e.g. kind=len=512) "
                "-- assumed length is valid only for dummy arguments (scheme "
                "args and control/lifecycle variables).".format(
                    mvar.local_name, mvar.standard_name,
                    table.table_type, table.table_name,
                )
            )
    return errors


_FORTRAN_EXTENSIONS = ('.F90', '.f90', '.F', '.f')


def _fortran_file_for_table(table) -> Optional[str]:
    """Return the Fortran source path for a scheme *table* using ``source_path``.

    The convention is that the ``.F90`` file has the same base name as the
    ``.meta`` file but lives in the directory given by ``table.source_path``
    (which defaults to the ``.meta`` file's own directory when not set).

    Returns ``None`` if no matching file is found.

    Parameters
    ----------
    table : MetadataTable
        A scheme table with ``file_path`` and ``source_path`` set.

    Returns
    -------
    str or None
        Absolute path to the Fortran source file, or ``None``.

    Examples
    --------
    >>> from metadata.parse_tools import ParseContext
    >>> from metadata.metadata_table import MetadataTable
    >>> import tempfile, os
    >>> with tempfile.TemporaryDirectory() as d:
    ...     meta = os.path.join(d, 'foo.meta')
    ...     fort = os.path.join(d, 'foo.F90')
    ...     open(fort, 'w').close()
    ...     ctx = ParseContext(0, meta)
    ...     t = MetadataTable('foo', 'scheme', meta, ctx)
    ...     t.apply_table_props({})
    ...     os.path.basename(_fortran_file_for_table(t))
    'foo.F90'
    """
    meta_base = os.path.splitext(os.path.basename(table.file_path))[0]
    search_dir = table.source_path or os.path.dirname(os.path.abspath(table.file_path))
    for ext in _FORTRAN_EXTENSIONS:
        candidate = os.path.join(search_dir, meta_base + ext)
        if os.path.isfile(candidate):
            return candidate
    return None


def validate(
    scheme_files: List[str],
    source_files: Optional[List[str]] = None,
    host_files: Optional[List[str]] = None,
    logger: Optional[logging.Logger] = None,
) -> List[str]:
    """Validate scheme + host metadata against Fortran source files.

    Scheme tables in *scheme_files* are validated against the subroutine
    signatures in *source_files* (the existing scheme-side check).
    ``type = host`` and ``type = ddt`` tables in *host_files* are
    additionally validated against module-level decls and derived-type
    definitions in those same source files.  ``type = control`` tables
    are silent-skipped (no Fortran source backs control vars — they are
    framework-injected at the cap call sites).  A ``type = scheme`` table
    appearing in *host_files* is a hard error: schemes must be passed via
    *scheme_files* so the validator can find the per-phase subroutines.

    When *source_files* is ``None`` or empty, the validator resolves the
    Fortran source for each scheme / host / ddt table automatically using
    the ``source_path`` attribute from the metadata (defaulting to the
    ``.meta`` file's directory if ``source_path`` is absent).  Pass an
    explicit list to override.

    Parameters
    ----------
    scheme_files : list of str
        Paths to scheme ``.meta`` files.
    source_files : list of str, optional
        Explicit Fortran source files to scan.  If omitted, auto-discovered
        via ``source_path`` in the metadata.
    host_files : list of str, optional
        Paths to host ``.meta`` files (``type = host`` / ``type = ddt`` /
        ``type = control``).  Defaults to an empty list (scheme-only
        validation).
    logger : Logger, optional

    Returns
    -------
    list of str
        All validation error messages (empty means success).

    Raises
    ------
    CCPPError
        On metadata parse errors or missing files.
    """
    log = logger or _LOGGER

    scheme_files = list(scheme_files or [])
    host_files   = list(host_files or [])

    # At least one of --scheme-files / --host-files must be supplied;
    # otherwise the validator has nothing to do and would silently report
    # "Validation passed."  That was the old scheme-only behaviour with
    # host metadata accidentally passed in; the new contract requires
    # the caller to opt in to one side or the other (or both).
    if not scheme_files and not host_files:
        raise CCPPError(
            "ccpp_validator requires at least one of --scheme-files or "
            "--host-files; neither was supplied."
        )

    log.info("Loading scheme metadata from %d file(s)", len(scheme_files))
    scheme_tables = []
    for fpath in scheme_files:
        scheme_tables.extend(parse_metadata_file(fpath))

    # Reject host / control / suite tables passed via --scheme-files.
    # ``type = ddt`` IS allowed alongside scheme tables — schemes
    # routinely co-locate their own derived-type definitions in the
    # same .meta file (e.g. radiation schemes defining their internal
    # ty_rad_lw / ty_rad_sw DDTs).  Such DDTs go through the same
    # ``_validate_ddt_table`` pass as host-side DDTs.  Symmetric to
    # the rejection of scheme tables in --host-files (see below): each
    # CLI flag has a single, narrow responsibility so misclassified
    # host / control / suite .meta files fail fast with a clear pointer.
    scheme_nonscheme_violations = [
        t for t in scheme_tables
        if t.table_type in ('host', 'control', 'suite')
    ]
    if scheme_nonscheme_violations:
        details = sorted({
            "{} (type = {})".format(t.table_name, t.table_type)
            for t in scheme_nonscheme_violations
        })
        raise CCPPError(
            "Only type = scheme and type = ddt tables may appear in "
            "--scheme-files; host / control / suite tables must be "
            "passed via --host-files instead.  Offending tables: {}".format(
                details,
            )
        )

    scheme_store = SchemeStore.build_from(scheme_tables)
    log.info("Found %d scheme(s): %s", len(scheme_store.scheme_names()), scheme_store.scheme_names())

    # Collect DDT tables that travelled in via --scheme-files; they get
    # the same per-component validation as host-side DDTs.
    scheme_ddt_tables = [t for t in scheme_tables if t.table_type == 'ddt']

    host_tables = []
    if host_files:
        log.info("Loading host metadata from %d file(s)", len(host_files))
        for fpath in host_files:
            host_tables.extend(parse_metadata_file(fpath))

    # Reject scheme tables passed via --host-files: they wouldn't get
    # phase-aware validation, and silent acceptance would hide a real
    # mistake.  Fail fast before any per-table check runs.
    host_scheme_violations = [
        t for t in host_tables if t.is_scheme
    ]
    if host_scheme_violations:
        names = sorted({t.table_name for t in host_scheme_violations})
        raise CCPPError(
            "type = scheme tables may not appear in --host-files; pass "
            "them via --scheme-files instead.  Offending tables: {}".format(
                names,
            )
        )

    if source_files:
        log.info("Scanning %d explicit Fortran source file(s)", len(source_files))
        resolved_sources = list(source_files)
    else:
        # Auto-discover Fortran files via source_path in each scheme +
        # host / ddt table (control tables have no Fortran source).
        resolved_sources = []
        for tbl in scheme_tables:
            if tbl.table_type == 'control':
                continue
            fort = _fortran_file_for_table(tbl)
            if fort:
                resolved_sources.append(fort)
                log.debug("Resolved Fortran source for '%s': %s", tbl.table_name, fort)
            else:
                log.warning(
                    "No Fortran source found for %s '%s' (source_path='%s')",
                    tbl.table_type, tbl.table_name, tbl.source_path,
                )
        for tbl in host_tables:
            if tbl.table_type == 'control':
                continue
            fort = _fortran_file_for_table(tbl)
            if fort:
                resolved_sources.append(fort)
                log.debug(
                    "Resolved Fortran source for host/ddt table '%s': %s",
                    tbl.table_name, fort,
                )
            else:
                log.warning(
                    "No Fortran source found for %s table '%s' "
                    "(source_path='%s')",
                    tbl.table_type, tbl.table_name, tbl.source_path,
                )
    subroutine_tree = _load_source_tree(resolved_sources)
    log.info("Found %d subroutine definitions", len(subroutine_tree))

    modules_tree: Dict[str, _ModuleSig] = {}
    ddt_index: Dict[str, Dict[str, _ArgAttrs]] = {}
    # DDT validation needs the module/type parse whenever any DDT table is
    # in play — scheme-co-located DDTs count too.
    if host_tables or scheme_ddt_tables:
        modules_tree, ddt_index = _load_modules_tree(resolved_sources)
        log.info(
            "Found %d module(s) and %d derived-type definition(s)",
            len(modules_tree), len(ddt_index),
        )

    all_errors: List[str] = []
    for sname in scheme_store.scheme_names():
        all_errors.extend(
            _validate_scheme(sname, scheme_store, subroutine_tree, log)
        )
    # Scheme-co-located DDTs validate the same way as host-side DDTs;
    # their character components are definition sites too (a DDT component
    # may not be assumed-length), so the len=* guard applies.
    for tbl in scheme_ddt_tables:
        all_errors.extend(_check_definition_character_lengths(tbl))
        all_errors.extend(_validate_ddt_table(tbl, ddt_index, log))
    for tbl in host_tables:
        # Host and DDT tables define their character storage: reject len=*
        # regardless of the Fortran-comparison outcome.  Control tables are
        # exempt -- their character vars are pass-through dummy arguments.
        if tbl.table_type == 'host':
            all_errors.extend(_check_definition_character_lengths(tbl))
            all_errors.extend(_validate_host_table(tbl, modules_tree, log))
        elif tbl.table_type == 'ddt':
            all_errors.extend(_check_definition_character_lengths(tbl))
            all_errors.extend(_validate_ddt_table(tbl, ddt_index, log))
        elif tbl.table_type == 'control':
            log.info(
                "Skipping control table '%s' (no Fortran source backs "
                "control variables)", tbl.table_name,
            )
        # scheme tables were already rejected above; suite tables aren't
        # expected here but would be silent-skipped by omission.

    if all_errors:
        log.warning("%d validation error(s) found.", len(all_errors))
    else:
        log.info("Validation passed.")
    return all_errors


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------

def _build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        prog='ccpp_validator.py',
        description='Validate CCPP scheme Fortran source against metadata',
    )
    parser.add_argument(
        '--scheme-files',
        required=False,
        default='',
        metavar='FILE[,FILE...]',
        help=(
            'Comma-separated scheme metadata (.meta) files.  May contain '
            'type = scheme tables and type = ddt tables (schemes routinely '
            "co-locate their own DDTs in the same .meta file); host / "
            'control / suite tables are rejected (pass them via '
            '--host-files instead).  At least one of --scheme-files or '
            '--host-files must be supplied.'
        ),
    )
    parser.add_argument(
        '--host-files',
        required=False,
        default='',
        metavar='FILE[,FILE...]',
        help=(
            'Comma-separated host metadata (.meta) files.  '
            'type = host / type = ddt tables in these files are validated '
            'against module-level decls and derived-type definitions in '
            'the same --source-files.  type = control is silent-skipped '
            '(no Fortran source backs control vars).  type = scheme is '
            'rejected (pass via --scheme-files instead).'
        ),
    )
    parser.add_argument(
        '--source-files',
        required=True,
        metavar='FILE[,FILE...]',
        help='Comma-separated Fortran source (.F90) files',
    )
    parser.add_argument(
        '--verbose', '-v',
        action='count',
        default=0,
        help='Increase verbosity (use twice for DEBUG)',
    )
    # legacy-compat: transient migration shim (delete the argument,
    # the enable() call below, and the rest of the legacy_compat
    # touchpoints when the migration is complete).
    parser.add_argument(
        '--legacy-mode',
        action='store_true',
        help=(
            "TRANSIENT MIGRATION SHIM.  Accept legacy CCPP standard "
            "names (currently 'horizontal_loop_extent') in scheme "
            "metadata and silently rewrite them to their canonical "
            "capgen equivalents ('horizontal_dimension').  Emits a "
            "loud warning at startup.  Will be removed."
        ),
    )
    # NB: --gfs-dim-aliases is NOT exposed here.  That shim only takes
    # effect inside generator.suite_resolver._canonical_dim, which the
    # validator never invokes (the validator compares metadata against
    # Fortran source, not host metadata against scheme metadata), so
    # the flag would be a no-op.  See capgen/ccpp_capgen.py.
    # auto-clone-constituents: transient legacy shim.  This one DOES
    # belong on the validator because the shim extends the parser's
    # ``_KNOWN_ATTRS`` set — without the flag the validator rejects
    # the four legacy attrs (default_value/min_value/water_species/
    # mixing_ratio_type) with "Unknown variable attribute", which
    # blocks pre-flight validation runs.  The validator never builds
    # a host_dict, so the shim's single-instance host guard is not
    # called here (it's a no-op without a host metadata pass).
    parser.add_argument(
        '--legacy-auto-clone-constituents',
        action='store_true',
        help=(
            "TRANSIENT LEGACY SHIM.  Accept four legacy constituent "
            "attributes (default_value, min_value, water_species, "
            "mixing_ratio_type) on scheme args.  Mirrors the same "
            "flag on ccpp_capgen so legacy scheme metadata that "
            "needs auto-clone-static-constituent codegen can be "
            "validated against its Fortran source.  Emits a loud "
            "warning at startup.  Will be removed."
        ),
    )
    return parser


def main(argv: Optional[List[str]] = None) -> int:
    parser = _build_parser()
    args = parser.parse_args(argv)

    if args.verbose == 0:
        set_log_level(_LOGGER, logging.WARNING)
    elif args.verbose == 1:
        set_log_level(_LOGGER, logging.INFO)
    else:
        set_log_level(_LOGGER, logging.DEBUG)

    # legacy-compat: transient migration shim.  Emit the loud banner
    # before any parsing happens so user has fair warning.
    if args.legacy_mode:
        from metadata import legacy_compat
        legacy_compat.enable(_LOGGER)

    # auto-clone-constituents: transient legacy shim.  Emit the loud
    # banner before any parsing happens so user has fair warning.
    # The single-instance host guard is intentionally NOT invoked
    # here (no host metadata pass in the validator).
    if args.legacy_auto_clone_constituents:
        from metadata import auto_clone_constituents
        auto_clone_constituents.enable(_LOGGER)

    scheme_files = [f.strip() for f in args.scheme_files.split(',') if f.strip()]
    source_files = [f.strip() for f in args.source_files.split(',') if f.strip()]
    host_files   = [f.strip() for f in args.host_files.split(',') if f.strip()]

    try:
        errors = validate(scheme_files, source_files, host_files=host_files)
    except CCPPError as exc:
        _LOGGER.error("%s", exc)
        return 2
    except OSError as exc:
        _LOGGER.error("File error: %s", exc)
        return 2

    if errors:
        for err in errors:
            print("ERROR:", err, file=sys.stderr)
        return 1
    return 0


if __name__ == '__main__':
    sys.exit(main())
