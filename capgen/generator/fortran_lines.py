"""Line-length wrapping for generated Fortran.

Free-form Fortran ends at column 132 (``FREE_FORM_MAX_COLUMNS``) and capgen
holds the Fortran it *reads* to that limit (``doc/followups.md`` FU-038).  It
has to hold the Fortran it *writes* to a limit too, and until this module
existed it did not: the generated caps carried 58 over-long lines and the
end-to-end harness compiled them only because it passed gfortran's
``-ffree-line-length-none`` -- the very ambiguity the input rule removes.

The limit used here is the stricter :data:`GENERATED_MAX_COLUMNS`, 120, which
is the ``ColumnLimit`` in this repository's ``.codee-format``.  Nothing else
of that format is implemented: matching it in full is a separate project and
waits on a revisit of the Codee style for ccpp-framework (FU-005).  Wrapping
to its column limit now simply keeps the caps from being the thing such a
pass would have to reflow.

Every cap writer goes through :func:`write_fortran` rather than calling
``open_if_changed`` itself.  :func:`wrap_fortran_lines` is a pure pass over
the finished line list, so a line already within the limit is emitted
unchanged and wrapping shows up in generated output only where a line would
have overrun.

Four ways to break a line, tried in this order:

1. A logical ``if (<cond>) <stmt>`` breaks after the condition -- the one
   structural break the statement has.  Tried first so the action's commas
   are not packed onto the condition's line.
2. ``use <module>, only: <symbols>`` breaks after ``only:``, one symbol per
   continuation line.  This is the form ``host_cap`` already emits for its
   own long import lists, and the one ``.codee-format`` asks for.
3. Any other statement breaks after the commas that lie outside a character
   literal, packing as many items per line as fit.  A comma break is legal
   at any parenthesis depth, so one over-long actual argument wraps the
   same way a long argument list does.
4. A piece with no comma to break on but with a character literal across
   the limit -- a generated ``errmsg`` is the case that matters -- continues
   the literal itself: the line ends with ``&`` and the next resumes with
   ``&`` immediately before the rest of the text.  Blanks *before* that
   leading ``&`` are not part of the literal, so the message is unchanged.

A trailing comment moves onto its own line above the statement: ``&`` inside
a comment is just another comment character, so a comment cannot be
continued.

A line none of that can break is a generator bug rather than something to
emit and hope about, so :func:`wrap_fortran_lines` raises ``CCPPError``
naming the file and what it could not break.
"""

import logging
import re
from typing import List, Optional, Sequence, Tuple

from metadata.parse_tools import CCPPError, open_if_changed

# What capgen wraps its *own* output to.  Free-form Fortran ends at 132
# (``FREE_FORM_MAX_COLUMNS``, which is what the validator checks on input),
# but `.codee-format` sets ``ColumnLimit: 120`` for this repository, so
# generated code is held to the stricter of the two: a cap that is already
# inside 120 columns cannot be the thing a Codee pass has to reflow.  The
# rest of that format is a separate project -- see ``doc/followups.md``
# FU-005.
GENERATED_MAX_COLUMNS = 120

# Continuation indent, in spaces added to the statement's own indent.  An
# import list lines up two in, matching the host cap's existing long-import
# form; everything else lines up four in, matching the generated call sites.
_USE_CONT_INDENT = 2
_CODE_CONT_INDENT = 4

# ``, only:`` with any spacing.  Resolved with the *last* match in the
# statement so ``use, intrinsic :: <mod>, only: ...`` splits at the import
# list and not at the ``intrinsic`` attribute.
_ONLY_RE = re.compile(r',[ \t]*only[ \t]*:[ \t]*', re.IGNORECASE)
_USE_RE = re.compile(r'[ \t]*use\b', re.IGNORECASE)

# The opening parenthesis of a logical ``if (<cond>) <stmt>``.
_IF_RE = re.compile(r'[ \t]*if[ \t]*\(', re.IGNORECASE)


def wrap_fortran_lines(
    lines: Sequence[str],
    path: Optional[str] = None,
    limit: int = GENERATED_MAX_COLUMNS,
) -> List[str]:
    """Return *lines* with every over-long line broken into continuations.

    Parameters
    ----------
    lines : sequence of str
        Generated Fortran source lines, without trailing newlines.
    path : str, optional
        Output path, used only to name the file in an error message.
    limit : int
        Column limit; defaults to :data:`GENERATED_MAX_COLUMNS`.

    Returns
    -------
    list of str
        The same lines, with any line past *limit* replaced by the
        continuation block that carries the same statement.

    Raises
    ------
    CCPPError
        If a line is past *limit* and holds no comma and no character
        literal to break at.

    Examples
    --------
    A line within the limit is returned untouched:

    >>> wrap_fortran_lines(['  x = 1'])
    ['  x = 1']

    An import list breaks after ``only:``, one symbol per line:

    >>> for line in wrap_fortran_lines(
    ...     ['  use m, only: alpha, beta, gamma'], limit=30
    ... ):
    ...     print(repr(line))
    '  use m, only: &'
    '    alpha, &'
    '    beta, &'
    '    gamma'

    Anything else packs as many comma-separated items per line as fit:

    >>> for line in wrap_fortran_lines(
    ...     ['    call sub(alpha, beta, gamma, delta)'], limit=30
    ... ):
    ...     print(repr(line))
    '    call sub(alpha, beta, &'
    '        gamma, delta)'
    """
    out: List[str] = []
    for line in lines:
        if len(line) <= limit:
            out.append(line)
        else:
            out.extend(_wrap_one(line, limit, path))
    return out


def write_fortran(
    out_path: str,
    lines: Sequence[str],
    logger: Optional[logging.Logger] = None,
) -> str:
    """Wrap *lines* to the free-form limit and write them to *out_path*.

    The write itself is :func:`~metadata.parse_tools.open_if_changed`, so an
    unchanged file keeps its mtime.

    Parameters
    ----------
    out_path : str
        Path of the generated Fortran file.
    lines : sequence of str
        Source lines, without trailing newlines.
    logger : logging.Logger, optional
        Passed through to the write helper.

    Returns
    -------
    str
        *out_path*, for the caller to return on.
    """
    wrapped = wrap_fortran_lines(lines, path=out_path)
    with open_if_changed(out_path, logger=logger) as fh:
        fh.write('\n'.join(wrapped) + '\n')
    return out_path


########################################################################
# Internals
########################################################################


def _wrap_one(line: str, limit: int, path: Optional[str]) -> List[str]:
    """Return the continuation block for one over-long *line*."""
    indent = line[:len(line) - len(line.lstrip())]
    code, comment = _split_comment(line)
    code = code.rstrip()
    # A statement that was already a continuation keeps its ``&``: strip it
    # for the wrapping and put it back on the final line.
    trailing_amp = code.endswith('&')
    if trailing_amp:
        code = code[:-1].rstrip()

    out: List[str] = []
    if comment:
        out.extend(_wrap_comment(indent, comment, limit))
    if not code:
        return out
    out.extend(_wrap_code(indent, code, limit, trailing_amp, path))
    return out


def _split_comment(line: str) -> Tuple[str, str]:
    """Split *line* at the first ``!`` outside a character literal.

    Returns ``(code, comment)``; *comment* keeps its ``!`` and is empty
    when the line carries no comment.

    Examples
    --------
    >>> _split_comment("  x = 1  ! why")
    ('  x = 1  ', '! why')
    >>> _split_comment("  msg = 'not ! a comment'")
    ("  msg = 'not ! a comment'", '')
    """
    quote = None
    idx = 0
    while idx < len(line):
        char = line[idx]
        if quote is not None:
            if char == quote:
                if line[idx + 1:idx + 2] == quote:
                    idx += 2        # doubled delimiter inside the literal
                    continue
                quote = None
        elif char in ("'", '"'):
            quote = char
        elif char == '!':
            return line[:idx], line[idx:]
        idx += 1
    return line, ''


def _wrap_comment(indent: str, comment: str, limit: int) -> List[str]:
    """Return *comment* as one or more full-line comments at *indent*."""
    words = comment.lstrip('!').split()
    lines: List[str] = []
    current = ''
    for word in words:
        candidate = '{} {}'.format(current, word) if current else word
        if current and len(indent) + 2 + len(candidate) > limit:
            lines.append('{}! {}'.format(indent, current))
            current = word
        else:
            current = candidate
    lines.append('{}! {}'.format(indent, current) if current
                 else '{}!'.format(indent))
    return lines


def _wrap_code(
    indent: str,
    code: str,
    limit: int,
    trailing_amp: bool,
    path: Optional[str],
) -> List[str]:
    """Return the finished continuation lines for one statement."""
    # A logical ``if`` breaks after its condition, which is the one
    # structural break such a statement has.  Tried first, because the
    # action half often carries commas that would otherwise be packed into
    # the condition's line.  Skipped when the condition alone is the part
    # that does not fit, so the comma path can break inside it.
    logical_if = _logical_if_split(code)
    if logical_if is not None and len(logical_if[0]) + 2 <= limit:
        condition, action = logical_if
        cont = indent + ' ' * _CODE_CONT_INDENT
        rows = [(condition, False)]
        rows.extend(_pack(cont, _comma_chunks(cont + action), limit - 2))
        rows = _split_literals(rows, cont, limit, path, code)
        return _add_continuations(rows, trailing_amp)

    only = _ONLY_RE.search(code)
    if _USE_RE.match(code) and only is not None:
        cont = indent + ' ' * _USE_CONT_INDENT
        head = code[:only.end()].rstrip()
        rows = [(head, False)]
        rows.extend((cont + chunk.rstrip(), False)
                    for chunk in _comma_chunks(code[only.end():]))
    else:
        cont = indent + ' ' * _CODE_CONT_INDENT
        rows = _pack(cont, _comma_chunks(code), limit - 2)

    rows = _split_literals(rows, cont, limit, path, code)
    return _add_continuations(rows, trailing_amp)


def _logical_if_split(code: str) -> Optional[Tuple[str, str]]:
    """Split ``if (<cond>) <stmt>`` into its condition and its action.

    Returns ``None`` for anything else, including a block ``if ... then``,
    whose body is already on its own lines.

    Examples
    --------
    >>> _logical_if_split('  if (allocated(x)) deallocate(x)')
    ('  if (allocated(x))', 'deallocate(x)')
    >>> _logical_if_split('  if (x > 1) then') is None
    True
    >>> _logical_if_split('  call sub(a)') is None
    True
    """
    opening = _IF_RE.match(code)
    if opening is None:
        return None
    closing = _matching_paren(code, opening.end() - 1)
    if closing is None:
        return None
    action = code[closing + 1:].strip()
    if not action or action.lower() == 'then' or action.lower().startswith('then '):
        return None
    return code[:closing + 1], action


def _matching_paren(code: str, opening: int) -> Optional[int]:
    """Return the index closing the parenthesis at *opening*, or ``None``."""
    depth = 0
    quote = None
    idx = opening
    while idx < len(code):
        char = code[idx]
        if quote is not None:
            if char == quote:
                if code[idx + 1:idx + 2] == quote:
                    idx += 2
                    continue
                quote = None
        elif char in ("'", '"'):
            quote = char
        elif char == '(':
            depth += 1
        elif char == ')':
            depth -= 1
            if depth == 0:
                return idx
        idx += 1
    return None


def _comma_chunks(code: str) -> List[str]:
    """Split *code* after every comma outside a character literal.

    Each chunk keeps its comma and the blanks that followed it, so
    concatenating the chunks reproduces *code* exactly.

    Examples
    --------
    >>> _comma_chunks('alpha, beta, gamma')
    ['alpha, ', 'beta, ', 'gamma']
    >>> _comma_chunks("f(a, 'x,y')")
    ['f(a, ', "'x,y')"]
    """
    chunks: List[str] = []
    start = 0
    quote = None
    idx = 0
    while idx < len(code):
        char = code[idx]
        if quote is not None:
            if char == quote:
                if code[idx + 1:idx + 2] == quote:
                    idx += 2
                    continue
                quote = None
        elif char in ("'", '"'):
            quote = char
        elif char == ',':
            end = idx + 1
            while end < len(code) and code[end] in ' \t':
                end += 1
            chunks.append(code[start:end])
            start = end
            idx = end
            continue
        idx += 1
    if start < len(code):
        chunks.append(code[start:])
    return chunks


def _pack(
    cont: str,
    chunks: Sequence[str],
    budget: int,
) -> List[Tuple[str, bool]]:
    """Greedily fill lines of at most *budget* columns with *chunks*.

    The first chunk carries the statement's own indent, so only the
    continuation rows are prefixed with *cont*.
    """
    rows: List[Tuple[str, bool]] = []
    current = ''
    placed = False
    for chunk in chunks:
        if placed and len((current + chunk).rstrip()) > budget:
            rows.append((current.rstrip(), False))
            current = cont + chunk
        else:
            current = current + chunk
        placed = True
    rows.append((current.rstrip(), False))
    return rows


def _split_literals(
    rows: Sequence[Tuple[str, bool]],
    cont: str,
    limit: int,
    path: Optional[str],
    statement: str,
) -> List[Tuple[str, bool]]:
    """Break any row still past the limit inside its character literal."""
    out: List[Tuple[str, bool]] = []
    for text, tight in rows:
        # A row ending in a tight ``&`` needs one column for it; one that
        # takes a ``' &'`` needs two.  Charge two throughout: it is the
        # safe side and costs at most one column of fill.
        if len(text) <= limit - 2:
            out.append((text, tight))
        else:
            out.extend(
                _literal_rows(text, cont, limit, path, statement, tight)
            )
    return out


def _literal_rows(
    text: str,
    cont: str,
    limit: int,
    path: Optional[str],
    statement: str,
    tight: bool,
) -> List[Tuple[str, bool]]:
    """Return the rows for *text*, continuing the literal that overruns.

    The literal straddling the limit is cut into as many pieces as it takes.
    Every piece but the last ends with a tight ``&`` and the next opens with
    ``&``, so the literal resumes with the character right after it.
    """
    quote, open_at = _literal_at(text, limit - 2)
    if quote is None:
        raise CCPPError(_unbreakable(path, statement, limit))
    close_at = _literal_end(text, open_at, quote)
    lead = text[:open_at + 1]                 # through the opening delimiter
    content = text[open_at + 1:close_at]      # doubled delimiters intact
    trailer = text[close_at:]                 # closing delimiter and the rest

    rows: List[Tuple[str, bool]] = []
    while True:
        if len(lead) + len(content) + len(trailer) <= limit - 2:
            rows.append((lead + content + trailer, tight))
            return rows
        # One column goes to the tight ``&`` that continues the literal.
        room = limit - 1 - len(lead)
        cut = _atom_boundary(content, min(room, len(content) - 1), quote)
        if cut < 1:
            raise CCPPError(_unbreakable(path, statement, limit))
        rows.append((lead + content[:cut], True))
        content = content[cut:]
        lead = cont + '&'


def _literal_end(text: str, open_at: int, quote: str) -> int:
    """Return the index of the delimiter closing the literal at *open_at*."""
    idx = open_at + 1
    while idx < len(text):
        if text[idx] == quote:
            if text[idx + 1:idx + 2] == quote:
                idx += 2
                continue
            return idx
        idx += 1
    return len(text)


def _literal_at(text: str, column: int) -> Tuple[Optional[str], int]:
    """Return the delimiter and start index of the literal open at *column*.

    ``(None, -1)`` when *column* is not inside a character literal.
    """
    quote = None
    open_at = -1
    idx = 0
    while idx < len(text):
        char = text[idx]
        if quote is not None:
            if char == quote:
                if text[idx + 1:idx + 2] == quote:
                    idx += 2
                    continue
                if idx >= column:
                    return quote, open_at
                quote = None
                open_at = -1
        elif char in ("'", '"'):
            quote = char
            open_at = idx
        if quote is not None and idx >= column:
            return quote, open_at
        idx += 1
    return (quote, open_at) if quote is not None else (None, -1)


def _atom_boundary(content: str, cut: int, quote: str) -> int:
    """Return the largest safe split index in *content* at or below *cut*.

    A doubled delimiter (``''`` inside a ``'``-quoted literal) is one
    character of content and must not be split down the middle.  A split
    straight after a blank is preferred, so a continued message breaks
    between words rather than mid-word.

    Examples
    --------
    >>> _atom_boundary('one two three', 9, "'")
    8
    >>> _atom_boundary("a''b", 2, "'")
    1
    """
    stops: List[int] = []
    idx = 0
    while idx < cut:
        if content[idx] == quote and content[idx + 1:idx + 2] == quote:
            idx += 2
        else:
            idx += 1
        if idx <= cut:
            stops.append(idx)
    if not stops:
        return 0
    for stop in reversed(stops):
        if content[stop - 1] == ' ':
            return stop
    return stops[-1]


def _add_continuations(
    rows: Sequence[Tuple[str, bool]],
    trailing_amp: bool,
) -> List[str]:
    """Place the continuation markers on a finished set of rows."""
    out: List[str] = []
    for index, (text, tight) in enumerate(rows):
        last = index == len(rows) - 1
        if tight:
            # The literal is still open: the ``&`` has to hug the text,
            # because a blank before it would join the literal.
            out.append(text if text.endswith('&') else text + '&')
        elif last and not trailing_amp:
            out.append(text)
        else:
            out.append(text + ' &')
    return out


def _unbreakable(path: Optional[str], statement: str, limit: int) -> str:
    """Return the error message for a statement that cannot be wrapped."""
    where = ' in {}'.format(path) if path else ''
    return (
        "Generated Fortran line{} is longer than {} columns and has no "
        "comma, logical-if condition or character literal to continue at; "
        "the emitter has to break it:\n  {}".format(where, limit, statement)
    )
