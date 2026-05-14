#!/usr/bin/env python3

"""ccpp_validator — validate Fortran source files against CCPP scheme metadata.

For each scheme phase declared in a ``.meta`` file this tool checks that the
corresponding Fortran subroutine:

1. **Exists** in the Fortran source tree.
2. Has the **same number of dummy arguments** as declared in the metadata.
3. The dummy-argument **names match** the ``local_name`` values in the metadata
   (order-insensitive).

The tool does *not* parse full Fortran type declarations — that level of
verification is intentionally kept out of the code generator path (see design
doc: toolchain structure).

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
from typing import Dict, List, NamedTuple, Optional, Set

# Ensure the capgen-ng package is importable when invoked directly.
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
    """
    args: List[str]
    optional: Set[str]


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


def _line_optional_names(line: str) -> List[str]:
    """Return lowercase var names from a type-decl line carrying ``optional``.

    Matches Fortran lines of the form
    ``<type-spec> [, <attr>...] :: <var>[, <var>...]`` where one of the
    comma-separated attributes (paren-aware) is the bare token
    ``optional``.  Returns an empty list when ``::`` is absent or when no
    ``optional`` attribute is present.

    Examples
    --------
    >>> _line_optional_names('integer, optional, intent(in) :: innie')
    ['innie']
    >>> _line_optional_names('real, intent(out), optional :: outie')
    ['outie']
    >>> _line_optional_names('real(kind=kind_phys), optional :: x, y(:,:)')
    ['x', 'y']
    >>> _line_optional_names('integer :: not_optional')
    []
    >>> _line_optional_names('  ! a comment, optional :: not_a_decl')
    []
    """
    if '::' not in line:
        return []
    before, _, after = line.partition('::')
    attrs = [a.strip().lower() for a in _paren_aware_split(before, ',')]
    if 'optional' not in attrs:
        return []
    names: List[str] = []
    for tok in _paren_aware_split(after, ','):
        m = re.match(r'\s*(\w+)', tok)
        if m:
            names.append(m.group(1).lower())
    return names


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
    ['  foo  bar   )', '  baz']
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
    """
    logical = _join_continuation(
        source.splitlines(keepends=True), filename=filename,
    )
    args_by_name: Dict[str, List[str]] = {}
    optional_by_name: Dict[str, Set[str]] = {}
    # Stack of names whose body we are currently scanning.  Each entry is
    # the recorded name (for which we collect optionals) or ``None`` when
    # this is a duplicate-name sub whose body should be skipped for the
    # purpose of optional-attribution (its args were already discarded).
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
            for n in _line_optional_names(line):
                if n in arg_set:
                    optional_by_name[tracked].add(n)

    return {
        name: _SubSig(args=args_by_name[name],
                      optional=optional_by_name[name])
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
    logger: Optional[logging.Logger] = None,
) -> List[str]:
    """Validate scheme metadata against Fortran source files.

    When *source_files* is ``None`` or empty, the validator resolves the
    Fortran source for each scheme automatically using the ``source_path``
    attribute from the metadata (defaulting to the ``.meta`` file's directory
    if ``source_path`` is absent).  Pass an explicit list to override.

    Parameters
    ----------
    scheme_files : list of str
        Paths to scheme ``.meta`` files.
    source_files : list of str, optional
        Explicit Fortran source files to scan.  If omitted, auto-discovered
        via ``source_path`` in the metadata.
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

    log.info("Loading scheme metadata from %d file(s)", len(scheme_files))
    all_tables = []
    for fpath in scheme_files:
        all_tables.extend(parse_metadata_file(fpath))
    scheme_store = SchemeStore.build_from(all_tables)
    log.info("Found %d scheme(s): %s", len(scheme_store.scheme_names()), scheme_store.scheme_names())

    if source_files:
        log.info("Scanning %d explicit Fortran source file(s)", len(source_files))
        resolved_sources = list(source_files)
    else:
        # Auto-discover Fortran files via source_path in each scheme table.
        resolved_sources = []
        for tbl in all_tables:
            if not tbl.is_scheme:
                continue
            fort = _fortran_file_for_table(tbl)
            if fort:
                resolved_sources.append(fort)
                log.debug("Resolved Fortran source for '%s': %s", tbl.table_name, fort)
            else:
                log.warning(
                    "No Fortran source found for scheme '%s' (source_path='%s')",
                    tbl.table_name, tbl.source_path,
                )
    subroutine_tree = _load_source_tree(resolved_sources)
    log.info("Found %d subroutine definitions", len(subroutine_tree))

    all_errors: List[str] = []
    for sname in scheme_store.scheme_names():
        errs = _validate_scheme(sname, scheme_store, subroutine_tree, log)
        all_errors.extend(errs)

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
        required=True,
        metavar='FILE[,FILE...]',
        help='Comma-separated scheme metadata (.meta) files',
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
            "capgen-ng equivalents ('horizontal_dimension').  Emits a "
            "loud warning at startup.  Will be removed."
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

    scheme_files = [f.strip() for f in args.scheme_files.split(',') if f.strip()]
    source_files = [f.strip() for f in args.source_files.split(',') if f.strip()]

    try:
        errors = validate(scheme_files, source_files)
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
