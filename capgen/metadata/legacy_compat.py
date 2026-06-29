"""TRANSIENT compatibility shim for legacy CCPP standard names.

The original ccpp-prebuild + ccpp-capgen toolchain used the standard
name ``horizontal_loop_extent`` where capgen uses
``horizontal_dimension``.  This module provides an opt-in shim
(``--legacy-mode`` on the capgen / ccpp_validator CLI) that
silently rewrites legacy names to their canonical equivalents at
metadata parse time so the rest of the toolchain only ever sees the
canonical names.

This module is **deliberately self-contained** so the migration can
be undone with a clean delete.  Removing the feature is:

1. Delete ``metadata/legacy_compat.py``
2. Delete ``tests/test_legacy_compat.py``
3. ``grep -rn 'legacy-compat\\|legacy_compat\\|--legacy-mode' .`` and
   remove every remaining touchpoint (each is a 1-3 line snippet
   marked with a ``# legacy-compat:`` comment).

Every hook in the rest of the codebase is a no-op when the mode is
not enabled, so the shim has zero impact on non-legacy workflows.

Examples
--------
>>> from metadata import legacy_compat
>>> legacy_compat.is_enabled()
False
>>> legacy_compat.translate('horizontal_loop_extent')
'horizontal_loop_extent'
>>> legacy_compat.translate('air_temperature')
'air_temperature'

When enabled, legacy names are rewritten:

>>> import io, logging
>>> logger = logging.getLogger('legacy_compat_doctest')
>>> legacy_compat.enable(logger, _stream=io.StringIO())
>>> legacy_compat.is_enabled()
True
>>> legacy_compat.translate('horizontal_loop_extent')
'horizontal_dimension'
>>> legacy_compat.translate('air_temperature')
'air_temperature'
>>> legacy_compat.disable()
>>> legacy_compat.is_enabled()
False
"""

from __future__ import annotations

import sys
from typing import Dict, Optional, TextIO


# ----------------------------------------------------------------------
# Legacy → canonical name map.
#
# Keep this short and audited.  Every entry is a deliberate decision
# that a legacy name has a single, unambiguous canonical replacement.
# ----------------------------------------------------------------------
_LEGACY_NAME_MAP: Dict[str, str] = {
    # ccpp-prebuild / original ccpp-capgen used ``horizontal_loop_extent``
    # in scheme metadata where capgen uses ``horizontal_dimension``.
    'horizontal_loop_extent': 'horizontal_dimension',

    # Legacy CCPP-physics hosts (and SCM 17p8 in particular) sized
    # per-thread DDT containers by ``number_of_openmp_threads``; the
    # capgen convention is ``number_of_threads`` (matching the
    # ``thread_number`` control variable name).  Aliasing here lets the
    # host metadata flow through unchanged; once hosts have migrated,
    # drop this entry.
    'number_of_openmp_threads': 'number_of_threads',
}


# Process-level on/off flag.  Module state is intentional: a single
# CLI invocation is the natural unit, and threading the flag through
# every parse call would bloat the API.  Tests must use the
# ``disable()`` helper (or the ``legacy_mode_disabled`` context
# manager) to restore the default between cases.
_ENABLED: bool = False


def enable(logger=None, _stream: Optional[TextIO] = None) -> None:
    """Turn legacy mode on and emit a single bold warning banner.

    The warning goes to *_stream* (defaults to ``sys.stderr``) and is
    also logged at WARNING level on *logger* (if supplied) so that
    downstream consumers of the logger see it.

    Idempotent: a second call is a no-op (no double warning).

    Parameters
    ----------
    logger : logging.Logger, optional
        Logger to emit a ``WARNING``-level companion message on.
    _stream : file-like, optional
        Override for the banner destination (used by tests to capture
        output).  ``None`` (default) means ``sys.stderr``.
    """
    global _ENABLED
    if _ENABLED:
        return
    _ENABLED = True

    stream = _stream if _stream is not None else sys.stderr
    border_width = 70
    border = '*' * border_width
    # Content width between the leading ``*** `` and trailing ` ***``.
    _content_width = border_width - len('*** ') - len(' ***')

    def _pad(s: str) -> str:
        """Format *s* as a banner row, left-padded to the border width."""
        return '*** {:<{w}} ***'.format(s[:_content_width], w=_content_width)

    banner_lines = ['', border, _pad('WARNING: LEGACY-MODE ENABLED'),
                    _pad('')]
    if len(_LEGACY_NAME_MAP) == 1:
        # Singular phrasing reads better when there's only one pair.
        old, new = next(iter(_LEGACY_NAME_MAP.items()))
        banner_lines += [
            _pad('Metadata using the deprecated standard name'),
            _pad("  '{}'".format(old)),
            _pad('will be silently rewritten to'),
            _pad("  '{}'".format(new)),
            _pad('at parse time.'),
        ]
    else:
        banner_lines += [
            _pad('Metadata using any of these deprecated standard names'),
            _pad('will be silently rewritten at parse time:'),
            _pad(''),
        ]
        for old, new in sorted(_LEGACY_NAME_MAP.items()):
            banner_lines.append(_pad("  '{}'  ->  '{}'".format(old, new)))
    banner_lines += [
        _pad(''),
        _pad('This is a TRANSIENT migration shim. Update your'),
        _pad('metadata to use the canonical names; legacy mode'),
        _pad('WILL BE REMOVED in a future capgen release.'),
        border,
        '',
    ]
    stream.write('\n'.join(banner_lines) + '\n')
    try:
        stream.flush()
    except Exception:  # pylint: disable=broad-except
        pass

    if logger is not None:
        pair_str = ', '.join(
            "'{}' -> '{}'".format(old, new)
            for old, new in sorted(_LEGACY_NAME_MAP.items())
        )
        logger.warning(
            "Legacy mode enabled: the following deprecated standard "
            "names will be rewritten in metadata at parse time: %s. "
            "This shim is transient and will be removed.",
            pair_str,
        )


def disable() -> None:
    """Turn legacy mode off.  Intended for tests and library users that
    wrap a generator invocation."""
    global _ENABLED
    _ENABLED = False


def is_enabled() -> bool:
    """Return ``True`` iff legacy mode has been enabled in this process."""
    return _ENABLED


def translate(name: str) -> str:
    """Return the canonical replacement for *name*, or *name* unchanged.

    When legacy mode is **disabled** this is a strict identity — even
    a legacy name like ``horizontal_loop_extent`` is returned as-is so
    downstream parsers reject it just as they would in non-legacy
    workflows.  When legacy mode is **enabled** the entries in
    :data:`_LEGACY_NAME_MAP` are rewritten and everything else passes
    through unchanged.

    The function is tolerant of any input that lookup-by-string is
    valid for (``str``).  Callers may pre-lowercase the input — the
    map keys are already lowercase to match capgen's
    case-insensitive standard-name convention.
    """
    if not _ENABLED:
        return name
    return _LEGACY_NAME_MAP.get(name, name)
