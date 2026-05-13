"""TRANSIENT compatibility shim for legacy CCPP standard names.

The original ccpp-prebuild + ccpp-capgen toolchain used the standard
name ``horizontal_loop_extent`` where capgen-ng uses
``horizontal_dimension``.  This module provides an opt-in shim
(``--legacy-mode`` on the capgen-ng / ccpp_validator CLI) that
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
    # in scheme metadata where capgen-ng uses ``horizontal_dimension``.
    'horizontal_loop_extent': 'horizontal_dimension',
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
    banner_lines = [
        '',
        '*' * 70,
        '*** WARNING: LEGACY-MODE ENABLED                                   ***',
        '***                                                                ***',
        '*** Scheme metadata using the deprecated standard name             ***',
        "***   'horizontal_loop_extent'                                     ***",
        '*** will be silently rewritten to                                  ***',
        "***   'horizontal_dimension'                                       ***",
        '*** at parse time.                                                 ***',
        '***                                                                ***',
        '*** This is a TRANSIENT migration shim. Update your scheme         ***',
        '*** metadata to use the canonical name; legacy mode WILL BE        ***',
        '*** REMOVED in a future capgen-ng release.                         ***',
        '*' * 70,
        '',
    ]
    stream.write('\n'.join(banner_lines) + '\n')
    try:
        stream.flush()
    except Exception:  # pylint: disable=broad-except
        pass

    if logger is not None:
        logger.warning(
            "Legacy mode enabled: 'horizontal_loop_extent' will be "
            "rewritten to 'horizontal_dimension' in scheme metadata. "
            "This shim is transient and will be removed."
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
    map keys are already lowercase to match capgen-ng's
    case-insensitive standard-name convention.
    """
    if not _ENABLED:
        return name
    return _LEGACY_NAME_MAP.get(name, name)
