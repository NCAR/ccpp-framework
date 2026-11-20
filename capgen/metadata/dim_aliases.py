"""TRANSIENT GFS-physics compatibility shim for equivalent dim names.

A handful of CCPP-physics scheme groups (notably GFS radiation and
GFS chemistry / aerosol composition) declare array dimensions using
standard names that are *physically* the vertical layer dimension but
spelled differently because the legacy code path carries the historical
name around for clarity (e.g. ``adjusted_vertical_layer_dimension_for_radiation``
on the radiation side, ``vertical_composition_dimension`` on the
composition side).

These names cannot simply be substituted for ``vertical_layer_dimension``
at parse time — each one is also exposed by some hosts (and consumed by
some schemes) as a standalone scalar control variable, so the
:mod:`metadata.legacy_compat` style of name rewriting would erase a
distinct variable.  Instead we want them treated as *equivalent only at
the point where two metadata dimension entries (host side and scheme
side) are compared for identity*.

This module provides an opt-in shim (``--gfs-dim-aliases`` on the
capgen / ccpp_validator CLI) that collapses each member of an alias
group to a single canonical representative when ``_canonical_dim``
prepares a dimension entry for the strict identity comparison in
:func:`generator.suite_resolver._check_compat`.  Every other consumer
keeps the original name verbatim.

This module is **deliberately self-contained** so the workaround can
be undone with a clean delete.  Removing the feature is:

1. Delete ``metadata/dim_aliases.py``
2. Delete ``unit-tests/test_dim_aliases.py``
3. ``grep -rn 'gfs-dim-aliases\\|dim_aliases\\|--gfs-dim-aliases' .`` and
   remove every remaining touchpoint (each is a 1-3 line snippet
   marked with a ``# dim-aliases:`` comment).

Every hook in the rest of the codebase is a no-op when the mode is
not enabled, so the shim has zero impact on default workflows.

Examples
--------
>>> from metadata import dim_aliases
>>> dim_aliases.is_enabled()
False
>>> dim_aliases.canonical('adjusted_vertical_layer_dimension_for_radiation')
'adjusted_vertical_layer_dimension_for_radiation'

When enabled, each alias collapses to its group representative:

>>> import io, logging
>>> logger = logging.getLogger('dim_aliases_doctest')
>>> dim_aliases.enable(logger, _stream=io.StringIO())
>>> dim_aliases.is_enabled()
True
>>> dim_aliases.canonical('adjusted_vertical_layer_dimension_for_radiation')
'vertical_layer_dimension'
>>> dim_aliases.canonical('vertical_composition_dimension')
'vertical_layer_dimension'
>>> dim_aliases.canonical('air_temperature')
'air_temperature'
>>> dim_aliases.disable()
>>> dim_aliases.is_enabled()
False
"""

from __future__ import annotations

import sys
from typing import Dict, Optional, TextIO


# ----------------------------------------------------------------------
# Alias-member -> canonical representative.
#
# Keep this list short and audited.  Each entry is a deliberate
# decision that a name is physically the same axis as the
# representative *for the purpose of host/scheme dim comparison*.
# The names remain distinct as standalone variables everywhere else.
# ----------------------------------------------------------------------
_DIM_ALIAS_MAP: Dict[str, str] = {
    # GFS radiation carries a separately-named "adjusted" vertical
    # layer dimension that, in practice, is the same axis as
    # vertical_layer_dimension.  Collapse for the per-position dim
    # identity check only.
    'adjusted_vertical_layer_dimension_for_radiation':
        'vertical_layer_dimension',
    # GFS chemistry / aerosol composition uses
    # vertical_composition_dimension where the layer count is meant.
    'vertical_composition_dimension':
        'vertical_layer_dimension',
}


# Process-level on/off flag.  Module state is intentional: a single
# CLI invocation is the natural unit, and threading the flag through
# every parse call would bloat the API.  Tests must use the
# ``disable()`` helper to restore the default between cases.
_ENABLED: bool = False


def enable(logger=None, _stream: Optional[TextIO] = None) -> None:
    """Turn the GFS dim-aliases shim on and emit a bold warning banner.

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
    _content_width = border_width - len('*** ') - len(' ***')

    def _pad(s: str) -> str:
        """Format *s* as a banner row, left-padded to the border width."""
        return '*** {:<{w}} ***'.format(s[:_content_width], w=_content_width)

    banner_lines = ['', border, _pad('WARNING: GFS DIM-ALIASES ENABLED'),
                    _pad('')]
    banner_lines += [
        _pad('The following dimension standard names will be'),
        _pad('treated as equivalent to their canonical axis ONLY'),
        _pad('during host/scheme dim-position comparison:'),
        _pad(''),
    ]
    for alias, canon in sorted(_DIM_ALIAS_MAP.items()):
        banner_lines.append(_pad("  '{}'  =>  '{}'".format(alias, canon)))
    banner_lines += [
        _pad(''),
        _pad('Variables keep their original names everywhere'),
        _pad('else.  This is a TRANSIENT GFS-physics shim and'),
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
            "'{}' => '{}'".format(alias, canon)
            for alias, canon in sorted(_DIM_ALIAS_MAP.items())
        )
        logger.warning(
            "GFS dim-aliases enabled: the following dimension names "
            "will compare equal to their canonical axis: %s. This "
            "shim is transient and will be removed.",
            pair_str,
        )


def disable() -> None:
    """Turn the dim-aliases shim off.  Intended for tests and library
    users that wrap a generator invocation."""
    global _ENABLED
    _ENABLED = False


def is_enabled() -> bool:
    """Return ``True`` iff the dim-aliases shim is enabled in this
    process."""
    return _ENABLED


def canonical(name: str) -> str:
    """Return the canonical representative for *name*, or *name*
    unchanged.

    When the shim is **disabled** this is a strict identity — the
    aliased names compare distinct, just as they would in a default
    capgen workflow.  When the shim is **enabled** every entry in
    :data:`_DIM_ALIAS_MAP` collapses to its representative; everything
    else passes through unchanged.

    Intended for use by :func:`generator.suite_resolver._canonical_dim`
    on the *upper bound* of a dimension entry; no other call site
    should consult this function.
    """
    if not _ENABLED:
        return name
    return _DIM_ALIAS_MAP.get(name, name)
