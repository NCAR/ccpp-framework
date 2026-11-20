"""TRANSIENT shim — original-capgen auto-clone-static-constituent path.

The legacy ccpp-prebuild / original-capgen toolchain auto-registered
every ``is_constituent`` scheme arg by lifting its metadata properties
(``long_name``, ``diagnostic_name``, ``units``, ``default_value``, …)
into a synthetic ``%instantiate(...)`` call emitted into the generated
host cap.  Capgen deliberately dropped that path in favour of
explicit registration (``host_constituents`` host arg + register-phase
``ccpp_constituent_properties_t(:)`` scheme args).

This module re-enables the legacy auto-clone path as an opt-in shim
(``--legacy-auto-clone-constituents`` on the capgen CLI).  It exists
for legacy host models — notably CAM-SIMA — that drive original capgen
heavily today and have not yet migrated to explicit registration.

When enabled, the shim:

* extends :data:`MetaVar._KNOWN_ATTRS` (in ``metadata/metadata_table.py``)
  with four legacy attributes that the strict-mode parser otherwise
  rejects: ``default_value``, ``min_value``, ``water_species``,
  ``mixing_ratio_type``;
* enables :class:`generator.suite_resolver.SuiteResolution.auto_cloned_constituents`
  collection — every ``is_constituent`` consumer whose ``std_name`` has
  no register-phase source is recorded with its metadata snapshot;
* drives :func:`generator.suite_cap._register_lines` to emit synthesised
  ``%instantiate(...)`` calls into the per-suite dynamic-constituents
  buffer, mirroring what a hand-written register-phase scheme would do.

## Single-instance constraint

The legacy code paths these models came from never supported multiple
in-memory host instances.  The shim follows the same restriction: when
enabled, the host metadata MUST NOT declare both ``instance_number``
and ``number_of_instances`` (the capgen multi-instance opt-in pair
— see :data:`metadata.registered_dimensions.SCALAR_INDEX_DIMS`).  The
gate is enforced in :func:`require_single_instance_host`, called from
:mod:`ccpp_capgen` after host metadata parse.  Code paths under the
shim assume ``instance_number`` is the literal ``1``.

## Self-contained for clean removal

Every touchpoint in the rest of the codebase is tagged
``# auto-clone-constituents:``.  Removing the feature is:

1. Delete ``metadata/auto_clone_constituents.py``
2. Delete ``unit-tests/test_auto_clone_constituents.py``
3. Delete the sample fixture files (search ``auto_clone`` under
   ``unit-tests/sample_files``)
4. ``grep -rn 'auto-clone-constituents\\|--legacy-auto-clone-constituents' .``
   and remove every remaining touchpoint (each is a 1-5 line snippet
   marked with a ``# auto-clone-constituents:`` comment).

Every hook is a no-op when the mode is not enabled — the shim has zero
impact on default capgen workflows.

Examples
--------
>>> from metadata import auto_clone_constituents
>>> auto_clone_constituents.is_enabled()
False
>>> auto_clone_constituents.extra_known_attrs()
frozenset()

When enabled, four legacy attrs become recognised:

>>> import io, logging
>>> logger = logging.getLogger('auto_clone_doctest')
>>> auto_clone_constituents.enable(logger, _stream=io.StringIO())
>>> auto_clone_constituents.is_enabled()
True
>>> sorted(auto_clone_constituents.extra_known_attrs())
['default_value', 'min_value', 'mixing_ratio_type', 'water_species']
>>> auto_clone_constituents.disable()
>>> auto_clone_constituents.is_enabled()
False
"""

from __future__ import annotations

import sys
from typing import FrozenSet, Optional, TextIO


# ----------------------------------------------------------------------
# Legacy metadata attributes the shim accepts on scheme args.
#
# Mapped to the matching kwargs of ``ccp_instantiate`` in
# ``src/ccpp_constituent_prop_mod.F90``:
#
# * default_value     -> default_value     (real, kind_phys)
# * min_value         -> min_value         (real, kind_phys)
# * water_species     -> water_species     (logical)
# * mixing_ratio_type -> mixing_ratio_type (character)
#
# The remaining %instantiate kwargs (std_name, long_name, diag_name,
# units, vertical_dim, advected, molar_mass) are already accepted by
# the strict-mode parser, just under canonical capgen names.
# ----------------------------------------------------------------------
_EXTRA_KNOWN_ATTRS: FrozenSet[str] = frozenset({
    'default_value',
    'min_value',
    'water_species',
    'mixing_ratio_type',
})


# Process-level on/off flag.  Mirrors the model used by
# :mod:`metadata.legacy_compat` and :mod:`metadata.dim_aliases`.
_ENABLED: bool = False


def enable(logger=None, _stream: Optional[TextIO] = None) -> None:
    """Turn the auto-clone shim on and emit a bold warning banner.

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

    banner_lines = ['', border,
                    _pad('WARNING: LEGACY AUTO-CLONE-CONSTITUENTS ENABLED'),
                    _pad('')]
    banner_lines += [
        _pad('Every is_constituent scheme arg (advected /'),
        _pad('constituent / molar_mass) without a register-phase'),
        _pad('source will be auto-registered into the per-suite'),
        _pad('dynamic-constituents buffer from its scheme'),
        _pad('metadata. Four legacy attributes become accepted'),
        _pad('on scheme args:'),
        _pad(''),
    ]
    for name in sorted(_EXTRA_KNOWN_ATTRS):
        banner_lines.append(_pad("  '{}'".format(name)))
    banner_lines += [
        _pad(''),
        _pad('The host metadata MUST NOT declare instance_number'),
        _pad('/ number_of_instances (single-instance only).'),
        _pad(''),
        _pad('This is a TRANSIENT shim for legacy hosts that have'),
        _pad('not migrated to explicit registration. It WILL BE'),
        _pad('REMOVED in a future capgen release.'),
        border,
        '',
    ]
    stream.write('\n'.join(banner_lines) + '\n')
    try:
        stream.flush()
    except Exception:  # pylint: disable=broad-except
        pass

    if logger is not None:
        logger.warning(
            "Legacy auto-clone-constituents enabled: is_constituent "
            "scheme args without an explicit register-phase source "
            "will be auto-registered from their metadata; legacy "
            "attributes %s become accepted on scheme args; host must "
            "be single-instance. This shim is transient and will be "
            "removed.",
            ', '.join("'{}'".format(n) for n in sorted(_EXTRA_KNOWN_ATTRS)),
        )


def disable() -> None:
    """Turn the auto-clone shim off.  Intended for tests and library
    users that wrap a generator invocation."""
    global _ENABLED
    _ENABLED = False


def is_enabled() -> bool:
    """Return ``True`` iff the auto-clone shim is enabled in this
    process."""
    return _ENABLED


def extra_known_attrs() -> FrozenSet[str]:
    """Return the legacy scheme-arg attribute names that the shim adds
    to :data:`MetaVar._KNOWN_ATTRS` when enabled.

    When the shim is disabled, returns an empty frozenset so that the
    parser's strict-mode "unknown attribute" rejection fires on these
    names just as it would for any other unrecognised key.
    """
    if not _ENABLED:
        return frozenset()
    return _EXTRA_KNOWN_ATTRS


def require_single_instance_host(host_dict) -> None:
    """Raise :class:`CCPPError` if the host declares the multi-instance
    pair (``instance_number`` + ``number_of_instances``) while the shim
    is enabled.

    The auto-clone path's emitted code paths assume the literal ``1``
    for every per-instance subscript.  Multi-instance support is not in
    scope for this transient shim — legacy hosts that need this path
    were always single-instance.  Called from
    :mod:`ccpp_capgen` after the host metadata has been flattened.

    No-op when the shim is disabled.  Accepts the resolved
    ``host_dict`` flat mapping (or any container with ``__contains__``)
    so it can be wired in wherever the host has been parsed.
    """
    if not _ENABLED:
        return
    # Lazy import to keep the shim free of generator dependencies.
    from metadata.parse_tools import CCPPError  # noqa: E402
    has_inst   = 'instance_number' in host_dict
    has_ninst  = 'number_of_instances' in host_dict
    if has_inst or has_ninst:
        raise CCPPError(
            "--legacy-auto-clone-constituents is single-instance only "
            "but the host metadata declares "
            "{found}.  Either remove the multi-instance pair from the "
            "host metadata or drop the --legacy-auto-clone-constituents "
            "flag.".format(
                found=' and '.join(
                    name for name, present in (
                        ('instance_number',     has_inst),
                        ('number_of_instances', has_ninst),
                    ) if present
                )
            )
        )
