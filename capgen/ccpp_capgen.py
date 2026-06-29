#!/usr/bin/env python3

"""ccpp_capgen — next-generation CCPP cap code generator.

This script replaces both ``ccpp_prebuild.py`` and ``ccpp_capgen.py`` from the
legacy toolchain.  It reads host-model metadata files, scheme metadata files,
and suite XML definition files, resolves all variable connections, and writes:

* ``ccpp_kinds.F90``       — kind parameter definitions
* ``<host>_ccpp_cap.F90``  — static dispatch API (per-host; filename and module
  name derived from ``--host-name``)
* ``ccpp_<suite>_cap.F90`` — suite-level cap (state machine, group dispatch)
* ``ccpp_<suite>_<group>_cap.F90`` — group-level cap (scheme call sites)
* ``ccpp_<suite>_data.F90``        — suite-owned interstitial data module
* ``ccpp_<suite>_types.F90``       — shared types (pointer wrappers, temp locals)
* ``ccpp_<suite>.meta``            — generated suite metadata (for inspection)
* ``datatable.xml``                — generator database for ``ccpp_datafile.py``

Usage
-----
::

    ccpp_capgen.py \\
        --host-name    <name> \\
        --host-files   <f1.meta,f2.meta,...> \\
        --scheme-files <f1.meta,f2.meta,...> \\
        --suites       <s1.xml,s2.xml,...> \\
        --output-root  <path> \\
        --kind-type    NAME=[MODULE:]SPEC \\   # repeatable, see below
        --verbose                              # once=INFO, twice=DEBUG

``--kind-type``
^^^^^^^^^^^^^^^

Each ``--kind-type`` entry maps a CCPP-visible kind name to a Fortran
precision constant.  The syntax is::

    --kind-type <name>=[<module>:]<spec>

* ``<name>`` is the kind name as it will be published in ``ccpp_kinds`` and
  referenced in scheme metadata (e.g. ``kind_phys``).
* ``<spec>`` is the name of a precision constant (a kind parameter) defined
  in some Fortran module.
* ``<module>`` is the Fortran module that defines ``<spec>``.  When
  ``<module>:`` is omitted, ``<spec>`` must be a standard
  ``ISO_FORTRAN_ENV`` constant (``REAL32``, ``REAL64``, ``INT32`` etc.) and
  the module defaults to ``iso_fortran_env``.

The flag may be specified multiple times.

Examples::

    --kind-type kind_phys=REAL64
        # → use iso_fortran_env, only: REAL64
        #   integer, parameter, public :: kind_phys = REAL64

    --kind-type kind_phys=my_host_kinds:kind_r8
        # → use my_host_kinds, only: kind_r8
        #   integer, parameter, public :: kind_phys = kind_r8

If no ``--kind-type`` is supplied (or ``kind_phys`` is omitted from a
non-empty list), the generator injects ``kind_phys=iso_fortran_env:REAL64``
and logs an INFO message.  ``ccpp_kinds.F90`` is always written.

Exit codes
----------
0 — success
1 — user error (metadata problem, missing file, etc.)
2 — internal error (bug in the generator)
"""

import argparse
import logging
import os
import sys
from typing import Dict, List, Optional, Tuple

# Ensure the capgen package is importable when invoked directly.
_SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
_PACKAGE_DIR = os.path.dirname(_SCRIPT_DIR)
if _PACKAGE_DIR not in sys.path:
    sys.path.insert(0, _PACKAGE_DIR)

from metadata.parse_tools import CCPPError, init_log, set_log_level
from metadata.metadata_table import parse_metadata_file, MetadataTable
from metadata.variable_resolver import (
    build_ddt_module_map,
    build_flat_host_dict,
    SchemeStore,
)
from generator.kinds_writer import write_ccpp_kinds
from generator.suite_xml import parse_suite_xml_files
from generator.suite_resolver import (
    resolve_suite, iter_phase_calls, validate_init_dimensions,
)
from generator.group_cap import write_group_cap
from generator.suite_data import write_suite_data, write_suite_meta
from generator.suite_cap import write_suite_cap
from generator.suite_types import write_suite_types
from generator.host_cap import write_host_cap
from generator.host_constituents import write_host_constituents
from generator.datatable import write_datatable


########################################################################
# Logging
########################################################################

_LOGGER = init_log('ccpp_capgen')


########################################################################
# Framework-shipped metadata
########################################################################

# Path to the framework-shipped constituent module metadata, auto-included
# as a host metadata file so that the constituent DDT types are always known
# to the generator (even when the host metadata does not declare them).
_FRAMEWORK_SRC_DIR = os.path.join(_SCRIPT_DIR, 'src')
_FRAMEWORK_HOST_META = [
    os.path.join(_FRAMEWORK_SRC_DIR, 'ccpp_constituent_prop_mod.meta'),
]

# Framework Fortran source files that must be compiled alongside the
# generated cap modules whenever any suite touches constituent state.
# Listed in datatable.xml's <utilities> so host CMake projects pick them
# up via ccpp_datafile.py --utility-files / --ccpp-files queries.  All
# of these live in :data:`_FRAMEWORK_SRC_DIR` (capgen's own ``src/``);
# capgen ships self-contained — no external src/ companion needed.
_FRAMEWORK_F90_FILES = [
    'ccpp_constituent_prop_mod.F90',
    'ccpp_hashable.F90',
    'ccpp_hash_table.F90',
    'ccpp_scheme_utils.F90',
]


def _resolve_framework_f90_files() -> List[str]:
    """Return absolute paths for the framework F90 files.

    Each name in :data:`_FRAMEWORK_F90_FILES` is looked up under
    :data:`_FRAMEWORK_SRC_DIR` (``capgen/src/``).  A missing file is
    a hard error: capgen/src/ is the canonical (and only) location;
    a missing file means the deployment is incomplete and the host
    build would fail later with an opaque "Cannot open module file"
    error.  Surface it now with a precise message instead.
    """
    found: List[str] = []
    missing: List[str] = []
    for name in _FRAMEWORK_F90_FILES:
        p = os.path.join(_FRAMEWORK_SRC_DIR, name)
        if os.path.isfile(p):
            found.append(os.path.abspath(p))
        else:
            missing.append(p)
    if missing:
        raise CCPPError(
            "capgen deployment is incomplete: required framework "
            "Fortran source file(s) not found under {!r}:\n  {}\n"
            "Vendor the missing file(s) into capgen/src/ (the "
            "canonical location for files capgen emits a USE for).".format(
                _FRAMEWORK_SRC_DIR, '\n  '.join(missing),
            )
        )
    return found


########################################################################
# CLI
########################################################################

def _build_arg_parser() -> argparse.ArgumentParser:
    """Build and return the argument parser.

    Returns
    -------
    argparse.ArgumentParser
    """
    parser = argparse.ArgumentParser(
        prog='ccpp_capgen.py',
        description='CCPP next-generation cap code generator',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=__doc__,
    )
    parser.add_argument(
        '--host-name',
        required=True,
        metavar='NAME',
        help=(
            'Host model identifier.  Drives the file and module name '
            'of the generated static-API cap (``<host>_ccpp_cap.F90`` / '
            '``module <host>_ccpp_cap``) so multiple host integrations '
            'can co-exist in one executable, and is written into '
            '``datatable.xml`` as the host var-dictionary name.'
        ),
    )
    parser.add_argument(
        '--host-files',
        required=True,
        metavar='FILE[,FILE...]',
        help='Comma-separated list of host-model metadata (.meta) files',
    )
    parser.add_argument(
        '--scheme-files',
        required=True,
        metavar='FILE[,FILE...]',
        help='Comma-separated list of physics scheme metadata (.meta) files',
    )
    parser.add_argument(
        '--suites',
        required=True,
        metavar='FILE[,FILE...]',
        help='Comma-separated list of suite XML definition (.xml) files',
    )
    parser.add_argument(
        '--output-root',
        required=True,
        metavar='DIR',
        help='Output directory for all generated files',
    )
    parser.add_argument(
        '--kind-type',
        action='append',
        default=[],
        metavar='NAME=[MODULE:]SPEC',
        help=(
            'Map a CCPP kind name to a Fortran precision constant. Syntax: '
            '``<name>=[<module>:]<spec>``. When ``<module>:`` is omitted, '
            '``<spec>`` must be an ISO_FORTRAN_ENV constant (REAL32/REAL64/'
            'INT32/...) and the module defaults to ``iso_fortran_env``. '
            'Examples: ``--kind-type kind_phys=REAL64``, '
            '``--kind-type kind_phys=my_host_kinds:kind_r8``. May be '
            'specified multiple times. If kind_phys is not supplied, '
            '``kind_phys=iso_fortran_env:REAL64`` is injected automatically.'
        ),
    )
    parser.add_argument(
        '--verbose', '-v',
        action='count',
        default=0,
        help=(
            'Increase verbosity.  Use once for INFO messages, '
            'twice (-vv) for DEBUG messages.'
        ),
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
    # dim-aliases: transient GFS-physics shim (delete the argument,
    # the enable() call below, and the rest of the dim_aliases
    # touchpoints when the workaround is removed).
    parser.add_argument(
        '--gfs-dim-aliases',
        action='store_true',
        help=(
            "TRANSIENT GFS-PHYSICS SHIM.  Treat a small audited list of "
            "physically-equivalent vertical-axis standard names as the "
            "same dimension during the host/scheme dim-position "
            "identity check only (e.g. "
            "'adjusted_vertical_layer_dimension_for_radiation' and "
            "'vertical_composition_dimension' both compare equal to "
            "'vertical_layer_dimension').  Variables keep their "
            "original names everywhere else.  Emits a loud warning at "
            "startup.  Will be removed."
        ),
    )
    # auto-clone-constituents: transient legacy shim (delete the
    # argument, the enable() call below, and the rest of the
    # auto_clone_constituents touchpoints when legacy hosts have
    # migrated to explicit registration).
    parser.add_argument(
        '--legacy-auto-clone-constituents',
        action='store_true',
        help=(
            "TRANSIENT LEGACY SHIM.  Replicate original capgen's "
            "auto-clone-static-constituent path: every is_constituent "
            "scheme arg (advected / constituent / molar_mass) without "
            "an explicit register-phase source is auto-registered into "
            "the per-suite dynamic-constituents buffer using values "
            "lifted directly from its scheme metadata.  Accepts four "
            "legacy attributes on scheme args (default_value, "
            "min_value, water_species, mixing_ratio_type).  "
            "SINGLE-INSTANCE ONLY — the host must not declare the "
            "(instance_number, number_of_instances) multi-instance "
            "pair.  Emits a loud warning at startup.  Will be removed."
        ),
    )
    parser.add_argument(
        '--no-host-introspection',
        action='store_true',
        help=(
            "Stub the five suite-introspection routines in "
            "<host>_ccpp_cap.F90 (ccpp_physics_suite_list / "
            "suite_part_list / suite_schemes / suite_variables / "
            "suite_host_data).  Signatures remain so callers still "
            "link, but bodies set errflg=1 with a clear errmsg "
            "(suite_list, which has no errflg, writes to error_unit "
            "and returns an empty list).  Use this to shrink the "
            "generated <host>_ccpp_cap.F90 from ~33000 lines to ~800 "
            "for multi-suite builds where even -O1 cannot finish "
            "compiling the introspection case-blocks."
        ),
    )
    parser.add_argument(
        '--trace',
        action='store_true',
        help=(
            "Set the default value of the per-module ``trace`` "
            "parameter to ``.true.`` in every generated cap.  The "
            "gated ``if (trace) write(error_unit,*) ...`` lines are "
            "ALWAYS emitted (one per cap subroutine that has "
            "intent(in)/inout control dummies) so that strict "
            "unused-variable warnings -- such as Intel oneAPI's -- "
            "are silenced even when tracing is off.  This flag only "
            "flips the compile-time default; a developer can also "
            "hand-edit ``logical, parameter :: trace`` in any "
            "generated cap to ``.true.`` to enable tracing for that "
            "module and rebuild."
        ),
    )
    return parser


# Standard ISO_FORTRAN_ENV kind constants accepted as a bare ``<spec>`` (i.e.
# without an explicit ``<module>:`` prefix).  Compared case-insensitively.
_ISO_FORTRAN_KINDS = frozenset({
    'INT8', 'INT16', 'INT32', 'INT64',
    'REAL32', 'REAL64', 'REAL128',
})

_ISO_FORTRAN_MODULE = 'iso_fortran_env'


def _parse_kind_types(
    kind_type_args: List[str],
) -> Dict[str, Tuple[str, str]]:
    """Parse ``--kind-type NAME=[MODULE:]SPEC`` arguments into a mapping.

    Parameters
    ----------
    kind_type_args : list of str
        Each entry must have the form ``<name>=[<module>:]<spec>``.

    Returns
    -------
    dict
        Mapping from kind name to a ``(module, spec)`` tuple.

    Raises
    ------
    CCPPError
        If any entry is malformed, has a duplicate name, or omits the module
        for a non-ISO ``<spec>``.

    Examples
    --------
    Default ``iso_fortran_env`` module when spec is a known ISO kind:

    >>> _parse_kind_types(['kind_phys=REAL64', 'kind_dyn=REAL32'])
    {'kind_phys': ('iso_fortran_env', 'REAL64'), 'kind_dyn': ('iso_fortran_env', 'REAL32')}

    Explicit host-supplied module:

    >>> _parse_kind_types(['kind_phys=my_host_kinds:kind_r8'])
    {'kind_phys': ('my_host_kinds', 'kind_r8')}

    Mixed:

    >>> sorted(_parse_kind_types([
    ...     'kind_iso=REAL64',
    ...     'kind_host=my_kinds:kind_r4',
    ... ]).items())
    [('kind_host', ('my_kinds', 'kind_r4')), ('kind_iso', ('iso_fortran_env', 'REAL64'))]

    Malformed (missing ``=``):

    >>> _parse_kind_types(['bad_entry'])
    Traceback (most recent call last):
        ...
    metadata.parse_tools.parse_source.CCPPError: --kind-type 'bad_entry' must have the form NAME=[MODULE:]SPEC

    Duplicate entry:

    >>> _parse_kind_types(['kind_phys=REAL64', 'kind_phys=REAL32'])
    Traceback (most recent call last):
        ...
    metadata.parse_tools.parse_source.CCPPError: Duplicate --kind-type entry for 'kind_phys'

    Non-ISO spec without explicit module:

    >>> _parse_kind_types(['kind_phys=kind_r8'])
    Traceback (most recent call last):
        ...
    metadata.parse_tools.parse_source.CCPPError: --kind-type 'kind_phys=kind_r8': spec 'kind_r8' is not a standard ISO_FORTRAN_ENV constant; supply the module explicitly as <module>:<spec>
    """
    mapping: Dict[str, Tuple[str, str]] = {}
    for entry in kind_type_args:
        head, sep, tail = entry.partition('=')
        if not sep or not head.strip() or not tail.strip():
            raise CCPPError(
                "--kind-type '{}' must have the form NAME=[MODULE:]SPEC".format(entry)
            )
        kind_name = head.strip()
        rhs       = tail.strip()

        # Split the right-hand side on ':'.  At most one colon is permitted.
        rhs_parts = rhs.split(':')
        if len(rhs_parts) == 1:
            spec   = rhs_parts[0].strip()
            module = _ISO_FORTRAN_MODULE
            if spec.upper() not in _ISO_FORTRAN_KINDS:
                raise CCPPError(
                    "--kind-type '{}': spec '{}' is not a standard "
                    "ISO_FORTRAN_ENV constant; supply the module "
                    "explicitly as <module>:<spec>".format(entry, spec)
                )
        elif len(rhs_parts) == 2:
            module = rhs_parts[0].strip()
            spec   = rhs_parts[1].strip()
            if not module or not spec:
                raise CCPPError(
                    "--kind-type '{}': both <module> and <spec> must be "
                    "non-empty when using the <module>:<spec> form".format(entry)
                )
        else:
            raise CCPPError(
                "--kind-type '{}': at most one ':' is permitted "
                "(syntax is NAME=[MODULE:]SPEC)".format(entry)
            )

        if kind_name in mapping:
            raise CCPPError(
                "Duplicate --kind-type entry for '{}'".format(kind_name)
            )
        mapping[kind_name] = (module, spec)
    return mapping


def _ensure_kind_phys_default(
    kind_types: Dict[str, Tuple[str, str]],
    log: logging.Logger,
) -> Dict[str, Tuple[str, str]]:
    """Inject ``kind_phys=iso_fortran_env:REAL64`` if not already mapped.

    Mutates and returns *kind_types*.  Logs an INFO message when the default
    is injected, so users always know that the fallback is in effect.
    """
    if 'kind_phys' not in kind_types:
        kind_types['kind_phys'] = (_ISO_FORTRAN_MODULE, 'REAL64')
        log.info(
            "kind_phys not supplied via --kind-type or metadata kind_spec; "
            "defaulting to REAL64 from iso_fortran_env"
        )
    return kind_types


def _collect_metadata_kind_specs(
    tables: List[MetadataTable],
) -> Dict[str, Tuple[str, str]]:
    """Aggregate ``kind_spec`` declarations across loaded metadata tables.

    Each table contributes zero or more ``(kind_name, module, spec)`` triples
    via :attr:`MetadataTable.kind_specs`.  All contributions for the same
    ``kind_name`` must agree; identical duplicates are collapsed silently
    while a divergent ``(module, spec)`` raises :exc:`CCPPError` with a
    message naming both source files.

    Parameters
    ----------
    tables : list of MetadataTable
        Host and scheme metadata tables, in any order.

    Returns
    -------
    dict
        Mapping ``kind_name -> (module, spec)``.

    Raises
    ------
    CCPPError
        If two tables declare the same ``kind_name`` with different
        ``(module, spec)`` pairs.
    """
    result:  Dict[str, Tuple[str, str]] = {}
    sources: Dict[str, str]             = {}
    for tbl in tables:
        for kind_name, module, spec in tbl.kind_specs:
            pair   = (module, spec)
            origin = "{} (table '{}')".format(tbl.file_path, tbl.table_name)
            existing = result.get(kind_name)
            if existing is None:
                result[kind_name]  = pair
                sources[kind_name] = origin
            elif existing != pair:
                raise CCPPError(
                    "Conflicting kind_spec for kind '{}': {} declares "
                    "'{}:{}' but {} declares '{}:{}'".format(
                        kind_name, sources[kind_name],
                        existing[0], existing[1],
                        origin, pair[0], pair[1],
                    )
                )
    return result


def _merge_cli_and_metadata_kinds(
    cli_kinds:  Dict[str, Tuple[str, str]],
    meta_kinds: Dict[str, Tuple[str, str]],
) -> Dict[str, Tuple[str, str]]:
    """Combine ``--kind-type`` CLI mappings with metadata-declared kinds.

    For any ``kind_name`` defined in both sides the ``(module, spec)`` pair
    must match exactly.  Identical pairs collapse silently; mismatches raise
    :exc:`CCPPError`.

    Parameters
    ----------
    cli_kinds : dict
        Mapping from :func:`_parse_kind_types`.
    meta_kinds : dict
        Mapping from :func:`_collect_metadata_kind_specs`.

    Returns
    -------
    dict
        Merged mapping ``kind_name -> (module, spec)``.

    Raises
    ------
    CCPPError
        If CLI and metadata declare the same kind name with different
        ``(module, spec)`` pairs.
    """
    merged = dict(cli_kinds)
    for kind_name, pair in meta_kinds.items():
        existing = merged.get(kind_name)
        if existing is None:
            merged[kind_name] = pair
        elif existing != pair:
            raise CCPPError(
                "Kind '{}' declared inconsistently: --kind-type says "
                "'{}:{}' but metadata kind_spec says '{}:{}'".format(
                    kind_name, existing[0], existing[1], pair[0], pair[1],
                )
            )
    return merged


def _split_file_list(arg: str) -> List[str]:
    """Split a comma-separated file-list argument, stripping whitespace.

    Parameters
    ----------
    arg : str
        Comma-separated list of file paths.

    Returns
    -------
    list of str

    Examples
    --------
    >>> _split_file_list('a.meta, b.meta, c.meta')
    ['a.meta', 'b.meta', 'c.meta']
    >>> _split_file_list('single.meta')
    ['single.meta']
    >>> _split_file_list('')
    []
    """
    return [f.strip() for f in arg.split(',') if f.strip()]


########################################################################
# Metadata loading
########################################################################

# Loop-bound standard names that must never appear as variable dimensions.
# These are control variables (scalars passed as subroutine arguments) and
# using them as array dimensions indicates a porting error from the legacy
# toolchain.  Remove this guard once migration is complete.
_FORBIDDEN_DIMENSION_NAMES = frozenset({
    'horizontal_loop_extent',
    'horizontal_loop_begin',
    'horizontal_loop_end',
})


def _check_no_loop_dimensions(tables: list) -> None:
    """Raise CCPPError if any variable uses a forbidden dimension name.

    Parameters
    ----------
    tables : list of MetadataTable

    Raises
    ------
    CCPPError
        If any variable's dimensions list contains a name from
        ``_FORBIDDEN_DIMENSION_NAMES``.  All violations are collected and
        reported together.
    """
    errors = []
    for tbl in tables:
        for sec in tbl.sections():
            for var in sec.variables:
                for dim in var.dimensions:
                    if dim in _FORBIDDEN_DIMENSION_NAMES:
                        errors.append(
                            "Variable '{}' (standard_name='{}') in table "
                            "'{}' (type={}) in '{}' uses '{}' as a "
                            "dimension. Loop-bound control/legacy vars must "
                            "not appear in dimension attributes; use "
                            "horizontal_dimension instead.".format(
                                var.local_name, var.standard_name,
                                tbl.table_name, tbl.table_type,
                                tbl.file_path, dim,
                            )
                        )
    if errors:
        raise CCPPError(
            "Forbidden dimension names found in metadata:\n\n{}".format(
                '\n\n'.join("ERROR: " + e for e in errors)
            )
        )

def _load_metadata_files(
    file_list: List[str],
    expected_types: frozenset,
    label: str,
) -> List[MetadataTable]:
    """Load and validate a list of metadata files.

    Parameters
    ----------
    file_list : list of str
        Paths to ``.meta`` files.
    expected_types : frozenset of str
        Table types that are acceptable in these files.  Any table with a
        different type raises a :exc:`CCPPError`.
    label : str
        Human-readable description (``'host'`` or ``'scheme'``) used in
        error messages.

    Returns
    -------
    list of MetadataTable
        All tables parsed from all files, in order.

    Raises
    ------
    CCPPError
        On any parse error or unexpected table type.
    """
    tables: List[MetadataTable] = []
    for fpath in file_list:
        _LOGGER.info("Reading %s metadata: %s", label, fpath)
        file_tables = parse_metadata_file(fpath)
        for tbl in file_tables:
            if tbl.table_type not in expected_types:
                raise CCPPError(
                    "Unexpected table type '{}' in {} metadata file '{}'; "
                    "expected one of {}".format(
                        tbl.table_type, label, fpath, sorted(expected_types)
                    )
                )
        _check_no_loop_dimensions(file_tables)
        tables.extend(file_tables)
    return tables


########################################################################
# Control-variable validation
########################################################################

# Required control variables: (standard_name, expected_fortran_type, description)
_REQUIRED_CTRL_VARS = [
    ('suite_name',               'character', 'drives suite dispatch'),
    ('group_name',               'character', 'drives per-group dispatch inside ccpp_physics_* (each suite_cap emits a select case on this name)'),
    ('horizontal_loop_begin',    'integer',   'lower horizontal slice bound at scheme call sites'),
    ('horizontal_loop_end',      'integer',   'upper horizontal slice bound at scheme call sites'),
    ('number_of_physics_threads','integer',   'physics-internal thread budget (pass 1 if unused)'),
    ('ccpp_error_code',          'integer',   'CCPP error code'),
    ('ccpp_error_message',       'character', 'CCPP error message'),
]
# NOTE: the threading index/count (``thread_number`` / ``number_of_threads``)
# is NOT required — it is a paired-optional control pair, fully symmetric with
# (``instance_number`` / ``number_of_instances``); see
# ``_PAIRED_OPTIONAL_CTRL_VARS`` below.  ``number_of_physics_threads`` is a
# separate, unpaired scheme-facing scalar that stays unconditionally required.

# Paired-optional control variables.  Each entry is an (index, count) pair:
# the host declares BOTH members (in ``type=control``) or NEITHER; declaring
# exactly one is a hard error.  Declaring a pair opts the host into that
# multi-<X> API — the index flows as a per-call control dummy and the count
# gives the bound.  When a pair is absent the public API drops both args and
# the framework uses literal ``1`` wherever the index would appear.  A host
# variable may be dimensioned by the count standard name only when its pair is
# declared (otherwise the resolver's scalar-index collapse raises — it needs
# the index variable in scope).
#
# The two pairs are fully symmetric (decision 2026-06-09):
#   * (instance_number, number_of_instances) — multi-instance API.  The
#     framework reads ``number_of_instances`` at register/init to size its
#     own per-instance state (``ccpp_suite_data(:)``, ``ccpp_group_state(:)``).
#   * (thread_number, number_of_threads) — multi-threading API.
#     ``thread_number`` indexes host-owned per-thread containers;
#     ``number_of_threads`` is carried as a control dummy (the framework owns
#     no per-thread state yet, so its value is not consumed — kept for symmetry
#     with ``number_of_instances`` and future per-thread sizing).
# (A chunk/block index is intentionally NOT a control pair: capgen's
# slice-based design passes the current chunk as a horizontal range via
# horizontal_loop_begin/end, so no scheme ever indexes by chunk inside a call.)
# Each entry: (index std_name, count std_name, index description, count description).
_PAIRED_OPTIONAL_CTRL_VARS = [
    ('instance_number', 'number_of_instances',
     'current model instance index', 'total number of model instances'),
    ('thread_number',   'number_of_threads',
     'current thread index', 'total thread count'),
]


def _validate_required_control_vars(
    host_name: str,
    host_dict: dict,
) -> None:
    """Check that every required control variable is present in *host_dict*.

    Collects all failures and raises a single :exc:`CCPPError` listing them.

    Parameters
    ----------
    host_name : str
        Host model identifier, used in error messages so the developer
        can tell which host the failure refers to when more than one
        capgen invocation is in flight.
    host_dict : dict
        Flat host variable dictionary built by :func:`build_flat_host_dict`.

    Raises
    ------
    CCPPError
        If any required control variable is missing, not marked as a control
        variable, has the wrong Fortran type, or is not a scalar.
    """
    errors = []

    def _check_control_var(std_name, expected_type, description, required: bool) -> None:
        """Validate a variable that must live in a ``type=control`` table."""
        entry = host_dict.get(std_name)

        if entry is None:
            if required:
                errors.append(
                    "Required control variable '{}' not found in host '{}' "
                    "type=control metadata.\n"
                    "  This variable {}. Add it to a "
                    "[ccpp-table-properties] / type=control block in the "
                    "host metadata files.".format(std_name, host_name, description)
                )
            return

        if not entry.is_control:
            errors.append(
                "Variable '{}' must be declared in a type=control table "
                "for host '{}', but it was found in a type=host table.\n"
                "  Move it to a [ccpp-table-properties] / type=control "
                "block.".format(std_name, host_name)
            )
            return

        if entry.type.lower() != expected_type.lower():
            errors.append(
                "Required control variable '{}' in host '{}' has Fortran "
                "type '{}' but '{}' is required.".format(
                    std_name, host_name, entry.type, expected_type
                )
            )

        if entry.dimensions:
            errors.append(
                "Required control variable '{}' in host '{}' must be a "
                "scalar (rank-0) but has dimensions {}.".format(
                    std_name, host_name, entry.dimensions
                )
            )

    for std_name, expected_type, description in _REQUIRED_CTRL_VARS:
        _check_control_var(std_name, expected_type, description, required=True)

    # Paired-optional control pairs (see _PAIRED_OPTIONAL_CTRL_VARS): for each
    # (index, count) pair the host declares both members in a type=control
    # table or neither.  Declaring exactly one is an error.  Both pairs —
    # (instance_number, number_of_instances) and (thread_number,
    # number_of_threads) — are validated identically.
    for idx_name, cnt_name, idx_desc, cnt_desc in _PAIRED_OPTIONAL_CTRL_VARS:
        _check_control_var(idx_name, 'integer', idx_desc, required=False)
        _check_control_var(cnt_name, 'integer', cnt_desc, required=False)

        idx_present = host_dict.get(idx_name) is not None
        cnt_present = host_dict.get(cnt_name) is not None
        if idx_present ^ cnt_present:
            present, missing = (
                (idx_name, cnt_name) if idx_present else (cnt_name, idx_name)
            )
            errors.append(
                "Host '{}' declares '{}' in a type=control table but is "
                "missing its paired variable '{}' (which must also be in a "
                "type=control table).\n"
                "  '{}' and '{}' are a paired-optional control pair: declare "
                "both members to opt into that API, or neither.".format(
                    host_name, present, missing, idx_name, cnt_name,
                )
            )

    # Control-table allowlist: a type=control table may declare ONLY the
    # framework's known control variables — the unconditionally required set
    # plus the members of the paired-optional pairs.  Anything else in a
    # type=control table is a hard error.  Host-specific quantities that
    # schemes consume belong in a type=host table; the subcycle loop variables
    # (ccpp_loop_counter / ccpp_loop_extent) are generator-owned locals the
    # host never declares.
    allowed_control = {name for name, _type, _desc in _REQUIRED_CTRL_VARS}
    for idx_name, cnt_name, _idesc, _cdesc in _PAIRED_OPTIONAL_CTRL_VARS:
        allowed_control.add(idx_name)
        allowed_control.add(cnt_name)
    for std_name, entry in sorted(host_dict.items()):
        if entry.is_control and std_name not in allowed_control:
            errors.append(
                "Variable '{}' is declared in a type=control table for host "
                "'{}' but is not a recognized framework control variable.\n"
                "  A type=control table may declare only: {}.\n"
                "  If '{}' is a host quantity that schemes consume, declare it "
                "in a type=host table instead.".format(
                    std_name, host_name, ', '.join(sorted(allowed_control)),
                    std_name,
                )
            )

    if errors:
        raise CCPPError(
            "Host '{}' has invalid control-variable metadata:\n\n{}".format(
                host_name,
                '\n\n'.join("ERROR: " + e for e in errors),
            )
        )


########################################################################
# Entry point
########################################################################

def capgen(
    host_name: str,
    host_files: List[str],
    scheme_files: List[str],
    suite_files: List[str],
    output_root: str,
    kind_types: Dict[str, Tuple[str, str]],
    logger: Optional[logging.Logger] = None,
    no_host_introspection: bool = False,
    trace: bool = False,
    return_state: bool = False,
):
    """Programmatic entry point for the cap generator.

    Mirrors the CLI behaviour.  Both the CLI and programmatic paths call
    this function.

    Parameters
    ----------
    host_name : str
        Host model identifier.  Drives the file and module name of the
        generated static-API cap (``<host>_ccpp_cap.F90`` / ``module
        <host>_ccpp_cap``) and is written into ``datatable.xml``.
    host_files : list of str
        Host metadata (``.meta``) file paths.
    scheme_files : list of str
        Scheme metadata (``.meta``) file paths.
    suite_files : list of str
        Suite XML (``.xml``) file paths.
    output_root : str
        Directory where all generated files are written.
    kind_types : dict
        Mapping ``kind_name -> (module_name, kind_spec)``.  May be empty;
        ``kind_phys=(iso_fortran_env, REAL64)`` is injected automatically
        when missing.
    logger : logging.Logger, optional
        Logger to use.  Defaults to the module-level logger.

    Raises
    ------
    CCPPError
        On any user-facing error.
    """
    log = logger or _LOGGER

    # Snapshot the CLI-provided kinds; the default ``kind_phys`` and any
    # metadata-declared kind_specs are folded in below, after metadata loads.
    cli_kind_types = dict(kind_types)

    # ---- validate output directory -----------------------------------------
    os.makedirs(output_root, exist_ok=True)

    # ---- load host metadata (host + control tables) -------------------------
    log.info("Loading host metadata for host '%s'", host_name)
    framework_meta = [p for p in _FRAMEWORK_HOST_META if os.path.isfile(p)]
    if framework_meta:
        log.info("Auto-including framework metadata: %s", framework_meta)
    host_tables = _load_metadata_files(
        framework_meta + list(host_files),
        expected_types=frozenset({'host', 'control', 'ddt'}),
        label='host',
    )
    log.info("Loaded %d host/control/ddt tables", len(host_tables))

    # ---- load scheme metadata -----------------------------------------------
    log.info("Loading scheme metadata")
    scheme_tables = _load_metadata_files(
        scheme_files,
        expected_types=frozenset({'scheme', 'ddt'}),
        label='scheme',
    )
    log.info("Loaded %d scheme/ddt tables", len(scheme_tables))

    # ---- merge --kind-type with metadata kind_spec declarations -----------
    meta_kind_types = _collect_metadata_kind_specs(host_tables + scheme_tables)
    if meta_kind_types:
        log.info(
            "Found %d kind_spec declaration(s) in metadata: %s",
            len(meta_kind_types), sorted(meta_kind_types),
        )
    kind_types = _merge_cli_and_metadata_kinds(cli_kind_types, meta_kind_types)
    kind_types = _ensure_kind_phys_default(kind_types, log)

    # ---- build flat host variable dictionary --------------------------------
    host_only    = [t for t in host_tables if t.table_type == 'host']
    control_only = [t for t in host_tables if t.table_type == 'control']
    ddt_from_host = [t for t in host_tables if t.table_type == 'ddt']
    ddt_from_schemes = [t for t in scheme_tables if t.table_type == 'ddt']
    all_ddt_tables = ddt_from_host + ddt_from_schemes

    host_dict = build_flat_host_dict(host_only, control_only, all_ddt_tables)
    log.info("Host dictionary contains %d variables", len(host_dict))

    # Map DDT type name → defining Fortran module, derived from co-located
    # tables in each .meta file.  Used by the suite data generator to emit
    # USE statements for DDT-typed suite-owned variables.
    ddt_module_map = build_ddt_module_map(host_tables + scheme_tables)

    # ---- Phase 1 validation: required control variables ---------------------
    _validate_required_control_vars(host_name, host_dict)

    # auto-clone-constituents: enforce the single-instance constraint
    # of the transient legacy shim.  No-op when the shim is disabled.
    from metadata import auto_clone_constituents
    auto_clone_constituents.require_single_instance_host(host_dict)

    # Signal which instance API the host opted into so users can tell which
    # branch the generator took.  Paired-presence has already been enforced.
    if host_dict.get('instance_number') is not None:
        log.info("Host '%s' declares instance_number — generating "
                 "multi-instance API.", host_name)
    else:
        log.info("Host '%s' did not declare instance_number — generating "
                 "single-instance API (per-instance arrays sized to 1).",
                 host_name)

    # ---- build scheme metadata store ----------------------------------------
    scheme_store = SchemeStore.build_from(scheme_tables)
    log.info("Scheme store contains %d schemes: %s",
             len(scheme_store.scheme_names()), scheme_store.scheme_names())

    # ---- write ccpp_kinds.F90 (always generated) ---------------------------
    # Every writer below logs its own "Wrote <path>" / "Unchanged: <path>"
    # line via the write-if-changed helper when *logger* is threaded
    # through.  Don't duplicate that log here.
    kinds_path = write_ccpp_kinds(kind_types, output_root, logger=log)

    # ---- parse suite XML files ----------------------------------------------
    suites = parse_suite_xml_files(suite_files, output_root, log)
    log.info("Loaded %d suite(s): %s", len(suites), [s.name for s in suites])

    # ---- resolve and generate per-suite outputs ----------------------------
    suite_names       = []
    suite_resolutions = []

    for suite in suites:
        log.info("Resolving suite '%s'", suite.name)
        suite_res = resolve_suite(suite, scheme_store, host_dict)
        # Fail early (before any cap is written) if a non-allocatable
        # suite-owned variable is dimensioned by a scheme-updated quantity
        # that isn't known when suite_data_init_fields allocates it.
        validate_init_dimensions(suite_res)
        suite_names.append(suite.name)
        suite_resolutions.append(suite_res)

        # Group caps
        for resolved_group in suite_res.groups:
            write_group_cap(
                suite.name, resolved_group.group_name, resolved_group, host_dict, output_root,
                logger=log,
                trace=trace,
            )

        # Suite data module
        write_suite_data(
            suite.name, suite_res.suite_vars, output_root, host_dict,
            ddt_module_map=ddt_module_map, logger=log,
        )

        # Suite metadata (for inspection)
        write_suite_meta(
            suite.name, suite_res.suite_vars, output_root, logger=log,
        )

        # Suite types module (only when optional args are present)
        write_suite_types(
            suite.name, suite_res, output_root,
            ddt_module_map=ddt_module_map, logger=log,
        )

        # Suite cap
        write_suite_cap(
            suite.name, suite_res, scheme_store, output_root, host_dict,
            logger=log,
            trace=trace,
        )

    # ---- host cap (one file for all suites) --------------------------------
    write_host_cap(
        host_name, suite_names, suite_resolutions, output_root,
        host_dict, scheme_store,
        logger=log,
        no_host_introspection=no_host_introspection,
        trace=trace,
    )

    # ---- host-wide constituent module (only when any suite touches
    #      constituent state) ------------------------------------------------
    host_consts_path = write_host_constituents(
        suite_resolutions, output_root, host_dict=host_dict, logger=log,
    )

    # ---- datatable.xml ------------------------------------------------------
    abs_root = os.path.abspath(output_root)
    utility_paths = [
        os.path.join(abs_root, 'ccpp_kinds.F90'),
    ]
    if host_consts_path:
        utility_paths.append(host_consts_path)
        # The generated ccpp_host_constituents.F90 USEs ccpp_constituent_prop_mod
        # (and transitively ccpp_hashable / ccpp_hash_table); host code that
        # calls ccpp_constituent_index pulls in ccpp_scheme_utils.  Add all
        # framework F90 dependencies so the host build picks them up.
        utility_paths.extend(_resolve_framework_f90_files())
    host_file_paths = [
        os.path.join(abs_root, '{}_ccpp_cap.F90'.format(host_name)),
    ]
    suite_file_paths = []
    suite_meta_paths = []
    for sname, suite_resolution in zip(suite_names, suite_resolutions):
        suite_file_paths.append(
            os.path.join(abs_root, 'ccpp_{}_cap.F90'.format(sname))
        )
        suite_file_paths.append(
            os.path.join(abs_root, 'ccpp_{}_data.F90'.format(sname))
        )
        # Types module is only present when optional args exist.
        types_file = os.path.join(abs_root, 'ccpp_{}_types.F90'.format(sname))
        if os.path.isfile(types_file):
            suite_file_paths.append(types_file)
        for resolved_group in suite_resolution.groups:
            suite_file_paths.append(
                os.path.join(
                    abs_root,
                    'ccpp_{}_{}_cap.F90'.format(sname, resolved_group.group_name),
                )
            )
        suite_meta_paths.append(
            os.path.join(abs_root, 'ccpp_{}_data.meta'.format(sname))
        )
    # Expanded SDFs (one per parsed suite) are inspection artifacts; carry
    # the paths set by parse_suite_xml() forward into datatable.xml.
    expanded_sdf_paths = [s.expanded_file for s in suites if s.expanded_file]
    # Collect dependency paths.  Host/control/ddt tables always contribute
    # (their Fortran is shared across suites).  Scheme-type tables only
    # contribute when the scheme is actually referenced by a resolved
    # suite — group phase calls, the suite-level <init> scheme, or the
    # suite-level <final> scheme.  DDT tables co-located in a scheme
    # ``.meta`` file (``type = ddt`` block alongside ``type = scheme``
    # blocks) always contribute since DDT modules are shared host-side
    # data, not gated on which scheme uses them.  Unreferenced scheme
    # metadata may sit on the CLI line for build-system convenience; we
    # don't want its dependencies to leak into datatable.xml.  Duplicates
    # are collapsed by ``write_datatable``.
    used_scheme_names: set = set()
    for suite_resolution in suite_resolutions:
        for resolved_group in suite_resolution.groups:
            for items in resolved_group.phase_calls.values():
                for resolved_call in iter_phase_calls(items):
                    used_scheme_names.add(resolved_call.scheme_name)
        if suite_resolution.suite_init_call is not None:
            used_scheme_names.add(suite_resolution.suite_init_call.scheme_name)
        if suite_resolution.suite_final_call is not None:
            used_scheme_names.add(suite_resolution.suite_final_call.scheme_name)

    dependency_paths = []
    for tbl in host_tables:
        dependency_paths.extend(tbl.dependencies)
    for tbl in scheme_tables:
        if tbl.table_type != 'scheme':
            # DDT (or other non-scheme) tables that live alongside scheme
            # tables in scheme metadata files — always contribute.
            dependency_paths.extend(tbl.dependencies)
        elif tbl.table_name in used_scheme_names:
            dependency_paths.extend(tbl.dependencies)

    # Used-scheme Fortran source paths.  Convention (shared with the
    # validator's ``_fortran_file_for_table``): the ``.F90`` (or ``.F`` /
    # ``.f90`` / ``.f``) lives under ``table.source_path`` with the same
    # base name as the ``.meta`` file.  Multiple scheme tables in one
    # ``.meta`` share that single source file, so dedupe per path.
    scheme_file_paths: List[str] = []
    _seen_scheme_files: set = set()
    for tbl in scheme_tables:
        # DDT tables inside scheme .meta files do not correspond to a
        # scheme .F90 — skip them outright.
        if tbl.table_type != 'scheme':
            continue
        if tbl.table_name not in used_scheme_names:
            continue
        meta_base = os.path.splitext(os.path.basename(tbl.file_path))[0]
        search_dir = tbl.source_path or os.path.dirname(
            os.path.abspath(tbl.file_path)
        )
        resolved = None
        for ext in ('.F90', '.f90', '.F', '.f'):
            candidate = os.path.join(search_dir, meta_base + ext)
            if os.path.isfile(candidate):
                resolved = candidate
                break
        if resolved is None:
            # Fall back to the canonical .F90 guess so the build-system
            # query returns a useful (if missing) path; surface the gap
            # at the same time so the user can fix source_path.
            resolved = os.path.join(search_dir, meta_base + '.F90')
            log.warning(
                "Scheme '%s': no Fortran source found under '%s' for "
                "any of .F90/.f90/.F/.f; using '%s' as the datatable "
                "entry.  Check the scheme's source_path table-property.",
                tbl.table_name, search_dir, resolved,
            )
        if resolved not in _seen_scheme_files:
            _seen_scheme_files.add(resolved)
            scheme_file_paths.append(resolved)

    write_datatable(
        suite_resolutions, scheme_store, utility_paths, suite_file_paths,
        output_root, host_file_paths=host_file_paths,
        scheme_file_paths=scheme_file_paths,
        dependency_paths=dependency_paths,
        suite_meta_paths=suite_meta_paths,
        expanded_sdf_paths=expanded_sdf_paths,
        host_dict=host_dict, host_name=host_name,
        logger=log,
    )

    log.info("Cap generation complete.")

    # When *return_state* is requested, hand the resolved state back
    # to the caller so external tools (host-side compat adapters,
    # debug utilities, downstream code generators) can consume the
    # in-memory ``host_dict`` and ``suite_resolutions`` without
    # re-running the load + resolve passes.  Returns ``None``
    # otherwise: the canonical signature is "side effects only".
    if return_state:
        return host_dict, suite_resolutions
    return None


def main(argv: Optional[List[str]] = None) -> int:
    """Command-line entry point.

    Parameters
    ----------
    argv : list of str, optional
        Override ``sys.argv[1:]`` (used by tests).

    Returns
    -------
    int
        Exit code: 0 = success, 1 = user error, 2 = internal error.
    """
    parser = _build_arg_parser()
    args = parser.parse_args(argv)

    # ---- configure logging -------------------------------------------------
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

    # dim-aliases: transient GFS-physics shim.  Emit the loud banner
    # before any parsing happens so user has fair warning.
    if args.gfs_dim_aliases:
        from metadata import dim_aliases
        dim_aliases.enable(_LOGGER)

    # auto-clone-constituents: transient legacy shim.  Emit the loud
    # banner before any parsing happens so the user has fair warning.
    # The single-instance assertion (host MUST NOT declare the
    # instance_number / number_of_instances pair) runs later, after
    # host metadata has been parsed, in ``capgen()``.
    if args.legacy_auto_clone_constituents:
        from metadata import auto_clone_constituents
        auto_clone_constituents.enable(_LOGGER)

    # ---- parse kind types --------------------------------------------------
    try:
        kind_types = _parse_kind_types(args.kind_type)
    except CCPPError as exc:
        _LOGGER.error("%s", exc)
        return 1

    # ---- call the generator ------------------------------------------------
    try:
        capgen(
            host_name=args.host_name,
            host_files=_split_file_list(args.host_files),
            scheme_files=_split_file_list(args.scheme_files),
            suite_files=_split_file_list(args.suites),
            output_root=args.output_root,
            kind_types=kind_types,
            no_host_introspection=args.no_host_introspection,
            trace=args.trace,
        )
    except CCPPError as exc:
        _LOGGER.error("%s", exc)
        return 1
    except Exception as exc:  # pylint: disable=broad-except
        _LOGGER.error("Internal error: %s", exc, exc_info=True)
        return 2

    return 0


if __name__ == '__main__':
    sys.exit(main())
