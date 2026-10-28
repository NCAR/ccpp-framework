#!/usr/bin/env python3

"""Variable resolution and access-path construction for ccpp-capgen.

This module flattens the host/control/DDT metadata hierarchy into a single
keyed dictionary and provides the ``SchemeStore`` lookup table that the code
generator uses to resolve scheme arguments.

Public API
----------
``HostVarEntry``
    One resolved variable in the flat host+control dictionary.

``build_flat_host_dict(host_tables, control_tables, ddt_tables)``
    Flatten host + control tables (expanding DDT instances into their fields)
    into a ``Dict[str, HostVarEntry]`` keyed by standard name.

``SchemeStore``
    Organises scheme metadata by scheme name and phase for O(1) lookup during
    variable resolution.

Registered dimension standard names (Section 4.3 of the redesign spec)
------------------------------------------------------------------------
The generator has built-in semantic knowledge of these standard names when they
appear as dimensions in host variables:

``instance_dimension``
    Scalar extraction — the DDT instance array is subscripted with the
    ``instance_number`` control variable:
    ``gfs_statein(instance_number)%field``

    Note: the sample metadata in section 3.5 of the spec uses
    ``number_of_instances`` for the same role.  Both names are treated as
    instance dimensions here; see ``_INSTANCE_DIMS``.

``horizontal_dimension``
    Horizontal slice — emitted as ``lb:ub`` (run phase) or
    ``1:<local_name>`` (non-run).  The code generator handles the slicing;
    the resolver only records the dimension standard name.

``vertical_*``
    Vertical slice — emitted as ``1:<local_name>``.  Same handling.

All other dimension standard names are "arbitrary" — the resolver looks up
the corresponding variable's local name and the code generator emits
``1:<local_name>``.
"""

import re
from typing import Dict, List, Optional

from .metadata_table import MetaVar, MetadataTable
from .parse_tools import CCPPError, check_fortran_intrinsic, FORTRAN_SCALAR_REF_RE

########################################################################
# Registered dimension constants
########################################################################

# The set of registered scalar-index dimensions (e.g.
# ``number_of_instances`` → ``instance_number``,
# ``number_of_threads`` → ``thread_number``) lives in a single
# documented module so the contract is easy for users and developers to
# find and extend.  See ``capgen/metadata/registered_dimensions.py``
# for the full table and the two rules that govern it.
from .registered_dimensions import (
    SCALAR_INDEX_DIMS,
    scalar_index_for,
    is_scalar_index_dim,
    registered_count_dims,
)

#: Regex that normalises ``type(typename)`` → ``typename``.
_TYPE_PAREN_RE = re.compile(
    r'(?i)^type\s*\(\s*([A-Za-z][A-Za-z0-9_]*)\s*\)$'
)

#: Regex for ``external:module:typename`` type syntax.
_EXTERNAL_RE = re.compile(r'^external\s*:', re.IGNORECASE)

########################################################################
# Helpers
########################################################################

def _ddt_typename(type_str: str) -> str:
    """Return the bare DDT type name, stripping ``type(...)`` if present."""
    m = _TYPE_PAREN_RE.match(type_str.strip())
    return m.group(1) if m else type_str.strip()


def _is_intrinsic(type_str: str) -> bool:
    """Return True if *type_str* is a Fortran intrinsic type name."""
    return check_fortran_intrinsic(type_str.strip(), error=False) is not None


def _is_external(type_str: str) -> bool:
    """Return True if *type_str* uses the ``external:module:typename`` syntax."""
    return bool(_EXTERNAL_RE.match(type_str.strip()))


def _is_known_ddt(type_str: str, ddt_index: Dict[str, MetadataTable]) -> bool:
    """Return True if *type_str* refers to a DDT type present in *ddt_index*."""
    if _is_intrinsic(type_str) or _is_external(type_str):
        return False
    return _ddt_typename(type_str) in ddt_index


def _split_local_name(local_name: str):
    """Split a local name into (base, subscript) tuple.

    For plain identifiers returns (local_name, '').
    For slice expressions like ``field(idx)`` returns
    (``'field'``, ``'idx'``).

    >>> _split_local_name('field')
    ('field', '')
    >>> _split_local_name('field(idx)')
    ('field', 'idx')
    >>> _split_local_name('q(:,:,index_of_water_vapor_specific_humidity)')
    ('q', ':,:,index_of_water_vapor_specific_humidity')
    """
    m = FORTRAN_SCALAR_REF_RE.match(local_name)
    if m is None:
        return local_name, ''
    return m.group(1), m.group(2).rstrip()


def _resolve_subscript(subscript: str, host_dict: Dict[str, 'HostVarEntry']) -> str:
    """Replace each standard-name token in *subscript* with its local name.

    Tokens that are ``:`` (colon slices) or integer literals are left as-is.
    Tokens that are standard names present in *host_dict* are replaced by
    the entry's ``local_name``.  Unrecognised tokens are left as-is (they
    may be integer constants or already-local names).

    >>> from collections import namedtuple
    >>> E = namedtuple('E', ['local_name'])
    >>> d = {'thread_number': E('thrd_no')}
    >>> _resolve_subscript('thread_number', d)
    'thrd_no'
    >>> _resolve_subscript(':, thread_number', d)
    ':, thrd_no'
    >>> _resolve_subscript('1', d)
    '1'
    """
    tokens = [t.strip() for t in subscript.split(',')]
    resolved = []
    for token in tokens:
        if token == ':' or token.isdigit():
            resolved.append(token)
        elif token in host_dict:
            resolved.append(host_dict[token].local_name)
        else:
            resolved.append(token)
    return ', '.join(resolved)


def _validate_leaf_dims(var: 'MetaVar', source_label: str) -> None:
    """Reject leaf variables that declare a registered scalar-index dim.

    Rule 2 of the registered-scalar-index-dimension contract (see
    :mod:`metadata.registered_dimensions`): a *leaf* variable — one that
    a physics scheme actually binds to (intrinsic-typed or ``external:``
    Fortran type) — MUST NOT declare a dim like ``number_of_instances``
    or ``number_of_threads``.  Those dims belong on container
    DDT-instance variables in the access path, never on the leaf data
    itself.

    Raises
    ------
    CCPPError
        With a message that names the offending variable, the offending
        dim, the paired index variable, the source label (host file /
        DDT table where the leaf was declared), and a pointer back to
        :mod:`metadata.registered_dimensions` for the full table and
        the remediation pattern.
    """
    offenders = [d for d in var.dimensions if is_scalar_index_dim(d)]
    if not offenders:
        return
    dim = offenders[0]
    idx_std = scalar_index_for(dim)
    raise CCPPError(
        "Variable '{name}' (standard_name='{std}', declared in {src}) "
        "is a leaf data variable but its dimensions list includes "
        "'{dim}', a registered scalar-index dimension reserved for "
        "DDT-instance container variables (paired with index "
        "'{idx}').\n"
        "\n"
        "Leaf variables (intrinsic- or external-typed, the kind a "
        "physics scheme binds to) MUST NOT carry registered scalar-"
        "index dimensions.  Wrap '{name}' in a container DDT whose "
        "dimensions = ({dim}), and declare '{name}' inside that DDT "
        "with only its spatial / tracer / count dims.  The generator "
        "will emit '<container>({idx})%{name}(...)' at every scheme "
        "call site automatically.\n"
        "\n"
        "See capgen/metadata/registered_dimensions.py for the full "
        "table of registered scalar-index pairings and how to extend "
        "it.".format(
            name=var.local_name,
            std=var.standard_name,
            src=source_label,
            dim=dim,
            idx=idx_std,
        )
    )


def _instance_subscript(var: MetaVar) -> str:
    """Return the scalar-index subscript for a container DDT-instance variable.

    Walks *var*'s declared dimensions in order; for each dim that is a
    registered scalar-index dim (see
    :mod:`metadata.registered_dimensions`), emits the paired index
    variable's standard name as a placeholder.  The placeholder is
    resolved to the host's local Fortran name at codegen time by
    :func:`generator.suite_resolver._substitute_scalar_idx`.

    Returns
    -------
    str
        Subscript string such as ``'(instance_number)'``,
        ``'(thread_number)'``, or for multi-pair containers
        ``'(instance_number, thread_number)'`` — one component per
        registered scalar-index dim found in *var.dimensions* in
        declared order.  Returns ``''`` when no registered scalar dim
        is present (the caller is left to handle non-registered dims
        through the normal slice machinery).
    """
    parts = []
    for dim in var.dimensions:
        idx = scalar_index_for(dim)
        if idx is not None:
            parts.append(idx)
    if not parts:
        return ''
    return '({})'.format(', '.join(parts))


########################################################################
# HostVarEntry
########################################################################

class HostVarEntry:
    """One resolved variable in the flat host+control dictionary.

    All fields are set at construction time and treated as read-only
    afterwards.

    Parameters
    ----------
    standard_name : str
        CF-compliant standard name (key in the flat dict).
    local_name : str
        Fortran local name of the innermost variable (e.g. ``'phii'``).
    access_path : str
        Fully-qualified Fortran access expression, with any DDT
        component separators and instance subscripts applied
        (e.g. ``'gfs_statein(instance_number)%phii'``).  For plain
        variables this equals *local_name*.
    module_name : str or None
        Fortran module that exports this variable, used to emit
        ``use <module>, only: <top_name>`` in the generated cap.
        ``None`` for control variables (passed as subroutine arguments).
    type : str
        Fortran type string.
    kind : str
        Optional kind parameter (empty string if not specified).
    units : str
        Physical units.
    dimensions : list of str
        Ordered dimension standard names; empty for scalars.
    protected : bool
        Whether any scheme is forbidden from declaring ``intent`` other
        than ``in`` for this variable.
    optional : bool
        Whether the variable may be absent (uses optional pointer in cap).
    allocatable : bool
        Whether the variable is declared with the Fortran ``allocatable``
        attribute.  Host and scheme metadata must agree.  Affects code
        generation: actual arguments at call sites omit explicit dimension
        subscripts for allocatable variables.
    active : str
        Fortran conditional expression in standard names; empty if always
        active.
    """

    __slots__ = (
        'standard_name', 'local_name', 'access_path', 'module_name',
        'type', 'kind', 'units', 'dimensions',
        'protected', 'optional', 'allocatable', 'active', 'local_subscript',
        'top_at_one',
    )

    def __init__(
        self,
        standard_name: str,
        local_name: str,
        access_path: str,
        module_name: Optional[str],
        type_: str,
        kind: str,
        units: str,
        dimensions: List[str],
        protected: bool,
        optional: bool,
        active: str,
        local_subscript: Optional[List[str]] = None,
        allocatable: bool = False,
        top_at_one: bool = False,
    ):
        self.standard_name    = standard_name
        self.local_name       = local_name
        self.access_path      = access_path
        self.module_name      = module_name
        self.type             = type_
        self.kind             = kind
        self.units            = units
        self.dimensions       = list(dimensions)
        self.protected        = protected
        self.optional         = optional
        self.allocatable      = allocatable
        self.active           = active
        self.local_subscript  = list(local_subscript) if local_subscript else []
        self.top_at_one       = top_at_one

    @property
    def is_control(self) -> bool:
        """True when this variable is a control variable (no module USE needed)."""
        return self.module_name is None

    def __repr__(self) -> str:
        return "HostVarEntry({!r}, access_path={!r})".format(
            self.standard_name, self.access_path
        )

    def __eq__(self, other: object) -> bool:
        if not isinstance(other, HostVarEntry):
            return NotImplemented
        return self.standard_name == other.standard_name

    def __hash__(self) -> int:
        return hash(self.standard_name)


########################################################################
# DDT index
########################################################################

def _build_ddt_index(ddt_tables: List[MetadataTable]) -> Dict[str, MetadataTable]:
    """Build a dict from DDT type name → ``MetadataTable`` for O(1) lookup."""
    return {tbl.table_name: tbl for tbl in ddt_tables}


def _resolve_module_name(tbl: MetadataTable) -> str:
    """Return the Fortran module that exports *tbl*'s symbols.

    Honors the per-table ``module_name = …`` override from
    ``[ccpp-table-properties]`` when present (the
    ``design_module_name_override`` rule); otherwise falls back to the
    table name (the implicit "module name = table name" convention).
    """
    return (tbl.module_name or '').strip() or tbl.table_name


def build_ddt_module_map(
    all_tables: List[MetadataTable],
) -> Dict[str, str]:
    """Build a map from DDT type name → Fortran module that defines it.

    Resolution order, per DDT table:

    1. **DDT's own override.**  If the DDT's own ``[ccpp-table-properties]``
       carries ``module_name = …``, that wins.  Most specific source — a
       DDT may genuinely live in a different Fortran module than the
       scheme/host its ``.meta`` is paired with.  Required when the DDT
       lives in a file with no co-located scheme/host/control table at
       all (real-world example: CCPP-physics
       ``Radiation/RRTMG/radsw_param.meta`` declares ``cmpfsw_type`` in
       Fortran ``module module_radsw_parameters``, with no co-located
       scheme metadata).
    2. **Co-located table's resolved module.**  Failing the DDT's own
       override, inherit from a co-located ``host``, ``control``, or
       ``scheme`` table in the same ``.meta`` file.  Its module is
       resolved by the same rule used elsewhere in capgen
       (:func:`_resolve_module_name`): the co-located table's own
       ``module_name = …`` if declared, else its table name.

    DDT tables that pass neither rule are skipped (no entry written).
    Those DDTs are only safe to leave out when no generator output
    references them directly — e.g. a DDT referenced only as the type
    of a host instance variable, where the host's own Fortran already
    imports the type.

    What happens when both are present
    ----------------------------------

    +-------------------------+-------------------------+-----------------+
    | DDT ``module_name=``    | Co-located ``module_    | Result          |
    |                         | name=`` (or table name) |                 |
    +=========================+=========================+=================+
    | X (set)                 | Y (set or default)      | X — DDT wins    |
    +-------------------------+-------------------------+-----------------+
    | unset                   | Y (set or default)      | Y               |
    +-------------------------+-------------------------+-----------------+
    | X (set)                 | (no co-located table)   | X               |
    +-------------------------+-------------------------+-----------------+
    | unset                   | (no co-located table)   | (skipped)       |
    +-------------------------+-------------------------+-----------------+

    Parameters
    ----------
    all_tables : list of MetadataTable
        Mixed list of all parsed metadata tables (host, control, scheme,
        ddt).  ``suite`` tables are ignored.

    Returns
    -------
    dict mapping DDT type name → Fortran module name
    """
    by_file: Dict[str, List[MetadataTable]] = {}
    for tbl in all_tables:
        by_file.setdefault(tbl.file_path, []).append(tbl)

    result: Dict[str, str] = {}
    for fpath, tables in by_file.items():
        # Co-located non-DDT table provides the fallback module name.
        # Apply the same module_name-override-then-table-name resolution
        # that ``build_flat_host_dict`` uses so a host/scheme that
        # carries ``module_name = X`` is honored consistently.
        colocated_module: Optional[str] = None
        for tbl in tables:
            if tbl.table_type in ('scheme', 'host', 'control'):
                colocated_module = _resolve_module_name(tbl)
                break

        for tbl in tables:
            if tbl.table_type != 'ddt':
                continue
            # Per-table explicit override on the DDT itself wins.
            if tbl.module_name:
                result[tbl.table_name] = tbl.module_name
                continue
            if colocated_module is not None:
                result[tbl.table_name] = colocated_module
    return result


########################################################################
# DDT instance flattening
########################################################################

def _flatten_ddt_instance(
    var: MetaVar,
    module_name: str,
    ddt_index: Dict[str, MetadataTable],
    access_prefix: str = '',
    depth: int = 0,
    max_depth: int = 8,
) -> List[HostVarEntry]:
    """Expand a DDT instance variable into flat ``HostVarEntry`` objects.

    The DDT instance entry itself is included first, followed by one entry
    per field.  If a field is itself a known DDT type, it is expanded
    recursively (up to *max_depth* levels deep).

    Parameters
    ----------
    var : MetaVar
        The DDT instance variable from a host table section.
    module_name : str
        Fortran module from which the DDT instance is imported.
    ddt_index : dict
        Map from DDT type name → ``MetadataTable``, built by
        :func:`_build_ddt_index`.
    access_prefix : str
        Fortran access-path prefix accumulated from enclosing DDT levels,
        e.g. ``'outer(instance_number)%'``.
    depth, max_depth : int
        Recursion guard — raises ``CCPPError`` if exceeded.

    Returns
    -------
    list of HostVarEntry

    Raises
    ------
    CCPPError
        If the DDT type is not in *ddt_index*, or if the nesting depth
        exceeds *max_depth* (circular reference guard).
    """
    if depth > max_depth:
        raise CCPPError(
            "DDT hierarchy for '{}' exceeds maximum nesting depth {}; "
            "possible circular reference".format(var.standard_name, max_depth)
        )

    ddt_name = _ddt_typename(var.type)
    if ddt_name not in ddt_index:
        raise CCPPError(
            "Variable '{}' (standard_name='{}') has type '{}' but no "
            "matching 'type = ddt' table was found; "
            "add the DDT metadata file to --host-files or --scheme-files".format(
                var.local_name, var.standard_name, var.type
            )
        )

    ddt_table = ddt_index[ddt_name]
    subscript = _instance_subscript(var)
    # If the DDT instance has dimensions but NONE of them are a
    # registered scalar-index dim, capgen can't bake a meaningful
    # scalar subscript into field access paths.  Two outcomes are both
    # legitimate, depending on how schemes use this DDT:
    #
    #   (a) Schemes take the whole sliced DDT array as a single arg
    #       (e.g. ``call rad_lw_run(fluxLW=phys_state%fluxLW(lb:ub), …)``)
    #       and dereference inner fields inside the scheme.  Flattening
    #       this DDT's components into host_dict is wasted and emits
    #       Fortran the compiler rejects.
    #   (b) Schemes request individual inner fields by standard name,
    #       which would require ``parent%var(<idx>)%field(…)`` access
    #       with a meaningful ``<idx>`` capgen can't synthesize.
    #
    # Skip the recursion either way: the DDT-instance's own entry is
    # still recorded (case (a) just works), and case (b) trips the
    # resolver's existing "standard_name not found" error when a scheme
    # tries to use a would-have-been-flattened inner field.  Use
    # ``--legacy-mode`` (or fix the host metadata) when the underlying
    # cause is a deprecated dimension name like
    # ``number_of_openmp_threads``.
    skip_recurse = bool(var.dimensions) and not subscript
    # Fortran access path to this DDT instance (without field component).
    instance_access = access_prefix + var.local_name + subscript

    entries: List[HostVarEntry] = []

    # The DDT instance variable itself — keyed by its own standard_name.
    entries.append(HostVarEntry(
        standard_name=var.standard_name,
        local_name=var.local_name,
        access_path=access_prefix + var.local_name,
        module_name=module_name,
        type_=var.type,
        kind=var.kind,
        units=var.units,
        dimensions=var.dimensions,
        protected=var.protected,
        optional=var.optional,
        active=var.active,
        local_subscript=[],
        allocatable=var.allocatable,
        top_at_one=var.top_at_one,
    ))

    # When the DDT-instance carries non-registered dims (skip_recurse),
    # leave its fields un-flattened — only the DDT-instance entry above
    # is recorded.  Schemes taking the whole sliced DDT work via that
    # entry; schemes asking for inner fields by std_name trip the
    # resolver's standard "not found" error.
    if skip_recurse:
        return entries

    # Expand each field of the DDT.
    for sec in ddt_table.sections():
        for field in sec.variables:
            if _is_known_ddt(field.type, ddt_index):
                # Nested DDT — recurse.
                entries.extend(_flatten_ddt_instance(
                    field,
                    module_name,
                    ddt_index,
                    access_prefix=instance_access + '%',
                    depth=depth + 1,
                    max_depth=max_depth,
                ))
            else:
                # Rule 2: a leaf DDT field cannot carry a registered
                # scalar-index dim.  Surface the violation at parse time
                # with a clear remediation pointer.
                _validate_leaf_dims(
                    field,
                    "DDT '{}' (file: {})".format(
                        ddt_name,
                        ddt_table.file_path,
                    ),
                )
                base_field, sub_str = _split_local_name(field.local_name)
                sub_tokens = [t.strip() for t in sub_str.split(',') if t.strip()] if sub_str else []
                field_path = instance_access + '%' + base_field
                entries.append(HostVarEntry(
                    standard_name=field.standard_name,
                    local_name=base_field,
                    access_path=field_path,
                    module_name=module_name,
                    type_=field.type,
                    kind=field.kind,
                    units=field.units,
                    dimensions=field.dimensions,
                    protected=field.protected,
                    optional=field.optional,
                    active=field.active,
                    local_subscript=sub_tokens,
                    allocatable=field.allocatable,
                    top_at_one=field.top_at_one,
                ))

    return entries


########################################################################
# Public: build_flat_host_dict
########################################################################

def build_flat_host_dict(
    host_tables: List[MetadataTable],
    control_tables: List[MetadataTable],
    ddt_tables: List[MetadataTable],
) -> Dict[str, 'HostVarEntry']:
    """Build the flat host+control variable dictionary.

    All host and control variables are flattened into a single
    ``Dict[str, HostVarEntry]`` keyed by standard name.  DDT instance
    variables are expanded into one entry per field; the instance variable
    itself is also stored under its own standard name.

    Control variables have ``module_name=None`` — they are passed as
    subroutine arguments, not imported via ``use``.

    Parameters
    ----------
    host_tables : list of MetadataTable
        Tables with ``table_type == 'host'``.
    control_tables : list of MetadataTable
        Tables with ``table_type == 'control'``.
    ddt_tables : list of MetadataTable
        Tables with ``table_type == 'ddt'``.

    Returns
    -------
    dict mapping standard_name → HostVarEntry

    Raises
    ------
    CCPPError
        On duplicate standard names across tables, or on a DDT reference
        without a matching DDT table.
    """
    ddt_index = _build_ddt_index(ddt_tables)
    result: Dict[str, HostVarEntry] = {}

    def _add(entry: HostVarEntry, source_label: str) -> None:
        # A character variable whose storage is DEFINED by host or DDT
        # metadata must be concrete: ``len=*`` (assumed length) is illegal
        # for a host module variable or a derived-type component.  Control
        # variables are EXEMPT -- they are pass-through dummy arguments
        # (suite_name, errmsg, ...) which the generated caps legitimately
        # declare ``character(len=*)``.  Reject it here so the error names
        # the table rather than surfacing downstream as undeclarable Fortran.
        if (not entry.is_control
                and (entry.type or '').strip().lower() == 'character'
                and (entry.kind or '').strip() == 'len=*'):
            raise CCPPError(
                "Character variable '{}' (standard_name='{}') in table '{}' "
                "declares kind='len=*'; host and DDT metadata must give "
                "character variables a concrete length (e.g. kind=len=512) "
                "-- assumed length is valid only for dummy arguments (scheme "
                "args and control/lifecycle variables).".format(
                    entry.local_name, entry.standard_name, source_label,
                )
            )
        prior = result.get(entry.standard_name)
        if prior is not None:
            prior_loc = (
                "module '{}'".format(prior.module_name)
                if prior.module_name else '<control>'
            )
            new_loc = "module '{}'".format(entry.module_name) \
                if entry.module_name else '<control>'
            raise CCPPError(
                "Duplicate standard name '{}':\n"
                "  already registered from {} via access path '{}'\n"
                "  re-registered from {} (source: {}) via access path '{}'\n"
                "If both paths come from the same parent DDT, the parent "
                "likely declares two sibling DDT instances of the same "
                "type — components of each get flattened into the host "
                "dictionary under the same standard names.  Drop one of "
                "the sibling DDT instances, or give one a non-overlapping "
                "standard name on every component.".format(
                    entry.standard_name,
                    prior_loc, prior.access_path,
                    new_loc, source_label, entry.access_path,
                )
            )
        result[entry.standard_name] = entry

    # ---- host tables -------------------------------------------------------
    for tbl in host_tables:
        # Explicit ``module_name`` from ``[ccpp-table-properties]`` overrides
        # the convention "module name = table name"; falls back to the table
        # name when not declared.
        host_module = (tbl.module_name or '').strip() or tbl.table_name
        for sec in tbl.sections():
            for var in sec.variables:
                if _is_known_ddt(var.type, ddt_index):
                    for entry in _flatten_ddt_instance(
                        var, host_module, ddt_index
                    ):
                        _add(entry, tbl.table_name)
                elif _is_intrinsic(var.type) or _is_external(var.type):
                    _validate_leaf_dims(
                        var,
                        "host table '{}' (file: {})".format(
                            tbl.table_name, tbl.file_path,
                        ),
                    )
                    base_name, sub_str = _split_local_name(var.local_name)
                    sub_tokens = [t.strip() for t in sub_str.split(',') if t.strip()] if sub_str else []
                    _add(HostVarEntry(
                        standard_name=var.standard_name,
                        local_name=base_name,
                        access_path=base_name,
                        module_name=host_module,
                        type_=var.type,
                        kind=var.kind,
                        units=var.units,
                        dimensions=var.dimensions,
                        protected=var.protected,
                        optional=var.optional,
                        active=var.active,
                        local_subscript=sub_tokens,
                        allocatable=var.allocatable,
                        top_at_one=var.top_at_one,
                    ), tbl.table_name)
                else:
                    raise CCPPError(
                        "Variable '{}' (standard_name='{}') in table '{}' has "
                        "type '{}' which is not a Fortran intrinsic, not an "
                        "'external:' type, and has no matching 'type = ddt' table; "
                        "declare the DDT in a metadata file and pass it via "
                        "--host-files, or use 'type = external:<module>:<typename>' "
                        "for non-CCPP types".format(
                            var.local_name, var.standard_name,
                            tbl.table_name, var.type,
                        )
                    )

    # ---- control tables ----------------------------------------------------
    for tbl in control_tables:
        for sec in tbl.sections():
            for var in sec.variables:
                _validate_leaf_dims(
                    var,
                    "control table '{}' (file: {})".format(
                        tbl.table_name, tbl.file_path,
                    ),
                )
                base_name, sub_str = _split_local_name(var.local_name)
                sub_tokens = [t.strip() for t in sub_str.split(',') if t.strip()] if sub_str else []
                _add(HostVarEntry(
                    standard_name=var.standard_name,
                    local_name=base_name,
                    access_path=base_name,
                    module_name=None,
                    type_=var.type,
                    kind=var.kind,
                    units=var.units,
                    dimensions=var.dimensions,
                    protected=var.protected,
                    optional=var.optional,
                    active=var.active,
                    local_subscript=sub_tokens,
                    allocatable=var.allocatable,
                    top_at_one=var.top_at_one,
                ), tbl.table_name)

    return result


########################################################################
# SchemeStore
########################################################################

class SchemeStore:
    """Organises scheme metadata for variable resolution.

    Provides O(1) lookup of scheme arguments by scheme name and phase.
    Constructed via :meth:`build_from`.
    """

    def __init__(self) -> None:
        # _data[scheme_name][phase] = list of MetaVar (in metadata order)
        self._data: Dict[str, Dict[str, List[MetaVar]]] = {}
        # _modules[scheme_name] = Fortran module that exports the scheme's
        # subroutines.  Populated from the metadata table's ``module_name``
        # attribute (``[ccpp-table-properties]``) when declared; otherwise
        # falls back to the scheme name (the common case where the .meta
        # file shares its base name with the Fortran module).
        self._modules: Dict[str, str] = {}
        # _source_paths[scheme_name][phase] = .meta file that first
        # registered this (scheme, phase) pair.  Consulted only on the
        # duplicate-phase error path so the message can name both the
        # original registration site and the duplicate.
        self._source_paths: Dict[str, Dict[str, str]] = {}
        # Memoised set of constituent standard names (base + tendency;
        # see constituent_stdnames); None until first computed.
        self._const_stds: Optional[frozenset] = None

    @classmethod
    def build_from(cls, scheme_tables: List[MetadataTable]) -> 'SchemeStore':
        """Build a :class:`SchemeStore` from *scheme_tables*.

        Non-scheme tables are silently skipped so the caller may pass a
        mixed list without filtering.

        Parameters
        ----------
        scheme_tables : list of MetadataTable
            One or more metadata tables (only ``scheme`` tables are used).

        Returns
        -------
        SchemeStore
        """
        store = cls()
        for tbl in scheme_tables:
            if not tbl.is_scheme:
                continue
            name = tbl.table_name
            if name not in store._data:
                store._data[name] = {}
                store._source_paths[name] = {}
            # Resolve module: explicit ``module_name`` from the table
            # properties overrides the implicit "module name equals scheme
            # name" convention.  See doc/scheme metadata format.
            mod = tbl.module_name.strip() if tbl.module_name else ''
            store._modules[name] = mod or name
            for sec in tbl.sections():
                if sec.phase is None:
                    continue
                if sec.phase in store._data[name]:
                    first_path = store._source_paths[name].get(sec.phase, '<unknown>')
                    dup_path = tbl.file_path or '<unknown>'
                    # Same-path duplicate is the common case (a .meta
                    # file listed twice in the host's --scheme-files
                    # input, often a stray CMake list entry); call it
                    # out so the user knows to look in the build glue
                    # rather than in the metadata content.
                    if first_path == dup_path:
                        hint = (' (both paths are identical — likely a '
                                'duplicate entry in the --scheme-files '
                                'list passed to capgen)')
                    else:
                        hint = ''
                    raise CCPPError(
                        "Duplicate phase '{}' for scheme '{}': "
                        "first registered from '{}', then again from "
                        "'{}'.{}  Check that the same scheme metadata "
                        "is not loaded twice.".format(
                            sec.phase, name, first_path, dup_path, hint,
                        )
                    )
                store._data[name][sec.phase] = list(sec.variables)
                store._source_paths[name][sec.phase] = tbl.file_path or '<unknown>'
        return store

    def scheme_names(self) -> List[str]:
        """Return sorted list of all known scheme names."""
        return sorted(self._data.keys())

    def constituent_stdnames(self) -> frozenset:
        """Standard names that some scheme declares as a CONSTITUENT.

        A variable is a constituent when any scheme argument flags it
        ``is_constituent`` (``advected`` / ``constituent`` / ``molar_mass``):

        * a base constituent -- e.g. a mixing ratio flagged ``advected = true``
          (read via ``vars_layer``), or
        * a constituent tendency -- a ``tendency_of_*`` arg flagged
          ``constituent = true`` (read via ``vars_layer_tend``).

        A *consumer* of either (a scheme reading a mixing ratio, or a diagnostics
        scheme reading a tendency) must NOT re-flag it: whether a given standard
        name is a constituent or an ordinary variable is the host's decision
        (CAM-SIMA registers it as a constituent; CCPP-SCM may expose the same
        name as an ordinary host variable).  The suite resolver therefore infers
        constituent-ness from this scheme-metadata-wide set rather than from the
        consumer's own metadata.  Memoised; covers every loaded scheme because
        standard-name semantics are global.
        """
        if self._const_stds is None:
            result = set()
            for phases in self._data.values():
                for varlist in phases.values():
                    for var in varlist:
                        if getattr(var, 'is_constituent', False):
                            result.add(var.standard_name)
            self._const_stds = frozenset(result)
        return self._const_stds

    def module_for(self, name: str) -> str:
        """Return the Fortran module name that exports scheme *name*.

        Returns the explicit ``module_name`` from the scheme's
        ``[ccpp-table-properties]`` block when set, falling back to the
        scheme name (the common case where the .meta file's table name
        equals the Fortran module name).  Returns *name* unchanged for
        unknown schemes so callers always get a usable token for USE
        emission; whether the unknown scheme exists in Fortran is
        validated elsewhere (the cap simply fails to link).
        """
        return self._modules.get(name, name)

    def has_scheme(self, name: str) -> bool:
        """Return True if *name* is a known scheme."""
        return name in self._data

    def phases_for(self, name: str) -> List[str]:
        """Return sorted list of phases defined for scheme *name*.

        Returns an empty list for unknown schemes.
        """
        return sorted(self._data.get(name, {}).keys())

    def variables_for(self, name: str, phase: str) -> Optional[List[MetaVar]]:
        """Return the variable list for *name* / *phase*, or ``None``.

        The returned list preserves the metadata declaration order, which
        determines argument order in the generated scheme call.
        """
        phases = self._data.get(name)
        if phases is None:
            return None
        inner = phases.get(phase)
        return list(inner) if inner is not None else None

    def __repr__(self) -> str:
        return "SchemeStore(schemes={})".format(self.scheme_names())
