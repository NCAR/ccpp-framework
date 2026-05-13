#!/usr/bin/env python3

"""Variable resolution and access-path construction for ccpp-capgen-ng.

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

``horizontal_dimension``, ``horizontal_loop_extent``
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

#: Dimension standard names that indicate a DDT is indexed by instance.
#: The host access path gains a ``(instance_number)`` subscript.
#: See Section 4.3 ("instance_dimension") and the Section 3.5 example
#: ("number_of_instances").
_INSTANCE_DIMS: frozenset = frozenset({
    'instance_dimension',
    'number_of_instances',
})

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
    For slice expressions like ``chunk_begin(ccpp_chunk_number)`` returns
    (``'chunk_begin'``, ``'ccpp_chunk_number'``).

    >>> _split_local_name('chunk_begin')
    ('chunk_begin', '')
    >>> _split_local_name('chunk_begin(ccpp_chunk_number)')
    ('chunk_begin', 'ccpp_chunk_number')
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
    >>> d = {'ccpp_chunk_number': E('inst_num')}
    >>> _resolve_subscript('ccpp_chunk_number', d)
    'inst_num'
    >>> _resolve_subscript(':, ccpp_chunk_number', d)
    ':, inst_num'
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


def _instance_subscript(var: MetaVar) -> str:
    """Return ``'(instance_number)'`` if *var* is a DDT instance array, else ``''``.

    A variable is treated as a DDT instance array when any of its declared
    dimension standard names matches one of the :data:`_INSTANCE_DIMS` names.
    """
    for dim in var.dimensions:
        if dim in _INSTANCE_DIMS:
            return '(instance_number)'
    return ''


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


def build_ddt_module_map(
    all_tables: List[MetadataTable],
) -> Dict[str, str]:
    """Build a map from DDT type name → Fortran module that defines it.

    A DDT table inherits its defining Fortran module from a co-located
    ``host``, ``control``, or ``scheme`` table in the same ``.meta`` file.
    The convention is that a CCPP scheme/host/control table's name is the
    name of the Fortran module that contains it; a DDT type defined alongside
    such a table is assumed to be defined in the same Fortran module.

    DDT tables in a file with no co-located scheme/host/control table are
    skipped (no entry written).  DDTs that are only referenced as types of
    host instance variables (declared in the host's own Fortran code) do not
    need an entry — the host's Fortran code already imports the type.

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
        module_name: Optional[str] = None
        for tbl in tables:
            if tbl.table_type in ('scheme', 'host', 'control'):
                module_name = tbl.table_name
                break
        if module_name is None:
            continue
        for tbl in tables:
            if tbl.table_type == 'ddt':
                result[tbl.table_name] = module_name
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
            # Resolve module: explicit ``module_name`` from the table
            # properties overrides the implicit "module name equals scheme
            # name" convention.  See doc/scheme metadata format.
            mod = tbl.module_name.strip() if tbl.module_name else ''
            store._modules[name] = mod or name
            for sec in tbl.sections():
                if sec.phase is None:
                    continue
                if sec.phase in store._data[name]:
                    raise CCPPError(
                        "Duplicate phase '{}' for scheme '{}'; "
                        "check that the same scheme metadata is not loaded twice".format(
                            sec.phase, name
                        )
                    )
                store._data[name][sec.phase] = list(sec.variables)
        return store

    def scheme_names(self) -> List[str]:
        """Return sorted list of all known scheme names."""
        return sorted(self._data.keys())

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
