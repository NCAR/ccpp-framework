#!/usr/bin/env python3

"""Metadata table parser for ccpp-capgen.

Each ``.meta`` file contains one or more CCPP metadata tables.  Every table
begins with a ``[ccpp-table-properties]`` header, followed by one or more
``[ccpp-arg-table]`` section headers, each followed by variable blocks.

Supported table types
---------------------
``scheme``
    Physics scheme subroutine interfaces.  One ``[ccpp-arg-table]`` section
    per phase (``register``, ``init``, ``timestep_init``, ``run``,
    ``timestep_final``, ``final``).  All variables carry an ``intent``
    attribute.

``host``
    Host-model module data (replaces the old ``module`` type — using
    ``type = module`` is a hard error in this generator).

``control``
    Framework control variables passed explicitly as subroutine arguments
    (e.g. ``horizontal_loop_begin``, ``ccpp_error_code``).

``ddt``
    Derived data type (DDT) structural definition.  Describes field layout;
    no instance information.  Instances are declared as variables inside a
    ``host`` table.

``suite``
    Generator-written tables for suite-owned interstitial data.  Never
    hand-authored.

Format reference
----------------
::

    [ccpp-table-properties]
      name = <table_name>
      type = <table_type>

    [ccpp-arg-table]
      name = <section_name>   # scheme: <scheme_name>_<phase>; others: same as table_name
      type = <table_type>     # must match [ccpp-table-properties] type

    [ local_name ]
      standard_name = <cf_standard_name>
      long_name     = <human readable description>   # optional
      units         = <unit string>                  # optional; defaults to 'none'
      dimensions    = (<dim1>, <dim2>, ...)           # () for scalar
      type          = <fortran_type_or_ddt_type_name>
      kind          = <fortran_kind>                  # optional
      intent        = in | out | inout                # required for scheme vars
      optional      = True | False                    # default False
      active        = <fortran_conditional>           # optional; uses standard names
      protected     = True | False                    # default False
      allocatable   = True | False                    # default False
      diagnostic_name       = <fortran_id_or_template> # optional; host-tooling hint
      diagnostic_name_fixed = <fortran_id>            # optional; mutually exclusive
                                                       #   with diagnostic_name

Multiple properties may appear on one line, separated by ``|``.

DDT instance entries in a ``host`` table use a DDT type name as ``type``, and
may declare ``dimensions = (number_of_instances)`` for array instances.

External (non-CCPP) DDT types use the syntax::

    type = external:<module_name>:<type_name>

e.g. ``type = external:mpi_f08:mpi_comm``.
"""

import os
import re
from typing import Dict, List, Optional, Tuple

# legacy-compat: transient migration shim (delete with the rest of
# the legacy_compat touchpoints — grep for ``legacy-compat``).
from . import legacy_compat
# auto-clone-constituents: transient shim (delete with the rest of
# the auto-clone-constituents touchpoints — grep for
# ``auto-clone-constituents``).
from . import auto_clone_constituents
from .parse_tools import (
    CCPPError,
    ParseContext,
    ParseSyntaxError,
    check_cf_standard_name,
    check_units,
    check_dimensions,
    check_diagnostic_fixed,
    check_diagnostic_id,
    check_fortran_id,
    check_fortran_ref,
    check_fortran_intrinsic,
    check_molar_mass,
    # auto-clone-constituents: legacy-shim checkers.
    check_default_value,
    check_min_value,
    check_water_species,
    check_mixing_ratio_type,
)

########################################################################
# Module-level constants
########################################################################

#: All table type values accepted by the parser.
VALID_TABLE_TYPES = frozenset({'scheme', 'host', 'control', 'suite', 'ddt'})

#: Table types that have exactly one ``[ccpp-arg-table]`` section per table.
SINGLETON_TABLE_TYPES = frozenset({'host', 'control', 'suite', 'ddt'})

#: The scheme table type (the only type with multiple sections).
SCHEME_TABLE_TYPE = 'scheme'

#: Valid scheme phase suffixes.  ``finalize`` is renamed to ``final``; using
#: ``<name>_finalize`` in a section name is a hard error.
VALID_SCHEME_PHASES = frozenset({
    'register', 'init', 'timestep_init', 'run', 'timestep_final', 'final'
})

#: Valid intent values for scheme variables.
VALID_INTENTS = frozenset({'in', 'out', 'inout'})

#: Maximum allowed length for a Fortran identifier (F2018 §6.1.1).
FORTRAN_MAX_IDENT_LEN = 63

#: Regex for a bare section header ``[ name ]``.
_VAR_HEADER_RE = re.compile(r"^\[\s*(\S+)\s*\]\s*$")

#: Regex for the reserved section keywords.
_TABLE_PROPS_HDR = '[ccpp-table-properties]'
_ARG_TABLE_HDR   = '[ccpp-arg-table]'

#: Lines that are blank or start with a comment character.
_BLANK_RE = re.compile(r"^\s*([#;].*)?$")

#: ``external:module:typename`` DDT type syntax.
_EXTERNAL_TYPE_RE = re.compile(
    r'^external\s*:\s*([A-Za-z][A-Za-z0-9_]*)\s*:\s*([A-Za-z][A-Za-z0-9_]*)$',
    re.IGNORECASE,
)

#: ``kind_spec`` value in ``[ccpp-table-properties]``.  Two accepted forms:
#:
#:   <module>:<kind_name>=>spec   -- explicit CCPP-visible kind name
#:   <module>:<spec>              -- shorthand; kind_name defaults to spec
#:
#: Captured groups: (module, kind_name_or_None, spec).  When the second
#: group is None the caller substitutes ``spec`` for ``kind_name``.
_KIND_SPEC_RE = re.compile(
    r'^\s*([A-Za-z][A-Za-z0-9_]*)\s*:\s*'
    r'(?:([A-Za-z][A-Za-z0-9_]*)\s*=>\s*)?'
    r'([A-Za-z][A-Za-z0-9_]*)\s*$'
)

########################################################################
# Helper functions
########################################################################

def _is_blank(line: str) -> bool:
    """Return True for blank lines and comment-only lines.

    >>> _is_blank('')
    True
    >>> _is_blank('  ')
    True
    >>> _is_blank('# comment')
    True
    >>> _is_blank('; comment')
    True
    >>> _is_blank('  name = foo')
    False
    """
    return _BLANK_RE.match(line) is not None


def _strip_inline_comment(line: str) -> str:
    """Drop a trailing ``# ...`` comment from a metadata line.

    Metadata files use ``#`` (and ``;`` at column 0) as comment markers.
    A ``#`` anywhere in a line — not just at column 0 — starts a comment
    that runs to the end of the line; the parser discards it before any
    further processing.  No escape mechanism: ``#`` is not a legitimate
    character in any metadata value (units, kinds, identifiers, dim
    lists, Fortran conditional expressions).

    Trailing whitespace left behind by the strip is also removed so that
    section/variable headers like ``[ name ]`` and key=value lines parse
    cleanly with their existing regexes.

    >>> _strip_inline_comment('dimensions = () # (nap_indices)')
    'dimensions = ()'
    >>> _strip_inline_comment('[ ap_indices ]   # legacy slot')
    '[ ap_indices ]'
    >>> _strip_inline_comment('# whole-line comment')
    ''
    >>> _strip_inline_comment('plain line with no comment')
    'plain line with no comment'
    >>> _strip_inline_comment('')
    ''
    """
    idx = line.find('#')
    if idx < 0:
        return line
    return line[:idx].rstrip()


def _parse_bool(value: str, context: ParseContext) -> bool:
    """Parse a Fortran/Python boolean string to a Python bool.

    Accepts ``True``/``False`` (case-insensitive).

    >>> from metadata.parse_tools import ParseContext
    >>> ctx = ParseContext(0, 'test.meta')
    >>> _parse_bool('True', ctx)
    True
    >>> _parse_bool('false', ctx)
    False
    >>> _parse_bool('.true.', ctx)
    True
    >>> _parse_bool('.false.', ctx)
    False
    """
    normalized = value.strip().lower()
    if normalized in ('true', '.true.', 't', '1'):
        return True
    if normalized in ('false', '.false.', 'f', '0'):
        return False
    raise CCPPError(
        "Invalid boolean '{}', at {}".format(value, context)
    )


def _parse_kind_spec_value(
    value: str, context: ParseContext,
) -> Tuple[str, str, str]:
    """Parse one ``kind_spec`` value into ``(kind_name, module, spec)``.

    Accepted syntax::

        <module>:<kind_name>=>spec   # explicit CCPP-visible kind name
        <module>:<spec>              # kind_name defaults to spec

    All three components must be valid Fortran identifiers.  Whitespace
    around the separators is tolerated.

    >>> from metadata.parse_tools import ParseContext
    >>> ctx = ParseContext(0, 'test.meta')
    >>> _parse_kind_spec_value('temp_kinds:kind_temp=>temp_r8', ctx)
    ('kind_temp', 'temp_kinds', 'temp_r8')
    >>> _parse_kind_spec_value('host_kinds:kind_r8', ctx)
    ('kind_r8', 'host_kinds', 'kind_r8')
    >>> _parse_kind_spec_value('  temp_kinds : kind_temp => temp_r8  ', ctx)
    ('kind_temp', 'temp_kinds', 'temp_r8')
    >>> _parse_kind_spec_value('not_a_kind_spec', ctx)  # doctest: +ELLIPSIS
    Traceback (most recent call last):
        ...
    metadata.parse_tools.parse_source.CCPPError: Malformed kind_spec ...
    """
    match = _KIND_SPEC_RE.match(value)
    if match is None:
        raise CCPPError(
            "Malformed kind_spec '{}', at {}: expected "
            "<module>:<kind_name>=>spec or <module>:<spec>".format(
                value, context
            )
        )
    module    = match.group(1)
    kind_name = match.group(2)
    spec      = match.group(3)
    if kind_name is None:
        kind_name = spec
    return kind_name, module, spec


def _parse_dimensions(value: str, context: ParseContext) -> List[str]:
    """Parse a dimension list ``(d1, d2, ...)`` into a Python list.

    The empty list ``[]`` represents a scalar (``dimensions = ()``).

    >>> from metadata.parse_tools import ParseContext
    >>> ctx = ParseContext(0, 'test.meta')
    >>> _parse_dimensions('()', ctx)
    []
    >>> _parse_dimensions('(horizontal_dimension)', ctx)
    ['horizontal_dimension']
    >>> _parse_dimensions('(horizontal_dimension, vertical_layer_dimension)', ctx)
    ['horizontal_dimension', 'vertical_layer_dimension']

    CCPP standard names are case-insensitive; mixed-case spellings in
    the metadata are normalised to lower-case so downstream lookups
    against ``host_dict`` (which stores std names lower-cased per
    ``check_cf_standard_name``) succeed:

    >>> _parse_dimensions('(number_of_aerosol_tracers_MG)', ctx)
    ['number_of_aerosol_tracers_mg']
    >>> _parse_dimensions('(ccpp_constant_one:Vertical_Layer_Dimension)', ctx)
    ['ccpp_constant_one:vertical_layer_dimension']
    """
    stripped = value.strip()
    if not (stripped.startswith('(') and stripped.endswith(')')):
        raise ParseSyntaxError(
            "dimensions value (must be parenthesised list)", token=value,
            context=context
        )
    inner = stripped[1:-1].strip()
    if not inner:
        return []
    parts = [p.strip() for p in inner.split(',')]
    normalised: List[str] = []
    for part in parts:
        if not part:
            raise ParseSyntaxError(
                "empty dimension entry in '{}'".format(value),
                context=context
            )
        check_dimensions([part], None, error=True)
        # Lowercase every non-integer token so the resolver's
        # host_dict lookups succeed regardless of the user's metadata
        # casing.  Range form ``lower:upper`` lowercases each half;
        # integer literals are unchanged by ``.lower()``.
        #
        # legacy-compat: after lowercasing, run each token through the
        # legacy translator so ``horizontal_loop_extent`` (etc.) is
        # rewritten to its canonical name.  No-op when legacy mode is
        # disabled.
        normalised.append(':'.join(
            legacy_compat.translate(t.strip().lower())
            for t in part.split(':')
        ))
    return normalised


def _check_var_type(value: str, context: ParseContext) -> str:
    """Validate and normalise a variable ``type`` attribute.

    Accepts:
    * Fortran intrinsic types (case-insensitive, returned as given).
    * ``type(<identifier>)`` DDT references (returned as given).
    * Plain ``<identifier>`` DDT type names (returned as given).
    * ``external:<module>:<typename>`` for non-CCPP types.

    >>> from metadata.parse_tools import ParseContext
    >>> ctx = ParseContext(0, 'test.meta')
    >>> _check_var_type('real', ctx)
    'real'
    >>> _check_var_type('integer', ctx)
    'integer'
    >>> _check_var_type('gfs_statein_type', ctx)
    'gfs_statein_type'
    >>> _check_var_type('external:mpi_f08:mpi_comm', ctx)
    'external:mpi_f08:mpi_comm'
    """
    stripped = value.strip()
    # Fortran intrinsic?
    if check_fortran_intrinsic(stripped, error=False) is not None:
        return stripped
    # external:module:typename?
    if _EXTERNAL_TYPE_RE.match(stripped):
        return stripped
    # type(identifier) form?
    m = re.match(r'(?i)^type\s*\(\s*([A-Za-z][A-Za-z0-9_]*)\s*\)$', stripped)
    if m:
        return stripped
    # Plain DDT name (a valid Fortran identifier)?
    if check_fortran_id(stripped, None, error=False) is not None:
        return stripped
    raise ParseSyntaxError(
        "variable type", token=value, context=context
    )


def _parse_config_line(line: str, context: ParseContext) -> List[Tuple[str, str]]:
    """Parse one ini-format key=value line (possibly multiple pairs per line
    separated by ``|``).

    Returns a list of ``(key, value)`` pairs.

    >>> from metadata.parse_tools import ParseContext
    >>> ctx = ParseContext(0, 'test.meta')
    >>> _parse_config_line('  name = foo  ', ctx)
    [('name', 'foo')]
    >>> _parse_config_line('units = 1 | dimensions = ()', ctx)
    [('units', '1'), ('dimensions', '()')]
    >>> _parse_config_line('', ctx)
    []
    """
    if _is_blank(line):
        return []
    pairs = []
    for segment in line.split('|'):
        parts = segment.split('=', 1)
        if len(parts) != 2:
            raise ParseSyntaxError(
                "key=value pair", token=segment.strip(), context=context
            )
        key = parts[0].strip().lower()
        val = parts[1].strip()
        if not key:
            raise ParseSyntaxError(
                "empty key in property", token=segment.strip(), context=context
            )
        pairs.append((key, val))
    return pairs


########################################################################
# Core data classes
########################################################################

class MetaVar:
    """A single variable entry parsed from a CCPP metadata table.

    Attributes
    ----------
    local_name : str
        The Fortran local variable name (from the ``[ name ]`` header).
    standard_name : str
        CF-compliant standard name (lowercase).
    long_name : str
        Human-readable description (may be empty).
    units : str
        Physical units string.  Defaults to ``'none'`` if the metadata
        entry omits the ``units`` attribute.
    dimensions : list of str
        Ordered list of dimension standard names; empty list for scalars.
    type : str
        Fortran type string (intrinsic, DDT name, or ``external:m:t``).
    kind : str
        Optional Fortran kind parameter (empty string if absent).
    intent : str or None
        ``'in'``, ``'out'``, or ``'inout'`` for scheme variables; ``None``
        for host/ddt/control variables.
    optional : bool
        Whether the variable is optional (default ``False``).
    active : str
        Fortran conditional expression (in standard names) controlling when
        the variable is present; empty string if unconditionally active.
    protected : bool
        If ``True``, any scheme declaring ``intent`` other than ``in`` is a
        metadata error.
    allocatable : bool
        Whether the variable is declared with the Fortran ``allocatable``
        attribute (default ``False``).  Host and scheme metadata must agree
        on this flag for matching standard names.  Affects code generation:
        actual arguments at call sites omit explicit dimension subscripts
        for allocatable variables.
    diagnostic_name : str
        Optional host-tooling hint: the name under which the variable is
        exposed to the host model's diagnostic / history-output system.  May
        contain ``${process}`` or ``${scheme_name}`` substitutions.  Mutually
        exclusive with :attr:`diagnostic_name_fixed`.  When neither attribute
        is explicitly set this property defaults to :attr:`local_name`, so
        downstream consumers always see a non-empty value unless
        ``diagnostic_name_fixed`` was provided instead.
    diagnostic_name_fixed : str
        Like :attr:`diagnostic_name` but a bare Fortran identifier with no
        substitutions allowed.  Mutually exclusive with
        :attr:`diagnostic_name`.
    context : ParseContext
        Source location for diagnostic messages.

    Examples
    --------
    >>> from metadata.parse_tools import ParseContext
    >>> ctx = ParseContext(10, 'example.meta')
    >>> v = MetaVar('im', ctx)
    >>> v.set_attr('standard_name', 'horizontal_dimension', ctx)
    >>> v.set_attr('units', 'count', ctx)
    >>> v.set_attr('dimensions', '()', ctx)
    >>> v.set_attr('type', 'integer', ctx)
    >>> v.set_attr('intent', 'in', ctx)
    >>> v.standard_name
    'horizontal_dimension'
    >>> v.intent
    'in'
    >>> v.dimensions
    []
    """

    # Attributes that are boolean flags.
    _BOOL_ATTRS = frozenset({'optional', 'protected', 'allocatable'})

    # All recognised per-variable attributes.
    # ``diagnostic_name`` / ``diagnostic_name_fixed`` are host-tooling hints
    # propagated to ``datatable.xml``; the cap code itself does not consume
    # them.  The two are mutually exclusive.
    _KNOWN_ATTRS = frozenset({
        'standard_name', 'long_name', 'units', 'dimensions',
        'type', 'kind', 'intent', 'optional', 'active', 'protected',
        'allocatable',
        'diagnostic_name', 'diagnostic_name_fixed',
        'constituent', 'advected', 'molar_mass',
        'top_at_one',
    })

    # auto-clone-constituents: the legacy auto-clone shim extends the
    # accepted set with four ``%instantiate``-kwarg-mapped attrs
    # (default_value, min_value, water_species, mixing_ratio_type).
    # ``_known_attrs()`` returns the union when the shim is enabled,
    # the base set otherwise — strict mode keeps rejecting the legacy
    # names with the original "Unknown variable attribute" error.
    @classmethod
    def _known_attrs(cls):
        extra = auto_clone_constituents.extra_known_attrs()
        if extra:
            return cls._KNOWN_ATTRS | extra
        return cls._KNOWN_ATTRS

    def __init__(self, local_name: str, context: ParseContext):
        """Initialise with *local_name* from the ``[ name ]`` section header.

        The bracket header accepts either a bare Fortran identifier
        (``[foo]``) or a sliced reference (``[dqdt(:,:,index_of_<X>)]``).
        The base identifier must be a valid Fortran name within the
        63-char limit; subscript tokens may be CCPP standard names
        (which routinely exceed 63 chars) and are validated separately
        as references later — apply the length check only to the base.

        All other attributes are set later via :meth:`set_attr`.
        """
        # Step 1: validate the overall syntactic form (bare id or array
        # reference) without imposing length limits on subscript tokens.
        if check_fortran_ref(local_name, None, error=False, max_len=0) is None:
            raise ParseSyntaxError(
                "variable local name (must be a valid Fortran identifier "
                "or scalar array reference)",
                token=local_name, context=context
            )
        # Step 2: enforce the Fortran-identifier length limit on the
        # base name only (everything before the first ``(``).
        paren = local_name.find('(')
        base_name = local_name[:paren].strip() if paren >= 0 else local_name.strip()
        if len(base_name) > FORTRAN_MAX_IDENT_LEN:
            raise ParseSyntaxError(
                "variable local name base '{}' is longer than the "
                "Fortran identifier limit ({} chars)".format(
                    base_name, FORTRAN_MAX_IDENT_LEN,
                ),
                token=local_name, context=context
            )
        self.local_name: str   = local_name
        self.standard_name: str = ''
        self.long_name: str    = ''
        self.units: str        = 'none'
        self.dimensions: List[str] = []
        self.type: str         = ''
        self.kind: str         = ''
        self.intent: Optional[str] = None
        self.optional: bool    = False
        self.active: str       = ''
        self.protected: bool   = False
        self.allocatable: bool = False
        # Backing field for the :attr:`diagnostic_name` property.  ``set_attr``
        # writes the explicitly-supplied value here; the property layers the
        # ``local_name`` default on top.
        self._diagnostic_name: str      = ''
        self.diagnostic_name_fixed: str = ''
        # Constituent-related hints (scheme metadata only).  A variable is
        # treated as a constituent if any of ``constituent``, ``advected``, or
        # ``molar_mass`` is set to a non-default value — see
        # :attr:`is_constituent`.
        self.constituent: bool   = False
        self.advected: bool      = False
        self.molar_mass: float   = 0.0
        # ``top_at_one`` declares the vertical-axis ordering for arrays with a
        # vertical dimension.  ``True`` means the model top is at index 1 (k=1
        # topmost, k=nz surface); the default ``False`` means surface at 1
        # (k=1 surface, k=nz top).  When a scheme and the host disagree on
        # this flag the generator emits a vertical-flip transform that
        # substitutes the vertical index with ``<vdim_local> - k + 1`` on the
        # host-side access expression.
        self.top_at_one: bool    = False
        # auto-clone-constituents: legacy attrs accepted only when the
        # shim is enabled.  Backing fields use ``None`` as the
        # "not set" sentinel so the emitter can distinguish an
        # explicit value from a default (optional kwargs on
        # %instantiate are only passed when explicitly set).
        self.default_value: Optional[float]      = None
        self.min_value: Optional[float]          = None
        self.water_species: Optional[bool]       = None
        self.mixing_ratio_type: Optional[str]    = None
        self.context: ParseContext = context
        # Track which attributes have been explicitly set (for validation).
        self._set_attrs: set   = set()

    # ------------------------------------------------------------------
    def set_attr(self, key: str, value: str, context: ParseContext) -> None:
        """Store attribute *key* = *value* after validating the value.

        Parameters
        ----------
        key : str
            Lower-case attribute name.
        value : str
            Raw string value from the metadata file.
        context : ParseContext
            Source location (for error messages).

        Raises
        ------
        CCPPError
            On unknown attribute names or invalid values.
        """
        # auto-clone-constituents: consult the dynamic union so the
        # legacy attrs (default_value/min_value/water_species/
        # mixing_ratio_type) are accepted only when the shim is on.
        if key not in self._known_attrs():
            raise CCPPError(
                "Unknown variable attribute '{}' for '{}', at {}".format(
                    key, self.local_name, context
                )
            )
        if key in self._set_attrs:
            raise CCPPError(
                "Duplicate attribute '{}' for variable '{}', at {}".format(
                    key, self.local_name, context
                )
            )
        self._set_attrs.add(key)

        # Wrap the per-attribute validation so any CCPPError raised by a
        # check_X helper (which only sees the raw value) gets enriched
        # with the variable name, the attribute name, and the source
        # location.  Without this, a parse failure like an empty
        # ``units =`` line surfaces as a bare "'' is not a valid unit"
        # with no clue which file/line/variable is at fault.
        try:
            if key == 'standard_name':
                # legacy-compat: rewrite deprecated names (e.g.
                # horizontal_loop_extent → horizontal_dimension) when
                # legacy mode is enabled.  Applied *after*
                # check_cf_standard_name (which lowercases) so mixed-case
                # legacy spellings are captured.  No-op otherwise.
                self.standard_name = legacy_compat.translate(
                    check_cf_standard_name(value, None, error=True))
            elif key == 'long_name':
                self.long_name = value
            elif key == 'units':
                self.units = check_units(value, None, error=True)
            elif key == 'dimensions':
                self.dimensions = _parse_dimensions(value, context)
            elif key == 'type':
                self.type = _check_var_type(value, context)
            elif key == 'kind':
                self.kind = value.strip()
            elif key == 'intent':
                iv = value.strip().lower()
                if iv not in VALID_INTENTS:
                    raise CCPPError(
                        "Invalid intent '{}'; must be one of {}".format(
                            value, sorted(VALID_INTENTS),
                        )
                    )
                self.intent = iv
            elif key == 'optional':
                self.optional = _parse_bool(value, context)
            elif key == 'active':
                # Standard names elsewhere are canonicalised to lowercase by
                # check_cf_standard_name; an active expression references those
                # same names, so normalise here too. Fortran is case-insensitive,
                # so embedded logical operators/literals are unaffected.
                self.active = value.strip().lower()
            elif key == 'protected':
                self.protected = _parse_bool(value, context)
            elif key == 'allocatable':
                self.allocatable = _parse_bool(value, context)
            elif key == 'diagnostic_name':
                self._diagnostic_name = check_diagnostic_id(
                    value.strip(), self._prop_snapshot(), error=True
                )
            elif key == 'diagnostic_name_fixed':
                self.diagnostic_name_fixed = check_diagnostic_fixed(
                    value.strip(), self._prop_snapshot(), error=True
                )
            elif key == 'constituent':
                self.constituent = _parse_bool(value, context)
            elif key == 'advected':
                self.advected = _parse_bool(value, context)
            elif key == 'molar_mass':
                self.molar_mass = check_molar_mass(value.strip(), None, error=True)
            elif key == 'top_at_one':
                self.top_at_one = _parse_bool(value, context)
            # auto-clone-constituents: BEGIN legacy-shim attr setters.
            # Only reachable when ``_known_attrs()`` returned the
            # union, so the strict-mode unknown-attr error fires
            # before we get here when the shim is off.
            elif key == 'default_value':
                self.default_value = check_default_value(
                    value.strip(), None, error=True)
            elif key == 'min_value':
                self.min_value = check_min_value(
                    value.strip(), None, error=True)
            elif key == 'water_species':
                self.water_species = check_water_species(
                    value.strip(), None, error=True)
            elif key == 'mixing_ratio_type':
                self.mixing_ratio_type = check_mixing_ratio_type(
                    value.strip(), None, error=True)
            # auto-clone-constituents: END legacy-shim attr setters.
        except CCPPError as exc:
            # Avoid double-wrapping if the inner check already carried
            # the location (some helpers do; most don't).
            inner = str(exc)
            location = str(context)
            if location and location in inner:
                raise
            raise CCPPError(
                "Invalid metadata for variable '{name}', attribute "
                "'{key}' = '{value}', at {ctx}:\n  {inner}".format(
                    name=self.local_name or '<unknown>',
                    key=key,
                    value=value,
                    ctx=context,
                    inner=inner,
                )
            ) from exc

    # ------------------------------------------------------------------
    @property
    def is_constituent(self) -> bool:
        """Return True iff this variable is flagged as a constituent.

        A variable is treated as a constituent when any of the three
        constituent-hint attributes is set to a non-default value:

        * ``constituent = True``
        * ``advected    = True``
        * ``molar_mass != 0.0``

        Matches the rollup in the original capgen
        (``scripts/metavar.py`` ``__is_constituent``).
        """
        return self.constituent or self.advected or self.molar_mass != 0.0

    # ------------------------------------------------------------------
    @property
    def diagnostic_name(self) -> str:
        """Effective diagnostic name.

        Returns the explicitly-set value when one was provided.  Otherwise,
        when ``diagnostic_name_fixed`` is also unset, falls back to
        :attr:`local_name` (matching the original capgen
        ``local_name_to_diag_name`` default).  Returns an empty string only
        when ``diagnostic_name_fixed`` was provided instead.
        """
        if self._diagnostic_name:
            return self._diagnostic_name
        if self.diagnostic_name_fixed:
            return ''
        return self.local_name

    def _prop_snapshot(self) -> Dict[str, str]:
        """Return a dict snapshot of attributes the diagnostic checkers may inspect.

        The ``check_diagnostic_id`` and ``check_diagnostic_fixed`` helpers
        cross-validate the two diagnostic attributes and reference the
        variable's local and standard names in error messages.  The snapshot
        reports the *explicitly-set* ``diagnostic_name`` (not the
        ``local_name`` default) so the mutual-exclusion check fires only when
        the author actually set it.
        """
        return {
            'local_name'           : self.local_name,
            'standard_name'        : self.standard_name,
            'diagnostic_name'      : self._diagnostic_name,
            'diagnostic_name_fixed': self.diagnostic_name_fixed,
        }

    # ------------------------------------------------------------------
    def validate(self, require_intent: bool, context: ParseContext) -> None:
        """Check that all required attributes are present.

        Parameters
        ----------
        require_intent : bool
            If ``True`` (scheme variables), ``intent`` must be set.
        context : ParseContext
            Source location (for error messages).

        Raises
        ------
        CCPPError
            If any required attribute is missing.
        """
        required = {'standard_name', 'dimensions', 'type'}
        if require_intent:
            required.add('intent')
        missing = required - self._set_attrs
        if missing:
            raise CCPPError(
                "Variable '{}' is missing required attributes: {}, at {}".format(
                    self.local_name, sorted(missing), context
                )
            )

    # ------------------------------------------------------------------
    def __repr__(self) -> str:
        return "MetaVar({!r}, standard_name={!r})".format(
            self.local_name, self.standard_name
        )

    def is_external_ddt(self) -> bool:
        """Return True if this variable's type is an external (non-CCPP) DDT."""
        return _EXTERNAL_TYPE_RE.match(self.type) is not None

    def external_ddt_module(self) -> Optional[str]:
        """Return the module name for an external DDT type, or None."""
        m = _EXTERNAL_TYPE_RE.match(self.type)
        return m.group(1) if m else None

    def external_ddt_typename(self) -> Optional[str]:
        """Return the type name for an external DDT type, or None."""
        m = _EXTERNAL_TYPE_RE.match(self.type)
        return m.group(2) if m else None


########################################################################

class MetadataSection:
    """One ``[ccpp-arg-table]`` section: name, type, and variables.

    For scheme tables the section name encodes the phase:
    ``<scheme_name>_<phase>`` where *phase* is one of
    ``register``, ``init``, ``timestep_init``, ``run``,
    ``timestep_final``, ``final``.

    For non-scheme tables the section name equals the table name.

    Parameters
    ----------
    section_name : str
        The ``name`` attribute from ``[ccpp-arg-table]``.
    section_type : str
        The ``type`` attribute (must match the enclosing table type).
    table_name : str
        The enclosing table's name (used for validation and error messages).
    context : ParseContext
        Source location.

    Examples
    --------
    >>> from metadata.parse_tools import ParseContext
    >>> ctx = ParseContext(5, 'scheme.meta')
    >>> sec = MetadataSection('my_scheme_run', 'scheme', 'my_scheme', ctx)
    >>> sec.section_name
    'my_scheme_run'
    >>> sec.phase
    'run'
    >>> sec = MetadataSection('host_data', 'host', 'host_data', ctx)
    >>> sec.phase is None
    True
    """

    def __init__(self, section_name: str, section_type: str,
                 table_name: str, context: ParseContext):
        if section_type not in VALID_TABLE_TYPES:
            raise CCPPError(
                "Section type '{}' is not a valid table type; "
                "must be one of {}, at {}".format(
                    section_type, sorted(VALID_TABLE_TYPES), context
                )
            )
        self.section_name: str   = section_name
        self.section_type: str   = section_type
        self.context: ParseContext = context
        self.variables: List[MetaVar] = []
        self._std_name_index: Dict[str, MetaVar] = {}
        # Derive the scheme phase from the section name (scheme only).
        self._phase: Optional[str] = None
        if section_type == SCHEME_TABLE_TYPE:
            self._phase = self._extract_phase(section_name, table_name, context)

    @staticmethod
    def _extract_phase(section_name: str, scheme_name: str,
                       context: ParseContext) -> str:
        """Extract and validate the phase suffix from a scheme section name.

        The section name must have the form ``<scheme_name>_<phase>``.
        If the suffix is ``finalize`` (the old name), a hard error is raised
        directing the user to rename it to ``final``.
        """
        prefix = scheme_name + '_'
        if not section_name.startswith(prefix):
            raise CCPPError(
                "Scheme section name '{}' does not begin with scheme name '{}', "
                "at {}".format(section_name, scheme_name, context)
            )
        phase = section_name[len(prefix):]
        if phase == 'finalize':
            raise CCPPError(
                "Phase 'finalize' has been renamed to 'final'; "
                "rename '{}' to '{}', at {}".format(
                    section_name, scheme_name + '_final', context
                )
            )
        if phase not in VALID_SCHEME_PHASES:
            raise CCPPError(
                "Unknown scheme phase '{}' in section '{}'; "
                "must be one of {}, at {}".format(
                    phase, section_name, sorted(VALID_SCHEME_PHASES), context
                )
            )
        return phase

    @property
    def phase(self) -> Optional[str]:
        """The scheme phase (``'run'``, ``'init'``, etc.) or ``None``."""
        return self._phase

    def add_variable(self, var: MetaVar) -> None:
        """Append *var* to this section, checking for duplicate standard names."""
        if var.standard_name in self._std_name_index:
            existing = self._std_name_index[var.standard_name]
            raise CCPPError(
                "Duplicate standard name '{}' in section '{}': "
                "first at {}, duplicate at {}".format(
                    var.standard_name, self.section_name,
                    existing.context, var.context
                )
            )
        self.variables.append(var)
        self._std_name_index[var.standard_name] = var

    def get_variable(self, standard_name: str) -> Optional[MetaVar]:
        """Return the variable with *standard_name*, or ``None``."""
        return self._std_name_index.get(standard_name)

    def __repr__(self) -> str:
        return "MetadataSection({!r}, nvars={})".format(
            self.section_name, len(self.variables)
        )


########################################################################

class MetadataTable:
    """A complete CCPP metadata table (one ``[ccpp-table-properties]`` block
    and all its ``[ccpp-arg-table]`` sections).

    Parameters
    ----------
    table_name : str
        The ``name`` from ``[ccpp-table-properties]``.
    table_type : str
        One of ``scheme``, ``host``, ``control``, ``suite``, ``ddt``.
    file_path : str
        Source file path (used in error messages and ``USE`` statements).
    context : ParseContext
        The location of the ``[ccpp-table-properties]`` header.

    Examples
    --------
    >>> from metadata.parse_tools import ParseContext
    >>> ctx = ParseContext(0, 'host.meta')
    >>> tbl = MetadataTable('my_module', 'host', 'host.meta', ctx)
    >>> tbl.table_name
    'my_module'
    >>> tbl.table_type
    'host'
    >>> tbl.is_scheme
    False
    """

    def __init__(self, table_name: str, table_type: str,
                 file_path: str, context: ParseContext):
        if table_type not in VALID_TABLE_TYPES:
            raise CCPPError(
                "Table type '{}' is not valid; must be one of {}, at {}".format(
                    table_type, sorted(VALID_TABLE_TYPES), context
                )
            )
        self.table_name: str    = table_name
        self.table_type: str    = table_type
        self.file_path: str     = file_path
        self.context: ParseContext = context
        self._sections: List[MetadataSection] = []
        # Optional table-level properties (set by apply_table_props).
        self.dependencies: List[str] = []
        self.source_path: str = ''
        # Fortran module name that exports this table's Fortran symbols.
        # When absent in metadata, falls back to :attr:`table_name` (the
        # common case: the .meta file shares its base name with the
        # Fortran module).  Applies to any table type whose contents are
        # imported from a Fortran module (``scheme``, ``host``, ``ddt``);
        # the cap generator uses it to emit ``use <module>, only: ...``
        # lines targeting the actual module rather than the table name.
        self.module_name: str = ''
        # Each entry is ``(kind_name, module, spec)``; aggregated by
        # ccpp_capgen into the kind map for ccpp_kinds.F90.
        self.kind_specs: List[Tuple[str, str, str]] = []

    def apply_table_props(self, props: dict) -> None:
        """Apply extra ``[ccpp-table-properties]`` key-value pairs to this table.

        Recognised keys
        ---------------
        ``source_path``
            Relative path from the ``.meta`` file directory to the directory
            containing the corresponding Fortran source (``.F90``).  Resolved
            to an absolute path and stored in :attr:`source_path`.  Defaults
            to the ``.meta`` file's own directory.

        ``dependencies``
            Comma-separated list of dependency file paths (may include
            ``../../``-style relative paths).  Resolved against the ``.meta``
            directory, optionally adjusted by ``dependencies_path``.

        ``dependencies_path``
            Relative path from the ``.meta`` directory used as the base for
            resolving entries in ``dependencies``.

        ``kind_spec``
            Either a single ``<module>:<kind_name>=>spec`` (or shorthand
            ``<module>:<spec>``) string, or a list of such strings when the
            ``kind_spec`` key appears more than once in the table header.
            Each entry is parsed into a ``(kind_name, module, spec)`` triple
            and appended to :attr:`kind_specs`.

        Examples
        --------
        >>> from metadata.parse_tools import ParseContext
        >>> ctx = ParseContext(0, 's.meta')
        >>> t = MetadataTable('s', 'scheme', '/project/src/s.meta', ctx)
        >>> t.apply_table_props({'source_path': 'fortran', 'dependencies': 'util.F90', 'dependencies_path': 'lib'})
        >>> t.source_path == '/project/src/fortran'
        True
        >>> t.dependencies
        ['/project/src/lib/util.F90']
        >>> t = MetadataTable('s', 'scheme', '/p/s.meta', ctx)
        >>> t.apply_table_props({'kind_spec': 'temp_kinds:kind_temp=>temp_r8'})
        >>> t.kind_specs
        [('kind_temp', 'temp_kinds', 'temp_r8')]
        >>> t = MetadataTable('s', 'scheme', '/p/s.meta', ctx)
        >>> t.apply_table_props({'kind_spec': [
        ...     'temp_kinds:kind_temp=>temp_r8',
        ...     'host_kinds:kind_r4',
        ... ]})
        >>> t.kind_specs
        [('kind_temp', 'temp_kinds', 'temp_r8'), ('kind_r4', 'host_kinds', 'kind_r4')]
        """
        meta_dir = os.path.dirname(os.path.abspath(self.file_path))

        if 'source_path' in props:
            self.source_path = os.path.normpath(
                os.path.join(meta_dir, props['source_path'])
            )
        else:
            self.source_path = meta_dir

        dep_base = meta_dir
        if 'dependencies_path' in props:
            dep_base = os.path.normpath(
                os.path.join(meta_dir, props['dependencies_path'])
            )

        if 'dependencies' in props:
            # ``dependencies`` may legitimately appear more than once in a
            # single ``[ccpp-table-properties]`` block; the parser
            # collects repeats into a list (analogous to ``kind_spec``).
            # A single occurrence still arrives as a string.  Each entry
            # is a comma-separated list of file paths (or "none" to
            # signal an empty dependency set).
            raw_entries = props['dependencies']
            if isinstance(raw_entries, str):
                raw_entries = [raw_entries]
            for raw in raw_entries:
                raw = raw.strip()
                if raw.lower() == 'none':
                    continue
                for entry in raw.split(','):
                    entry = entry.strip()
                    if entry:
                        self.dependencies.append(
                            os.path.normpath(os.path.join(dep_base, entry))
                        )

        if 'kind_spec' in props:
            raw_specs = props['kind_spec']
            if isinstance(raw_specs, str):
                raw_specs = [raw_specs]
            for entry in raw_specs:
                self.kind_specs.append(
                    _parse_kind_spec_value(entry, self.context)
                )

        if 'module_name' in props:
            raw = props['module_name'].strip()
            if raw:
                self.module_name = raw

    @property
    def is_scheme(self) -> bool:
        """True if this is a ``scheme`` table."""
        return self.table_type == SCHEME_TABLE_TYPE

    def sections(self) -> List[MetadataSection]:
        """Return all sections (``[ccpp-arg-table]`` blocks) in this table."""
        return list(self._sections)

    def variables(self) -> List[MetaVar]:
        """Return all variables across all sections (de-duplicated by standard name).

        For a singleton table (``host``, ``control``, ``ddt``) there is only
        one section, so this simply returns that section's variables.  For
        scheme tables all variables from all phases are returned; variables
        that appear in multiple phases are included only once (first occurrence
        wins).
        """
        seen: Dict[str, MetaVar] = {}
        for sec in self._sections:
            for var in sec.variables:
                if var.standard_name not in seen:
                    seen[var.standard_name] = var
        return list(seen.values())

    def add_section(self, section: MetadataSection) -> None:
        """Append *section* to this table.

        Enforces that singleton table types (``host``, ``control``, ``suite``,
        ``ddt``) have at most one section.  Also verifies that the section's
        type matches the table type.
        """
        if section.section_type != self.table_type:
            raise CCPPError(
                "Section type '{}' does not match table type '{}' "
                "in table '{}', at {}".format(
                    section.section_type, self.table_type,
                    self.table_name, section.context
                )
            )
        if self.table_type in SINGLETON_TABLE_TYPES and self._sections:
            raise CCPPError(
                "Table type '{}' allows only one section per table; "
                "found a second section '{}' in table '{}', at {}".format(
                    self.table_type, section.section_name,
                    self.table_name, section.context
                )
            )
        self._sections.append(section)

    def section_for_phase(self, phase: str) -> Optional[MetadataSection]:
        """Return the section for the given scheme *phase*, or ``None``.

        Only meaningful for scheme tables; always returns ``None`` for
        other table types.
        """
        for sec in self._sections:
            if sec.phase == phase:
                return sec
        return None

    def __repr__(self) -> str:
        return "MetadataTable({!r}, type={!r}, nsections={})".format(
            self.table_name, self.table_type, len(self._sections)
        )


########################################################################
# File parser
########################################################################

def parse_metadata_file(file_path: str) -> List[MetadataTable]:
    """Parse a ``.meta`` file and return all :class:`MetadataTable` objects.

    Parameters
    ----------
    file_path : str
        Absolute or relative path to the ``.meta`` file.

    Returns
    -------
    list of MetadataTable
        One entry per ``[ccpp-table-properties]`` block found in the file.

    Raises
    ------
    CCPPError
        On any structural or content error.  The error message includes the
        file path and line number.

    Notes
    -----
    ``type = module`` is rejected with a descriptive error directing the user
    to use ``type = host`` instead (breaking rename from the old generator).

    All blank lines and lines starting with ``#`` or ``;`` are ignored.

    Examples
    --------
    >>> import tempfile, os
    >>> content = '''
    ... [ccpp-table-properties]
    ...   name = test_host
    ...   type = host
    ...
    ... [ccpp-arg-table]
    ...   name = test_host
    ...   type = host
    ... [ im ]
    ...   standard_name = horizontal_dimension
    ...   units = count
    ...   dimensions = ()
    ...   type = integer
    ... '''
    >>> with tempfile.NamedTemporaryFile(mode='w', suffix='.meta',
    ...                                  delete=False) as f:
    ...     _ = f.write(content)
    ...     fname = f.name
    >>> tables = parse_metadata_file(fname)
    >>> os.unlink(fname)
    >>> len(tables)
    1
    >>> tables[0].table_name
    'test_host'
    >>> tables[0].table_type
    'host'
    >>> tables[0].sections()[0].variables[0].standard_name
    'horizontal_dimension'
    """
    if not os.path.isfile(file_path):
        raise CCPPError("Metadata file '{}' does not exist".format(file_path))

    with open(file_path, 'r', encoding='utf-8') as fh:
        lines = fh.readlines()

    return _parse_lines(lines, file_path)


def _parse_lines(lines: List[str], file_path: str) -> List[MetadataTable]:
    """Internal line-by-line parser.  Exposed for unit-testing without
    needing real files.

    Parameters
    ----------
    lines : list of str
        Source lines (with or without trailing newlines).
    file_path : str
        Used only for :class:`ParseContext` error messages.

    Returns
    -------
    list of MetadataTable
    """
    tables: List[MetadataTable] = []
    current_table: Optional[MetadataTable]   = None
    current_section: Optional[MetadataSection] = None
    current_var: Optional[MetaVar]           = None
    collecting_table_props = False
    collecting_section_props = False

    # Accumulate key=value pairs for the current table/section/variable header.
    pending_props: Dict[str, str] = {}
    pending_start: int = 0

    def ctx(lineno: int) -> ParseContext:
        return ParseContext(linenum=lineno, filename=file_path)

    def flush_var(lineno: int) -> None:
        """Validate and attach the buffered variable to the current section."""
        nonlocal current_var
        if current_var is None:
            return
        require_intent = (current_section is not None and
                          current_section.section_type == SCHEME_TABLE_TYPE)
        current_var.validate(require_intent=require_intent, context=ctx(lineno))
        if current_section is None:
            raise CCPPError(
                "Variable '{}' found outside any section, at {}".format(
                    current_var.local_name, ctx(lineno)
                )
            )
        current_section.add_variable(current_var)
        current_var = None

    def flush_section(lineno: int) -> None:
        """Attach the current section to the current table."""
        nonlocal current_section
        if current_section is None:
            return
        flush_var(lineno)
        if current_table is None:
            raise CCPPError(
                "Section found outside any table, at {}".format(ctx(lineno))
            )
        current_table.add_section(current_section)
        current_section = None

    def flush_table_props() -> None:
        """Apply any extra table-property keys to the current table."""
        if current_table is not None and collecting_table_props:
            current_table.apply_table_props(pending_props)

    for lineno, raw_line in enumerate(lines):
        line = raw_line.rstrip('\n').rstrip('\r')
        # Discard any inline ``# ...`` comment so headers, key=value lines,
        # and the blank-line check all see the same content the user
        # intended as data.
        line = _strip_inline_comment(line)

        # ---- [ccpp-table-properties] ----------------------------------------
        if line.strip().lower() == _TABLE_PROPS_HDR:
            # Finish whatever we were doing.
            flush_table_props()
            flush_section(lineno)
            if current_table is not None:
                tables.append(current_table)
            current_table = None
            pending_props = {}
            pending_start = lineno
            collecting_table_props = True
            collecting_section_props = False
            continue

        # ---- [ccpp-arg-table] -----------------------------------------------
        if line.strip().lower() == _ARG_TABLE_HDR:
            flush_table_props()
            flush_section(lineno)
            collecting_section_props = True
            collecting_table_props = False
            pending_props = {}
            pending_start = lineno
            continue

        # ---- Blank / comment lines ------------------------------------------
        if _is_blank(line):
            continue

        # ---- Variable header  [ name ] ----------------------------------------
        var_match = _VAR_HEADER_RE.match(line)
        if var_match:
            collecting_table_props = False
            collecting_section_props = False
            # Flush the previous variable.
            flush_var(lineno)
            # Start a new variable.
            local_name = var_match.group(1)
            current_var = MetaVar(local_name, ctx(lineno))
            continue

        # ---- Key = value line -----------------------------------------------
        if collecting_table_props:
            pairs = _parse_config_line(line, ctx(lineno))
            for key, val in pairs:
                if key in ('kind_spec', 'dependencies'):
                    # These keys may legitimately appear more than once
                    # in a single table header; accumulate occurrences
                    # into a list.  ``apply_table_props`` accepts either
                    # form (string when seen once, list when repeated).
                    pending_props.setdefault(key, []).append(val)
                    continue
                if key in pending_props:
                    raise CCPPError(
                        "Duplicate table property '{}', at {}".format(
                            key, ctx(lineno)
                        )
                    )
                pending_props[key] = val
            # Try to build the MetadataTable as soon as we have name + type.
            if 'name' in pending_props and 'type' in pending_props and current_table is None:
                current_table = _make_table(
                    pending_props['name'], pending_props['type'],
                    file_path, ctx(pending_start)
                )
            continue

        if collecting_section_props:
            pairs = _parse_config_line(line, ctx(lineno))
            for key, val in pairs:
                if key in pending_props:
                    raise CCPPError(
                        "Duplicate section property '{}', at {}".format(
                            key, ctx(lineno)
                        )
                    )
                pending_props[key] = val
            # Try to build the MetadataSection as soon as we have name + type.
            if ('name' in pending_props and 'type' in pending_props
                    and current_section is None and current_table is not None):
                current_section = _make_section(
                    pending_props['name'], pending_props['type'],
                    current_table.table_name, ctx(pending_start)
                )
            continue

        if current_var is not None:
            pairs = _parse_config_line(line, ctx(lineno))
            for key, val in pairs:
                if current_section is not None:
                    sec_type = current_section.section_type
                    # ``active`` is a host-model attribute: it expresses a
                    # condition (referencing host standard names) under
                    # which a host-owned variable is valid storage.  It
                    # belongs only on host and ddt tables.  control vars
                    # are unconditionally framework-injected, suite tables
                    # are generated wholesale, and scheme args are never
                    # the originating storage — so all three reject it.
                    if (sec_type not in ('host', 'ddt')
                            and key == 'active'):
                        raise ParseSyntaxError(
                            "'active' is a host-model attribute and may "
                            "only appear in host or ddt metadata; not "
                            "valid for {} tables".format(sec_type),
                            token=key, context=ctx(lineno)
                        )
                    if (sec_type != SCHEME_TABLE_TYPE
                            and key in ('intent', 'optional')):
                        raise ParseSyntaxError(
                            "'{}' is a scheme-only attribute and cannot "
                            "appear in host, control, ddt, or suite "
                            "metadata".format(key),
                            token=key, context=ctx(lineno)
                        )
                    if (sec_type != SCHEME_TABLE_TYPE and
                            key in ('constituent', 'advected', 'molar_mass')):
                        raise ParseSyntaxError(
                            "'{}' is a scheme-only constituent hint and cannot "
                            "appear in host, control, ddt, or suite metadata".format(key),
                            token=key, context=ctx(lineno)
                        )
                    # auto-clone-constituents: the four legacy
                    # instantiate-kwarg attrs are scheme-only when the
                    # shim is enabled.  When the shim is off they get
                    # rejected one layer down (unknown attribute), so
                    # the guard only matters in shim-on builds.
                    if (sec_type != SCHEME_TABLE_TYPE
                            and auto_clone_constituents.is_enabled()
                            and key in
                            auto_clone_constituents.extra_known_attrs()):
                        raise ParseSyntaxError(
                            "'{}' is a scheme-only constituent property "
                            "and cannot appear in host, control, ddt, or "
                            "suite metadata".format(key),
                            token=key, context=ctx(lineno)
                        )
                current_var.set_attr(key, val, ctx(lineno))
            continue

        # Line is not blank and not handled — syntax error.
        raise ParseSyntaxError("unexpected line", token=line.strip(),
                               context=ctx(lineno))

    # ---- End of file: flush any in-progress objects -------------------------
    flush_table_props()
    flush_section(len(lines))
    if current_table is not None:
        tables.append(current_table)

    return tables


def _make_table(name: str, type_str: str, file_path: str,
                context: ParseContext) -> MetadataTable:
    """Construct a :class:`MetadataTable`, with helpful error for ``type=module``."""
    ttype = type_str.strip().lower()
    if ttype == 'module':
        raise CCPPError(
            "Table type 'module' is not supported; use 'type = host' instead, "
            "at {}".format(context)
        )
    return MetadataTable(name.strip(), ttype, file_path, context)


def _make_section(name: str, type_str: str, table_name: str,
                  context: ParseContext) -> MetadataSection:
    """Construct a :class:`MetadataSection`."""
    return MetadataSection(
        name.strip(), type_str.strip().lower(), table_name, context
    )
