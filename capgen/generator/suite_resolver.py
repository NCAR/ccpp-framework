#!/usr/bin/env python3

"""Variable matching and call-site resolution for the cap code generator.

Resolves every scheme argument against the flat host/control dictionary built by
:func:`metadata.variable_resolver.build_flat_host_dict`, discovers suite-owned
(interstitial) variables, detects unit/kind transformations, and builds the
complete call-site information needed by :mod:`generator.group_cap`.

Variable matching rules (Section 8.4 of the redesign spec)
-----------------------------------------------------------
For each standard name requested by a scheme argument:

1. **Found in host/control dict** → direct reference; check units/kind for
   transformation.
2. **Not found, first use is ``intent(out)``** → suite-owned variable; add to
   suite data, generate declaration in ``ccpp_<suite>_data.F90``.
3. **Not found, first use is ``intent(in)`` or ``intent(inout)``** → code
   generation error: variable used before it is provided.
4. **Already in suite data (from a prior scheme)** → reference suite data path;
   check units/kind for transformation.

Dimension indexing rules (Section 9.2)
---------------------------------------
Each entry in the ``dimensions`` list of a host/suite variable is either a
bare standard name (``'vertical_layer_dimension'``) or an explicit
``lower:upper`` range (``'ccpp_constant_one:horizontal_dimension'``,
``'bot_idx:vertical_interface_dimension'``).  Bare names are normalised to
``ccpp_constant_one:<name>`` before processing.

After normalisation the upper-bound standard name drives dispatch:

- Registered scalar-index dim (see
  ``metadata.registered_dimensions.SCALAR_INDEX_DIMS``; currently
  ``number_of_instances`` → ``instance_number``,
  ``number_of_threads`` → ``thread_number``) → scalar extraction using
  the paired index variable's local name.  The scalar subscript is
  already in the access path for DDT-component fields, but needed here
  for a DDT instance variable itself when passed directly, and for any
  flat-array dim that hits the same registered name.
- ``horizontal_dimension`` →
  ``<lb_local>:<ub_local>`` (all phases).  The lower bound must resolve to
  ``1`` (i.e. be ``ccpp_constant_one`` or the integer literal ``1``).
- Everything else → ``<lower_expr>:<upper_expr>`` where both bounds are
  resolved from *host_dict* or as integer literals.

Transform cases (Section 10.3)
-------------------------------
Four cases, determined by ``optional`` and whether units/kind differ:

======  ================  ============
Case    optional?         transform?
======  ================  ============
1       no                no
2       yes               no
3       no                yes
4       yes               yes
======  ================  ============
"""

import hashlib
import re
from dataclasses import dataclass, field
from typing import Dict, List, Optional, Set, Tuple, Union

from metadata.parse_tools import CCPPError, FORTRAN_CONDITIONAL_REGEX
from metadata.registered_dimensions import (
    SCALAR_INDEX_DIMS,
    scalar_index_for,
    is_scalar_index_dim,
)
from metadata.variable_resolver import HostVarEntry, _resolve_subscript
# dim-aliases: transient GFS-physics shim (delete this import and the
# canonical() call in _canonical_dim when the shim is removed).
from metadata import dim_aliases
# auto-clone-constituents: transient legacy shim (delete this import
# and the AutoCloneEntry / _collect_auto_clone_entries touchpoints
# when the shim is removed).
from metadata import auto_clone_constituents

# Dimension standard names that map to horizontal loop bounds.  The
# legacy spelling ``horizontal_loop_extent`` is rejected at parse time
# (see ``_FORBIDDEN_DIMENSION_NAMES`` in ccpp_capgen.py) or rewritten
# by the ``--legacy-mode`` shim, so it can never appear here.
_HORIZ_LOOP_DIMS: frozenset = frozenset({
    'horizontal_dimension',
})

# Standard names for horizontal loop bounds and full horizontal dimension.
_HORIZ_BEGIN_STD  = 'horizontal_loop_begin'
_HORIZ_END_STD    = 'horizontal_loop_end'
_HORIZ_DIM_STD    = 'horizontal_dimension'
_INSTANCE_NUM_STD = 'instance_number'

# Vertical-dimension standard names (used by the vertical-flip transform
# when host and scheme metadata disagree on the ``top_at_one`` attribute).
_VDIM_STDS: frozenset = frozenset({
    'vertical_layer_dimension',
    'vertical_interface_dimension',
})

# Physics scheme phases that operate on the per-call horizontal slice and
# therefore receive (ub - lb + 1) when a scheme asks for a scalar
# horizontal_dimension.  Register is excluded: it runs at suite-cap level
# with the minimal framework signature (no loop bounds available).
_PHYSICS_PHASES: frozenset = frozenset({
    'init', 'timestep_init', 'run', 'timestep_final', 'final',
})

# Framework constant whose Fortran value is always 1.  Used as the implicit
# lower bound when a dimension string carries no explicit range, and is the
# only non-integer lower bound accepted for horizontal dimensions.
_CCPP_CONSTANT_ONE = 'ccpp_constant_one'

# Type marker for register-phase constituent registration args.  A scheme
# arg with this type, ``intent=out``, in the ``register`` phase is recognised
# as the per-scheme constituent array for the two-pass merge into the host's
# ``ccpp_model_constituents_object``.
_CONST_PROP_TYPE = 'ccpp_constituent_properties_t'

# Standard name the host model uses to expose its ``ccpp_model_constituents_t``
# object via the ``type=host`` table (opt-in, only required when at least
# one register-phase scheme produces constituents).
_CONST_OBJ_STDNAME = 'ccpp_model_constituents_object'

# Framework-provided constituent standard names.  The suite cap owns
# these symbols (allocates/binds them at init time); schemes reference
# them like any other variable and the resolver routes them to a
# synthetic source category ``'constituent'``.
_CONST_BASE_ARRAY_STD = 'ccpp_constituents'
_CONST_TEND_ARRAY_STD = 'ccpp_constituent_tendencies'
_CONST_PROPS_ARRAY_STD = 'ccpp_constituent_properties'
_CONST_NUM_STD         = 'number_of_ccpp_constituents'
_CONST_MINVAL_STD      = 'ccpp_constituent_minimum_values'
_TEND_PREFIX           = 'tendency_of_'
_INDEX_PREFIX          = 'index_of_'

# Fortran 2008 caps user-defined identifiers at 63 characters.  Several
# CCPP standard-name conventions (notably CAM-SIMA's
# ``_wrt_moist_air_and_condensed_water`` constituent suffix) produce
# base names ≥55 chars, which blow the limit once the ``index_of_``
# prefix is prepended.  The helper below mangles overlong names to a
# deterministic 63-char form so every emitter and resolver call site
# sees the same symbol; short names are returned unchanged.
_FORTRAN_ID_LIMIT = 63


def _index_symbol_name(base_std_name: str) -> str:
    """Return the Fortran local name for ``index_of_<base_std_name>``.

    Identity for inputs whose ``index_of_`` form fits Fortran's 63-char
    identifier limit; otherwise truncates the base and appends a short
    SHA-1 hash so distinct std-names map to distinct symbols.  The
    chosen layout is::

        index_of_<base[:max_base_len]>_<8-hex-sha1>

    where ``max_base_len = 63 - len('index_of_') - 1 - 8 = 45``.  All
    emit/reference sites (host_constituents.py public/declaration/
    reset/const_index/init-guard, suite_resolver.py auto-provisioned
    subscript, Path 1a call_expr) MUST route through this helper to
    keep the symbol consistent within a single capgen run.  The
    underlying std_name is still passed to ``const_index`` as a
    string literal, so the framework lookup keys remain unchanged --
    only the Fortran-side mapping symbol is mangled.

    Examples
    --------
    >>> _index_symbol_name('water_vapor')
    'index_of_water_vapor'
    >>> name = _index_symbol_name(
    ...     'cloud_liquid_water_mixing_ratio_wrt_moist_air_and_condensed_water')
    >>> len(name) <= 63
    True
    >>> name.startswith('index_of_')
    True
    >>> name == _index_symbol_name(
    ...     'cloud_liquid_water_mixing_ratio_wrt_moist_air_and_condensed_water')
    True
    """
    full = _INDEX_PREFIX + base_std_name
    if len(full) <= _FORTRAN_ID_LIMIT:
        return full
    import hashlib
    sha8 = hashlib.sha1(base_std_name.encode('utf-8')).hexdigest()[:8]
    # 63 - len('index_of_') - 1 (sep) - 8 (sha) = 45
    max_base_len = _FORTRAN_ID_LIMIT - len(_INDEX_PREFIX) - 1 - 8
    return '{}{}_{}'.format(
        _INDEX_PREFIX, base_std_name[:max_base_len], sha8,
    )

# Std names directly satisfied by host-constituents-module-owned symbols.
_FRAMEWORK_CONST_STDS = frozenset({
    _CONST_BASE_ARRAY_STD,
    _CONST_TEND_ARRAY_STD,
    _CONST_PROPS_ARRAY_STD,
    _CONST_NUM_STD,
    _CONST_MINVAL_STD,
})

# Per-instance constituent object name in ccpp_host_constituents.  Schemes
# access constituent state through ``<obj>(inst_num)%<member>``.
_CONST_OBJ_VAR = 'ccpp_model_constituents_obj'

# Mapping from framework-named std_name → DDT member.  Used to translate
# scheme args declaring one of these framework names into the matching
# per-instance access expression.
_FRAMEWORK_NAME_TO_MEMBER = {
    _CONST_BASE_ARRAY_STD:  'vars_layer',
    _CONST_TEND_ARRAY_STD:  'vars_layer_tend',
    _CONST_PROPS_ARRAY_STD: 'const_metadata',
    _CONST_NUM_STD:         'num_layer_vars',
    _CONST_MINVAL_STD:      'vars_minvalue',
}


# Single host-wide module that owns the constituent object, the
# framework-shared pointers, the per-suite dynamic-constituent buffers,
# and the host-facing constituent API.  All suite caps USE this module
# for their constituent symbol references.
_HOST_CONST_MOD = 'ccpp_host_constituents'


def _constituent_module_name(suite_name: str) -> str:
    """Return the module name that owns the host-wide constituent state.

    Constant across suites: in capgen (option A, matching original
    capgen) the constituent object is host-wide, not suite-local.
    """
    return _HOST_CONST_MOD


########################################################################
# Unit conversion look-up
########################################################################

def _normalize_unit_string(unit: str) -> str:
    """Canonicalise a unit string so that bare positive exponents carry an
    explicit ``+`` sign.

    The CF / udunits conventions allow either ``m2`` or ``m+2`` to denote
    "metres squared".  The two forms are equivalent, but downstream code
    treats unit strings as opaque tokens and compares them with ``==``.
    Without normalisation, a host declaring ``m2 s-2`` and a scheme
    declaring ``m+2 s-2`` would be flagged as a unit mismatch.

    Normalisation rule: a letter immediately followed by an unsigned
    positive integer is rewritten as ``letter+integer``.  Existing
    ``letter+N`` and ``letter-N`` forms are left unchanged.
    """
    return re.sub(r'([A-Za-z])(\d+)', r'\1+\2', unit)


def _unit_to_id(unit: str) -> str:
    """Convert a unit string to the Python identifier fragment used in
    :mod:`metadata.unit_conversion`.

    The input is first normalised by :func:`_normalize_unit_string` so
    that bare and explicit positive exponents collapse to the same form.

    Rules (after normalisation):

    * Spaces → ``_``
    * ``letter-N`` → ``letter_minus_N``
    * ``letter+N`` → ``letter_plus_N``
    """
    result = _normalize_unit_string(unit).replace(' ', '_')
    result = re.sub(r'([A-Za-z])([+])(\d+)', r'\1_plus_\3', result)
    result = re.sub(r'([A-Za-z])(-)(\d+)',   r'\1_minus_\3', result)
    return result


def find_unit_conversion(from_unit: str, to_unit: str):
    """Return the conversion formula callable, or ``None`` if unavailable.

    The formula callable takes no arguments and returns a format string
    where ``{var}`` is the Fortran expression to convert and ``{kind}``
    is the kind suffix (``_kind_phys`` or ``''``).

    Input unit strings are normalised by :func:`_normalize_unit_string`
    before the equality check and the function-name lookup, so equivalent
    forms (``m2`` and ``m+2``) compare equal and resolve to the same
    conversion entry.
    """
    from_norm = _normalize_unit_string(from_unit)
    to_norm   = _normalize_unit_string(to_unit)
    if from_norm == to_norm:
        return None
    from metadata import unit_conversion as _uc
    fn_name = '{}__to__{}'.format(_unit_to_id(from_norm), _unit_to_id(to_norm))
    return getattr(_uc, fn_name, None)


def _apply_transform_formula(formula_fn, var_expr: str, kind: str) -> str:
    """Apply a unit-conversion formula callable.

    Parameters
    ----------
    formula_fn : callable
        Returned by :func:`find_unit_conversion`.
    var_expr : str
        Fortran expression for the source variable.
    kind : str
        Kind parameter name (e.g. ``'kind_phys'``), or ``''``.

    Returns
    -------
    str
        Fortran expression for the converted value.
    """
    kind_suffix = '_{}'.format(kind) if kind else ''
    return formula_fn().format(kind=kind_suffix, var=var_expr)


#: Map from CCPP metadata ``type =`` to the Fortran kind-cast intrinsic.
#: Only the numeric types that legitimately carry distinct kinds in
#: CCPP physics are mapped; everything else (logical, character, DDT)
#: is rejected by :func:`_kind_cast_expr` because either the kind itself
#: is dimensioned differently (character ``len=``) or kinds don't apply.
_KIND_CAST_INTRINSIC = {
    'real':    'real',
    'integer': 'int',
    'complex': 'cmplx',
}


def _kind_cast_expr(
    var_type: str,
    var_expr: str,
    target_kind: str,
    local: str,
    std_name: str,
    scheme_name: str,
) -> str:
    """Build a Fortran expression that casts *var_expr* to *target_kind*.

    Used when host and scheme metadata differ in ``kind`` only -- no unit
    conversion, no vertical flip -- so the transformation temporary
    needs an explicit precision cast.  Without this the temp would be
    declared but never assigned (see :func:`_resolve_one_arg`).

    Returns a Fortran expression like ``real(con_pi, kind=kind_phys)``.

    Raises
    ------
    CCPPError
        When *var_type* is not one of the numeric types listed in
        :data:`_KIND_CAST_INTRINSIC` (kinds on logical / character are
        handled via separate metadata pathways, and DDT kinds don't
        apply -- a kind mismatch on those types indicates the metadata
        is wrong rather than something the cap should bridge).
    """
    intrinsic = _KIND_CAST_INTRINSIC.get(var_type.strip().lower())
    if intrinsic is None:
        raise CCPPError(
            "Variable '{}' (standard_name='{}', scheme '{}'): host and "
            "scheme metadata differ in 'kind' but the variable type "
            "'{}' has no defined kind-cast intrinsic.  Kind differences "
            "are supported for real / integer / complex only; fix the "
            "metadata so the kinds match.".format(
                local, std_name, scheme_name, var_type,
            )
        )
    return '{}({}, kind={})'.format(intrinsic, var_expr, target_kind)


########################################################################
# Dimension subscript helpers
########################################################################

def _format_available_std_names(
    host_dict: Dict[str, HostVarEntry],
    suite_vars: Optional[Dict[str, 'SuiteVar']] = None,
    near: Optional[str] = None,
) -> str:
    """Build a sorted listing of every standard name the resolver can see.

    Used in error messages when a lookup fails.  Each line is
    ``  <std_name>  [<source>]`` where source is ``control``,
    ``host: <module>``, or ``suite: <suite_module>``.  The final list
    is sorted alphabetically (case-insensitive); when *near* is a
    misspelled or mis-cased candidate, close matches surface first
    under a separate "did you mean" header so the user spots the
    typo quickly.
    """
    rows: List[Tuple[str, str]] = []
    for std, entry in host_dict.items():
        if entry.is_control:
            rows.append((std, 'control'))
        elif entry.module_name:
            rows.append((std, 'host: {}'.format(entry.module_name)))
        else:
            rows.append((std, 'host'))
    if suite_vars:
        for std, suite_var in suite_vars.items():
            rows.append((std, 'suite: {}'.format(suite_var.suite_module_name)))
    rows.sort(key=lambda t: t[0])

    if not rows:
        return '\n  (host_dict and suite_vars are both empty)'

    width = max(len(s) for s, _ in rows)
    fmt = '  {{:<{}}}  [{{}}]'.format(width)

    sections: List[str] = []
    if near:
        import difflib
        candidates = difflib.get_close_matches(
            near, [s for s, _ in rows], n=5, cutoff=0.6,
        )
        if not candidates:
            # Try a case-insensitive direct hit (the most common cause:
            # mixed-case in metadata vs lower-cased standard name).
            low = near.lower()
            candidates = [s for s, _ in rows if s == low]
        if candidates:
            sections.append('Did you mean (close matches to {!r}):'.format(near))
            sections.extend(
                fmt.format(s, src)
                for s, src in rows if s in candidates
            )
            sections.append('')

    sections.append('Available standard names ({} entries):'.format(len(rows)))
    sections.extend(fmt.format(s, src) for s, src in rows)
    return '\n' + '\n'.join(sections)


def _resolve_single_bound(
    bound: str,
    host_dict: Dict[str, HostVarEntry],
    used: Set[str],
    suite_vars: Optional[Dict[str, 'SuiteVar']] = None,
) -> Optional[str]:
    """Resolve one dimension bound token to a Fortran expression.

    Recognises, in order:

    1. ``ccpp_constant_one`` — the framework constant equal to ``1``.
    2. Any integer literal — returned as a string unchanged.
    3. A standard name present in *host_dict* — returns the local Fortran name
       and records the standard name in *used*.
    4. A standard name present in *suite_vars* — returns the suite data access
       path (e.g. ``ccpp_suite_data(inst)%dim_inter``) and records the standard
       name in *used*.  Suite-owned scalars set during ``_register`` are read
       here as dimension bounds in later phases.

    Returns ``None`` when the bound cannot be resolved.
    """
    if bound == _CCPP_CONSTANT_ONE:
        return '1'
    try:
        return str(int(bound))
    except ValueError:
        pass
    entry = host_dict.get(bound)
    if entry is not None:
        used.add(bound)
        # Use ``access_path``, not ``local_name``: for plain module-
        # level host vars these are identical, but for DDT-component
        # vars (e.g. ``physics%Model%levs`` with std_name
        # ``vertical_layer_dimension``) the full DDT walk is required
        # so the emitted subscript references the actual storage and
        # the USE statement (which walks back to the root via
        # ``_root_symbol``) imports the right top-level symbol.
        # ``_render_value_expr`` (a) resolves baked
        # ``(instance_number)`` / ``(thread_number)`` placeholders and
        # (b) re-attaches any literal subscript stripped from a
        # ``local_name = foo(1)``-style declaration.
        return _render_value_expr(entry, host_dict)
    if suite_vars:
        suite_var = suite_vars.get(bound)
        if suite_var is not None:
            used.add(bound)
            return suite_var.access_path
    return None


def _build_call_subscript(
    dimensions: List[str],
    phase: str,
    host_dict: Dict[str, HostVarEntry],
    suite_vars: Optional[Dict[str, 'SuiteVar']] = None,
    flip_vertical: bool = False,
) -> Tuple[str, Set[str]]:
    """Build the Fortran subscript string for a variable's dimension list.

    Returns the subscript string (empty for scalars or ``'(s1, s2, ...)'`` for
    arrays) and the set of dimension standard names that were resolved via
    *host_dict* (needed for USE-statement generation).

    Parameters
    ----------
    dimensions : list of str
        Ordered dimension standard names from the host/suite variable entry.
    phase : str
        Current scheme phase (affects horizontal subscripting).
    host_dict : dict
        Flat host+control variable dictionary.
    flip_vertical : bool
        When ``True``, every vertical-dimension entry is emitted with
        reverse stride (``<upper>:<lower>:-1`` instead of ``<lower>:<upper>``).
        Used by the vertical-flip transform when host and scheme disagree
        on ``top_at_one``.

    Returns
    -------
    tuple (subscript_str, used_std_names)

    Raises
    ------
    CCPPError
        If a dimension standard name cannot be resolved.
    """
    if not dimensions:
        return '', set()

    parts: List[str] = []
    used: Set[str] = set()
    for dim in dimensions:
        part, u = _one_dim_part(dim, phase, host_dict, suite_vars=suite_vars,
                                flip_vertical=flip_vertical)
        parts.append(part)
        used.update(u)
    return '({})'.format(', '.join(parts)), used


def _one_dim_part(
    dim: str,
    phase: str,
    host_dict: Dict[str, HostVarEntry],
    suite_vars: Optional[Dict[str, 'SuiteVar']] = None,
    flip_vertical: bool = False,
) -> Tuple[str, Set[str]]:
    """Return the Fortran subscript expression for one dimension entry.

    A dimension entry is either a bare standard name (``'vertical_layer_dimension'``)
    or an explicit lower:upper range (``'ccpp_constant_one:horizontal_dimension'``,
    ``'bot_idx:vertical_interface_dimension'``).  Bare names are normalised
    internally to ``ccpp_constant_one:<name>`` before processing.

    Rules applied after normalisation:

    * Upper bound is a registered scalar-index dim (see
      :data:`metadata.registered_dimensions.SCALAR_INDEX_DIMS`) → scalar
      subscript using the paired index variable's local name (e.g.
      ``instance_number``, ``thread_number``).
    * Upper bound in :data:`_HORIZ_LOOP_DIMS` → ``lb:ub`` (loop bounds).
      Lower bound **must** resolve to ``'1'`` (i.e. be ``ccpp_constant_one``
      or the integer literal ``1``); any other value is an error.
    * Everything else → resolve both bounds from *host_dict* or as integer
      literals and return ``lower_expr:upper_expr``.

    When *flip_vertical* is True and the dimension is a vertical-axis
    dimension (its upper bound is in :data:`_VDIM_STDS`), the bounds are
    emitted in reverse-stride form ``<upper_expr>:<lower_expr>:-1`` so the
    array section reads (and writes) the vertical axis bottom-to-top
    instead of top-to-bottom.  This is how the host-side access expression
    is rendered when host metadata declares ``top_at_one = .true.`` but
    the scheme expects bottom-at-one (or vice versa).

    Returns ``(expr, used_std_names)`` where *expr* is the subscript token
    and *used_std_names* is the set of standard names consumed from *host_dict*.
    """
    used: Set[str] = set()

    # Normalise to range: bare name → ccpp_constant_one:name
    if ':' not in dim:
        lower_str = _CCPP_CONSTANT_ONE
        upper_str = dim
    else:
        lower_str, upper_str = dim.split(':', 1)
        lower_str = lower_str.strip()
        upper_str = upper_str.strip()

    # Framework-provided constituent count dimension.  Any variable -- host,
    # suite-owned, or scheme -- may be dimensioned by
    # ``number_of_ccpp_constituents``.  The framework owns the extent via the
    # per-instance constituent object, so a *call subscript* passes the whole
    # constituent axis.  This mirrors :func:`_const_dim_part` (which only fires
    # for framework-constituent args) and generalises the recognition to every
    # other variable dimensioned by the count.  There is no host scalar to USE,
    # so ``used`` is left untouched.  (Allocating a suite-owned var sized by
    # this dim is handled separately in generator.suite_data, which resolves the
    # extent to ``ccpp_model_constituents_obj(i)%num_layer_vars``.)
    if upper_str == _CONST_NUM_STD:
        return ':', used

    # Registered scalar-index dimension: collapse to the paired index
    # variable's local Fortran name regardless of lower bound.  See
    # capgen/metadata/registered_dimensions.py for the contract.
    idx_std = scalar_index_for(upper_str)
    if idx_std is not None:
        idx_entry = host_dict.get(idx_std)
        if idx_entry is None:
            raise CCPPError(
                "Metadata references registered scalar-index dimension "
                "'{dim}', which is paired with index variable '{idx}', "
                "but the host has not declared '{idx}' in any type=control "
                "or type=host table.  Either declare '{idx}' as a scalar "
                "integer in the host control/host metadata, or remove "
                "the '{dim}' dimension from the affected metadata.  See "
                "capgen/metadata/registered_dimensions.py for the full "
                "table of registered scalar-index pairings.".format(
                    dim=upper_str, idx=idx_std,
                )
            )
        used.add(idx_std)
        return idx_entry.local_name, used

    # Horizontal dimension: validate lower, return loop bounds
    if upper_str in _HORIZ_LOOP_DIMS:
        lower_expr = _resolve_single_bound(lower_str, host_dict, set())
        if lower_expr != '1':
            raise CCPPError(
                "Lower bound '{}' for horizontal dimension '{}' must be "
                "1 or ccpp_constant_one".format(lower_str, upper_str)
            )
        lb = host_dict.get(_HORIZ_BEGIN_STD)
        ub = host_dict.get(_HORIZ_END_STD)
        if lb is None or ub is None:
            raise CCPPError(
                "Dimension '{}' requires '{}' and '{}' in the host "
                "metadata but they were not found".format(
                    dim, _HORIZ_BEGIN_STD, _HORIZ_END_STD
                )
            )
        used.update({_HORIZ_BEGIN_STD, _HORIZ_END_STD, upper_str})
        return '{}:{}'.format(lb.local_name, ub.local_name), used

    # General range: resolve both bounds independently
    lower_expr = _resolve_single_bound(lower_str, host_dict, used,
                                       suite_vars=suite_vars)
    if lower_expr is None:
        raise CCPPError(
            "Dimension lower bound '{}' in '{}' is not in the "
            "host metadata or suite-owned variables.{}".format(
                lower_str, dim,
                _format_available_std_names(host_dict, suite_vars, near=lower_str),
            )
        )
    upper_expr = _resolve_single_bound(upper_str, host_dict, used,
                                       suite_vars=suite_vars)
    if upper_expr is None:
        if upper_str.startswith('vertical_'):
            raise CCPPError(
                "Vertical dimension '{}' is not in the host metadata.{}".format(
                    upper_str,
                    _format_available_std_names(host_dict, suite_vars, near=upper_str),
                )
            )
        raise CCPPError(
            "Dimension '{}' is not in the host metadata or suite-owned "
            "variables.{}".format(
                dim,
                _format_available_std_names(host_dict, suite_vars, near=upper_str),
            )
        )
    if flip_vertical and upper_str in _VDIM_STDS:
        # Reverse-stride form for the vertical axis (top_at_one mismatch).
        return '{}:{}:-1'.format(upper_expr, lower_expr), used
    return '{}:{}'.format(lower_expr, upper_expr), used


def _build_merged_subscript(
    host_dims: List[str],
    local_subscript: List[str],
    phase: str,
    host_dict: Dict[str, HostVarEntry],
    suite_vars: Optional[Dict[str, 'SuiteVar']] = None,
    flip_vertical: bool = False,
) -> Tuple[str, Set[str]]:
    """Build a call subscript merging a local-name template with host dimensions.

    Walk *local_subscript* tokens left to right:

    * ``':'`` — dimension placeholder: consume the next entry from *host_dims*
      and emit the appropriate range expression (``lb:ub``, ``1:nlev``, etc.)
      via :func:`_one_dim_part`.
    * an integer literal — emitted verbatim.
    * any other token — explicit index using a CCPP standard name: resolved
      case-insensitively (standard names are case-insensitive) to the
      corresponding local Fortran name from *host_dict* (or *suite_vars*).
      The resolved standard name is added to the returned ``used`` set so the
      group cap emits a ``use <module>, only: <local>`` for it.  An
      unresolved non-literal token is a metadata error: subscript indices in
      a sliced ``local_name`` must be standard names with a defining source.

    When *local_subscript* is empty this is equivalent to calling
    :func:`_build_call_subscript` directly.

    Example
    -------
    local_subscript = [':', ':', 'index_of_water_vapor']
    host_dims       = ['horizontal_dimension', 'vertical_layer_dimension']
    phase = 'run'
    → '(lb:ub, 1:nlev, wv_idx)'
    """
    if not local_subscript:
        return _build_call_subscript(host_dims, phase, host_dict,
                                     suite_vars=suite_vars,
                                     flip_vertical=flip_vertical)

    dim_iter = iter(host_dims)
    parts: List[str] = []
    used: Set[str] = set()

    for token in local_subscript:
        token = token.strip()
        if token == ':':
            dim = next(dim_iter)
            part, u = _one_dim_part(dim, phase, host_dict,
                                    suite_vars=suite_vars,
                                    flip_vertical=flip_vertical)
            parts.append(part)
            used.update(u)
        elif token.isdigit():
            parts.append(token)
        else:
            key = token.lower()
            entry = host_dict.get(key)
            if entry is not None:
                # Use ``access_path`` so DDT-component subscript indices
                # (e.g. ``q(:,:,index_of_<X>)`` where index_of_X lives
                # on a DDT) resolve to the full DDT walk, not the bare
                # leaf name.  ``_render_value_expr`` (a) resolves baked
                # ``(instance_number)``/``(thread_number)`` placeholders
                # and (b) re-attaches any literal subscript stripped
                # from a ``local_name = foo(1)``-style declaration.
                parts.append(_render_value_expr(entry, host_dict))
                used.add(key)
            elif suite_vars and key in suite_vars:
                parts.append(
                    _substitute_scalar_idx(suite_vars[key].access_path, host_dict)
                )
                used.add(key)
            else:
                raise CCPPError(
                    "Subscript index '{}' in a sliced local_name is not a "
                    "known CCPP standard name in the host or suite metadata; "
                    "subscript indices must be standard names with a "
                    "defining source so the cap can resolve and import "
                    "the corresponding local variable".format(token)
                )

    return '({})'.format(', '.join(parts)), used


def _dim_has_vertical(dim: str) -> bool:
    """Return True if a dimension entry's upper bound is a vertical-axis
    standard name.

    Accepts both the bare form (``'vertical_layer_dimension'``) and the
    explicit ``lower:upper`` form (``'ccpp_constant_one:vertical_layer_dimension'``,
    ``'bot_idx:vertical_interface_dimension'``).
    """
    upper = dim.split(':', 1)[-1].strip() if ':' in dim else dim.strip()
    return upper in _VDIM_STDS


def _canonical_dim(dim: str) -> str:
    """Return a dimension entry in canonical ``lower:upper`` form for
    identity comparison.

    Three spellings of the implicit/default lower bound all collapse
    to a single representative:

    * bare ``foo`` (no explicit lower bound)
    * ``1:foo`` (the integer literal one)
    * ``ccpp_constant_one:foo`` (the standard name)

    Any *other* lower bound is distinct: ``2:foo`` is not the same
    axis as ``1:foo``, ``bar:foo`` is not the same as ``1:foo``, etc.
    Different lower bound describes a different sub-range and must
    not compare equal.

    No name aliasing on the upper bound happens here by default.
    ``horizontal_loop_extent`` and ``horizontal_dimension`` are
    different names — the :func:`metadata.legacy_compat` shim is the
    canonical place that rewrites the legacy name to the new one at
    parse time when ``--legacy-mode`` is enabled.  Without that shim
    the two should never appear on opposite sides of a host/scheme
    pairing.

    The one *opt-in* exception is the GFS dim-aliases shim
    (:mod:`metadata.dim_aliases`, ``--gfs-dim-aliases``): when enabled
    it collapses a small audited list of physically-equivalent
    standard names (e.g. ``adjusted_vertical_layer_dimension_for_radiation``
    -> ``vertical_layer_dimension``) on the *upper bound only*, so
    host and scheme metadata that use different historical spellings
    of the same axis compare equal here.  Variables keep their
    original standard names elsewhere.
    """
    if ':' in dim:
        lower, upper = dim.split(':', 1)
    else:
        lower, upper = _CCPP_CONSTANT_ONE, dim
    lower = lower.strip().lower()
    upper = upper.strip().lower()
    # Collapse the integer literal '1' and the standard name
    # 'ccpp_constant_one' to a single representative so all three
    # spellings of the default lower bound compare equal.
    if lower == '1':
        lower = _CCPP_CONSTANT_ONE
    # dim-aliases: transient GFS-physics shim.  No-op unless the
    # ``--gfs-dim-aliases`` CLI flag has been passed.  Only the upper
    # bound is rewritten; lower bounds (loop-begin control vars, etc.)
    # never alias.
    upper = dim_aliases.canonical(upper)
    return '{}:{}'.format(lower, upper)


def _substitute_scalar_idx(
    expr: str, host_dict: Dict[str, HostVarEntry],
) -> str:
    """Resolve registered scalar-index placeholders in a DDT access expr.

    :func:`metadata.variable_resolver._instance_subscript` bakes one
    placeholder per registered scalar-index dim into the access path of
    every HostVarEntry derived from a DDT-instance container.  The
    placeholders are *standard names* (e.g. ``instance_number``,
    ``thread_number``) drawn from
    :data:`metadata.registered_dimensions.SCALAR_INDEX_DIMS`; this
    function resolves each to the host's actual Fortran local name at
    codegen time.

    Resolution rules per placeholder:

    * Found in *host_dict* → substitute the host's ``local_name``.
    * Absent from *host_dict* → substitute the literal ``1`` (consistent
      with the ``instance_number`` paired-opt-in single-instance fallback;
      length-1 internal arrays still address correctly).

    Multi-pair access paths like ``foo(instance_number, thread_number)``
    are handled in a single pass — every registered placeholder in the
    expression is rewritten.
    """
    # Optimization: skip the work when no placeholder could possibly be
    # present.  ``(`` is the cheapest distinguishing token.
    if '(' not in expr:
        return expr
    out = expr
    for idx_std in SCALAR_INDEX_DIMS.values():
        # Multiple placeholders may appear: as a sole subscript
        # ``(idx_std)`` or as one of several ``(a, idx_std)``.  Replace
        # the bare std name token-wise, but only when it's clearly an
        # index placeholder (preceded by ``(`` or ``, `` and followed by
        # ``)`` or ``,``).  In practice _instance_subscript only emits
        # these inside a fresh subscript, so word-boundary replace is
        # safe; we use re.sub to enforce the word boundary.
        pattern = r'\b' + re.escape(idx_std) + r'\b'
        entry = host_dict.get(idx_std)
        replacement = entry.local_name if entry is not None else '1'
        out = re.sub(pattern, replacement, out)
    return out


# Backwards-compatibility shim — older code (and one external test) may
# still import the old name.  Forward to the generalized impl.
_substitute_instance_idx = _substitute_scalar_idx


def _render_value_expr(
    entry: HostVarEntry,
    host_dict: Dict[str, HostVarEntry],
) -> str:
    """Render *entry*'s full Fortran value-read expression.

    Combines two steps that callers usually need together:

    1. Resolve any baked registered scalar-index placeholders in the
       access path (``(instance_number)``, ``(thread_number)``) to the
       host's local Fortran names via :func:`_substitute_scalar_idx`.
    2. Re-attach any literal subscript that was stripped from the
       declared ``local_name`` at parse time (e.g. host metadata
       declaring ``local_name = nstf_name(1)`` parses into
       ``base='nstf_name'`` + ``local_subscript=['1']``; reading the
       value requires re-appending ``(1)``).  Std-name tokens inside
       the subscript are themselves resolved to host local names via
       :func:`metadata.variable_resolver._resolve_subscript`.

    Use this helper anywhere a host entry is rendered as a Fortran
    expression in generator output (active-expression translation,
    dimension-bound resolution, subscript-index tokens, subcycle
    loop-count expressions, etc.).  The scheme-arg base_expr +
    _build_merged_subscript path is the exception — that path consumes
    *entry.local_subscript* directly and interleaves it with scheme
    dimensions, so it must not be pre-joined here.
    """
    expr = _substitute_scalar_idx(entry.access_path, host_dict)
    if entry.local_subscript:
        sub = _resolve_subscript(', '.join(entry.local_subscript), host_dict)
        expr = '{}({})'.format(expr, sub)
    return expr


def _translate_active_expr(active: str, host_dict: Dict[str, HostVarEntry]) -> str:
    """Translate standard names in an ``active`` expression to local Fortran.

    Standard-name identifiers are replaced with the host entry's full
    Fortran access path (with any ``(instance_number)`` DDT-instance
    template resolved to the host's actual local name).  For free host
    variables this collapses to ``entry.local_name``; for DDT-component
    entries the substitution yields the fully qualified access path
    (e.g. ``instance_data(instance)%opt_array_flag``).
    """
    if not active:
        return ''

    def _replace(m: re.Match) -> str:
        word = m.group(0)
        entry = host_dict.get(word)
        if entry is None:
            return word
        return _render_value_expr(entry, host_dict)

    return FORTRAN_CONDITIONAL_REGEX.sub(_replace, active)


def _root_symbol(access_path: str) -> str:
    """Return the root Fortran symbol from an access path.

    This is the part before any ``%`` or ``(``, which is the name that
    appears in the ``use module, only: <name>`` statement.
    """
    return re.split(r'[%(]', access_path)[0]


_FORTRAN_ID_LIMIT_SV = 63


def _unique_suite_field(desired: str, std_name: str,
                        suite_vars: Dict[str, 'SuiteVar']) -> str:
    """Return a suite-data field name unique across existing suite vars.

    Two *distinct* suite-owned variables (different standard names) can be
    first produced by scheme args that happen to share a local name -- e.g.
    ``kdist`` for both ``longwave_gas_optics_object_for_RRTMGP`` and
    ``shortwave_gas_optics_object_for_RRTMGP``, or ``hrate`` for the lw/sw
    heating-rate tendencies.  The generated ``ccpp_<suite>_data`` DDT
    declares one component per suite var named by this field, and the group
    cap accesses it via the same name (``SuiteVar.access_path``), so the
    field MUST be unique -- otherwise Fortran rejects the duplicate
    component.

    Keep the bare name for the first occurrence (readable common case);
    disambiguate a later collision with a short, deterministic
    std_name-derived suffix (stable regardless of resolution order, since it
    keys on the standard name rather than a counter).
    """
    used = {sv.local_name for sv in suite_vars.values()}
    if desired not in used:
        return desired
    suffix = hashlib.sha1(std_name.encode('utf-8')).hexdigest()[:8]
    base = desired[:_FORTRAN_ID_LIMIT_SV - 1 - len(suffix)]
    return '{}_{}'.format(base, suffix)


########################################################################
# Data classes
########################################################################

@dataclass
class SuiteVar:
    """A suite-owned variable discovered during variable resolution.

    Suite-owned variables are not provided by the host model; they are
    first written by a scheme with ``intent(out)`` and then read by
    subsequent schemes.  They are declared in the generated
    ``ccpp_<suite>_data.F90`` module.

    Attributes
    ----------
    standard_name : str
    local_name : str
        Scheme's local variable name that first produces this variable.
    type_ : str
        Fortran type string from the scheme metadata.
    kind : str
        Optional kind parameter.
    units : str
    dimensions : list of str
    source_scheme : str
        Name of the scheme that first declared it (intent out).
    source_phase : str
    """
    standard_name: str
    local_name: str
    type_: str
    kind: str
    units: str
    dimensions: List[str]
    source_scheme: str
    source_phase: str
    suite_module_name: str = ''
    inst_access: str = '(1)'
    allocatable: bool = False

    @property
    def access_path(self) -> str:
        """Fortran access expression in the suite data module."""
        return 'ccpp_suite_data{}%{}'.format(self.inst_access, self.local_name)

    @property
    def module_name(self) -> str:
        return self.suite_module_name if self.suite_module_name else 'ccpp_suite_data'


@dataclass
class ResolvedArg:
    """One resolved argument at a scheme call site.

    Attributes
    ----------
    standard_name : str
    scheme_local_name : str
        Keyword name for the Fortran call (from the scheme metadata ``[ name ]``
        header).
    intent : str
        ``'in'``, ``'out'``, or ``'inout'``.
    is_optional : bool
    active : str
        Active condition in standard names (empty if always active).
    active_local : str
        Active condition translated to local Fortran names.
    source : str
        ``'host'``, ``'control'``, or ``'suite'``.
    host_entry : HostVarEntry or None
        The resolved host/control entry (``None`` for suite-owned vars
        that have already been declared before this call).
    suite_var : SuiteVar or None
        The suite data entry (``None`` for host/control vars).
    base_expr : str
        Fortran access path (without dimension subscripts).
    subscript : str
        Dimension subscript string, e.g. ``'(lb:ub, 1:nlev)'`` or ``''``.
    call_expr : str
        Full call-site expression: ``base_expr + subscript``.
    used_dim_std_names : set of str
        Standard names of host/control/suite dimension variables
        referenced in the subscript.  Used to drive USE statements and
        dummy-arg injection in the group cap.
    used_const_dim_std_names : set of str
        Standard names of *framework-constituent* dimension references
        (notably ``number_of_ccpp_constituents``) referenced in the
        subscript.  These do not produce USE statements (the value is
        reached via the per-instance constituent object), but they DO
        appear in the host-facing introspection inputs list — original
        capgen reports framework-constituent dim names there.
    needs_unit_transform : bool
    needs_kind_transform : bool
    unit_forward : str
        Fortran expression: host/suite → scheme (for pre-call, intent in/inout).
        Empty if no transformation needed.
    unit_backward : str
        Fortran expression: scheme → host/suite (for post-call, intent out/inout).
        Empty if no transformation needed.
    kind_scheme : str
        Kind declared in the scheme metadata.
    kind_host : str
        Kind of the host/suite variable.
    temp_name : str
        Name for the transformation temporary (``local_name + '_l'``).
    ptr_name : str
        Name for the optional pointer (``local_name + '_p'``).
    transform_case : int
        1 = direct, 2 = pointer only, 3 = transform only, 4 = pointer+transform.
    """
    standard_name: str
    scheme_local_name: str
    intent: str
    is_optional: bool
    active: str
    active_local: str
    source: str
    host_entry: Optional[HostVarEntry]
    suite_var: Optional['SuiteVar']
    base_expr: str
    subscript: str
    call_expr: str
    used_dim_std_names: Set[str]
    needs_unit_transform: bool
    needs_kind_transform: bool
    unit_forward: str
    unit_backward: str
    kind_scheme: str
    kind_host: str
    temp_name: str
    ptr_name: str
    transform_case: int
    scheme_dimensions: List[str]
    # ``needs_vert_flip`` is True when host and scheme metadata disagree on
    # ``top_at_one``: the host-side access expression carries a reverse-stride
    # subscript on the vertical axis and the transform pipeline copies through
    # a temp local just like a unit conversion does.  Composes with unit/kind
    # transforms when present.
    needs_vert_flip: bool = False
    is_constituent_arg: bool = False
    # ``is_constituent`` is True if the scheme metadata flagged this variable
    # with any of ``constituent``, ``advected``, or ``molar_mass`` (a non-default
    # value).  Distinct from :attr:`is_constituent_arg`, which marks the
    # ``ccpp_constituent_properties_t`` register-phase array argument.
    is_constituent: bool = False
    # For ``source == 'constituent'`` args: module that owns the constituent
    # symbols this arg references (typically the suite cap module).  ``None``
    # for non-constituent args.
    constituent_module_name: Optional[str] = None
    # Extra symbols (beyond :attr:`root_symbol`) that the group cap must USE
    # from :attr:`constituent_module_name`.  Typically ``index_of_<X>``
    # integers referenced inside the constituent array subscript.
    constituent_extra_symbols: Set[str] = field(default_factory=set)
    # Framework-constituent dim std names referenced in the subscript
    # (e.g. ``number_of_ccpp_constituents`` as the trailing axis of
    # ``ccpp_constituents``).  These are not USE'd from any module — the
    # value is reached via the per-instance constituent object — but
    # they're surfaced as inputs by the introspection routines in
    # :mod:`generator.host_cap`.  Replaces the older trick of stuffing
    # them into :attr:`used_dim_std_names`.
    used_const_dim_std_names: Set[str] = field(default_factory=set)
    # Real (un-mangled) constituent base standard names that this arg's
    # subscript indexes via ``index_of_<X>``.  The Fortran symbol in
    # :attr:`constituent_extra_symbols` is mangled to fit the 63-char
    # identifier limit (see :func:`_index_symbol_name`), so its suffix is
    # NOT a reliable source of the real standard name.  This set preserves
    # the real names verbatim so ``ccpp_initialize_constituents`` can pass
    # them to ``%const_index`` as the lookup key (and list them in
    # ``ccpp_model_const_stdnames``).
    constituent_index_std_names: Set[str] = field(default_factory=set)

    @property
    def needs_transform(self) -> bool:
        return (self.needs_unit_transform or self.needs_kind_transform
                or self.needs_vert_flip)

    @property
    def module_name(self) -> Optional[str]:
        """Module to USE for this argument (``None`` for control vars)."""
        if self.source == 'constituent':
            return self.constituent_module_name
        if self.host_entry is not None:
            return self.host_entry.module_name
        if self.suite_var is not None:
            return self.suite_var.module_name
        return None

    @property
    def root_symbol(self) -> str:
        """Root Fortran symbol name for the USE statement."""
        return _root_symbol(self.base_expr)


@dataclass
class ResolvedCall:
    """All resolved arguments for one scheme phase call."""
    scheme_name: str
    phase: str
    args: List[ResolvedArg] = field(default_factory=list)
    # Fortran module that exports the scheme's subroutines.  Defaults to
    # ``scheme_name`` for the common-case where the .meta table name and
    # the Fortran module name match; overridden by the ``module_name``
    # attribute in ``[ccpp-table-properties]`` when the two differ.
    scheme_module: str = ''

    @property
    def used_modules(self) -> Dict[str, Set[str]]:
        """Return ``{module_name: {symbol, ...}}`` for USE-statement building."""
        result: Dict[str, Set[str]] = {}
        for arg in self.args:
            mod = arg.module_name
            if mod is not None:
                sym = arg.root_symbol
                result.setdefault(mod, set()).add(sym)
            for dim_std in arg.used_dim_std_names:
                pass  # dim vars added separately by the group cap writer
        return result


def _resolve_subcycle_loop_bound(
    loop_str: Optional[str],
    host_dict: Dict[str, HostVarEntry],
    suite_vars: Optional[Dict[str, 'SuiteVar']] = None,
) -> Tuple[str, str]:
    """Resolve a subcycle ``loop=`` attribute into Fortran source.

    Returns ``(fortran_expr, std_name)`` where:

    * *fortran_expr* is the value to splice into ``do ccpp_loop_counter
      = 1, <fortran_expr>``.  For an absent or literal-integer value
      this is the literal itself; for a CCPP standard name it is the
      host's (or suite's) Fortran local name.
    * *std_name* is the resolved CCPP standard name (lower-cased) when
      the loop bound was a symbol, otherwise the empty string.  Used by
      the group cap to (a) emit ``use <module>, only: <local>`` for
      host-owned bounds and (b) inject the bound as a dummy argument
      when it's a control variable.

    Raises ``CCPPError`` if the loop bound is a non-integer token that
    doesn't resolve against the host/control dictionary or the suite's
    interstitial variables.
    """
    if loop_str is None:
        return '1', ''
    raw = loop_str.strip()
    if not raw:
        return '1', ''
    # Integer literal — pass through verbatim (also handles negative
    # numbers for completeness, though those aren't physically meaningful).
    try:
        int(raw)
        return raw, ''
    except ValueError:
        pass
    # Treat as a CCPP standard name.  Standard names are lower-cased at
    # parse time; the XML attribute may carry a mixed-case spelling, so
    # normalise before lookup.
    key = raw.lower()
    entry = host_dict.get(key)
    if entry is not None:
        # Use the full access_path — for a free module variable this is
        # just the local name, but for a DDT-component the access path is
        # ``<instance>%<component>`` (or
        # ``<instance>(instance_number)%<component>`` when the parent is
        # in an instance-dimensioned array; resolve that template here,
        # and re-attach any literal local_subscript from a
        # ``local_name = foo(1)``-style declaration).
        return _render_value_expr(entry, host_dict), key
    if suite_vars and key in suite_vars:
        suite_var = suite_vars[key]
        return suite_var.access_path, key
    raise CCPPError(
        "Subcycle loop=\"{}\" is not an integer literal and does not "
        "resolve to a CCPP standard name in the host/control metadata or "
        "as a suite-owned variable; declare it (typically in the "
        "type=control or type=host table) before using it as a subcycle "
        "loop bound".format(loop_str)
    )


@dataclass
class ResolvedSubcycle:
    """A subcycle ``do`` loop wrapping one or more scheme run calls.

    Only appears in ``phase_calls['run']``; non-run phases are always flat
    (subcycle boundaries are not meaningful for init/final).

    Attributes
    ----------
    loop : str
        Loop-count Fortran expression to splice into ``do
        ccpp_loop_counter = 1, <loop>``.  An integer literal when the
        XML attribute was a literal; otherwise the host's local Fortran
        name resolved from the CCPP standard name in the XML.
    loop_std_name : str
        The resolved CCPP standard name (lower-cased) when *loop* came
        from a symbol; empty string when *loop* is an integer literal.
        Drives USE-statement emission and control-variable dummy-arg
        injection in the group cap.
    calls : list of :data:`PhaseItem`
        Items wrapped by this loop.  Element types: :class:`ResolvedCall`
        for scheme calls; :class:`ResolvedSubcycle` for nested loops
        (yes, this is recursive — SDFs may declare arbitrary subcycle
        nesting and the resolver preserves that structure for the cap
        emitter to render as nested ``do`` loops).
    """
    loop: str
    # Use a forward-ref string for ``PhaseItem`` because the alias is
    # defined just below this class.  Runtime type-checking still works.
    calls: List['PhaseItem'] = field(default_factory=list)
    loop_std_name: str = ''


# Type alias for the contents of a phase's call list (and a subcycle's
# inner items).  PhaseItem is itself the union of plain scheme calls and
# nested subcycles, so a phase / subcycle can carry arbitrary nesting.
PhaseItem = Union[ResolvedCall, ResolvedSubcycle]


def iter_phase_calls(items: List[PhaseItem]):
    """Yield every :class:`ResolvedCall` in *items*, recursing into
    nested :class:`ResolvedSubcycle` items.  Subcycles themselves are
    not yielded — only the leaf scheme calls."""
    for item in items:
        if isinstance(item, ResolvedCall):
            yield item
        elif isinstance(item, ResolvedSubcycle):
            # Recurse so nested ``ResolvedSubcycle`` items unwrap too.
            yield from iter_phase_calls(item.calls)


def iter_phase_subcycles(items: List[PhaseItem]):
    """Yield every :class:`ResolvedSubcycle` in *items*, including nested
    subcycles.  Used by the group cap to emit nested ``do`` loops and
    by ``_collect_host_io`` to find every loop bound."""
    for item in items:
        if isinstance(item, ResolvedSubcycle):
            yield item
            yield from iter_phase_subcycles(item.calls)


@dataclass
class ResolvedGroup:
    """Resolution results for one suite group."""
    group_name: str
    # One list of PhaseItem objects per phase.  Non-run phases contain only
    # ResolvedCall; the run phase may also contain ResolvedSubcycle items.
    phase_calls: Dict[str, List[PhaseItem]] = field(default_factory=dict)
    # Module → symbols referenced by dimension lookups in this group.
    dim_uses: Dict[str, Set[str]] = field(default_factory=dict)


# auto-clone-constituents: snapshot of one consumer-side
# ``is_constituent`` scheme arg whose ``std_name`` has no
# register-phase source.  Carries everything the emitter needs to
# synthesise a ``%instantiate(...)`` call in ``<suite>_register``.
# Captured at resolve time so the emitter doesn't reach back into
# raw scheme metadata.
@dataclass
class AutoCloneEntry:
    std_name: str
    long_name: str
    diag_name: str          # diagnostic_name or local_name fallback
    units: str
    vertical_dim: str       # standard name of the vertical axis
    advected: bool
    molar_mass: float
    default_value: Optional[float]
    min_value: Optional[float]
    water_species: Optional[bool]
    mixing_ratio_type: Optional[str]


@dataclass
class SuiteResolution:
    """Complete resolution result for one suite.

    Attributes
    ----------
    constituent_register_calls : list of (scheme_name, scheme_local_name)
        For each register-phase scheme arg whose ``type`` is
        ``ccpp_constituent_properties_t`` (intent=out, allocatable), records
        the (scheme, scheme arg local name) pair.  The suite cap uses this
        list to emit two-pass merge logic that populates the host's
        ``ccpp_model_constituents_object`` with the per-scheme constituent
        arrays.  Empty when no register-phase scheme produces constituents.
    constituent_index_names : list of str
        Sorted list of base-constituent standard names that need an
        ``index_of_<X>`` integer emitted in the suite cap.  These are the
        REAL (un-mangled) standard names, collected from every
        ``source='constituent'`` ResolvedArg's
        ``constituent_index_std_names`` set -- NOT recovered from the
        (possibly mangled) ``index_of_*`` Fortran symbols.  Used by
        :mod:`generator.suite_cap` to emit the index declarations and the
        ``ccpp_model_constituents_object%const_index`` population calls
        in ``<suite>_init``.
    uses_constituents : bool
        True iff any scheme arg in this suite has ``source='constituent'``
        (excluding the legacy register-phase ``is_constituent_arg``).
        Drives suite-cap emission of the ccpp_constituents /
        ccpp_constituent_tendencies pointers and related state.
    suite_init_call : ResolvedCall or None
        Resolved call for the suite-level ``<init>`` scheme (if any).
        The scheme's ``init`` phase is invoked once per ``<suite>_init``
        call (per instance, per suite), after the group ``state_alloc``
        loop and before the state transition to
        ``CCPP_SUITE_FRAMEWORK_INITIALIZED``.
    suite_final_call : ResolvedCall or None
        Resolved call for the suite-level ``<final>`` scheme (if any).
        The scheme's ``final`` phase is invoked once per ``<suite>_final``
        call (per instance, per suite), before the state transition to
        ``CCPP_SUITE_UNREGISTERED``.
    """
    suite_name: str
    groups: List[ResolvedGroup] = field(default_factory=list)
    suite_vars: Dict[str, SuiteVar] = field(default_factory=dict)
    uses_instance_dimension: bool = False
    constituent_register_calls: List[Tuple[str, str]] = field(default_factory=list)
    constituent_index_names: List[str] = field(default_factory=list)
    uses_constituents: bool = False
    suite_init_call:  Optional[ResolvedCall] = None
    suite_final_call: Optional[ResolvedCall] = None
    # auto-clone-constituents: populated only when the legacy shim is
    # enabled.  Each entry is one is_constituent consumer whose
    # std_name has no register-phase source; the suite cap synthesises
    # a ``%instantiate(...)`` call per entry into the per-suite
    # dynamic-constituents buffer.  Empty when the shim is off.
    auto_cloned_constituents: List[AutoCloneEntry] = field(default_factory=list)

    # auto-clone-constituents: this property exists so consumer
    # emitters (host_constituents, suite_cap) never have to read
    # ``auto_cloned_constituents`` themselves.  When the legacy
    # auto-clone shim retires, drop the second clause below and the
    # property collapses to ``bool(self.constituent_register_calls)``;
    # every consumer keeps working without changes.
    @property
    def needs_dynamic_constituents_buffer(self) -> bool:
        """True iff the per-suite ``<suite>_dynamic_constituents`` buffer
        must be declared and populated for this suite.

        Single source of truth for the predicate.
        """
        return bool(
            self.constituent_register_calls
            # auto-clone-constituents:
            or self.auto_cloned_constituents
        )


########################################################################
# Argument resolution helpers
########################################################################

def _local_name_conflict(
    name: str,
    existing_names: Set[str],
) -> str:
    """Return *name* with a numeric suffix if it already exists in *existing_names*.

    Fortran identifiers are **case-insensitive**, so the collision check
    is performed in lowercase: ``cp_l`` and ``CP_l`` are the same name
    to the compiler and must not be emitted side-by-side as two locals.
    The returned name preserves the input case (so generated source
    keeps the metadata's spelling), but callers MUST add the lowercased
    name to *existing_names* so subsequent calls see the collision.
    """
    if name.lower() not in existing_names:
        return name
    # Split on last '_' to find the suffix ('_l' or '_p').
    if '_' in name:
        base, suffix = name.rsplit('_', 1)
        suffix = '_' + suffix
    else:
        base, suffix = name, ''
    n = 2
    while True:
        candidate = '{}_{}{}' .format(base, n, suffix)
        if candidate.lower() not in existing_names:
            return candidate
        n += 1


#: Standard names of the two loop-context control variables (see
#: doc/redesign_prompt.md §4.2).  Scheme args declaring these resolve
#: to the generated do-loop locals emitted by the group cap — they are
#: in scope only inside a ``<subcycle>`` block.
_LOOP_COUNTER_STD = 'ccpp_loop_counter'
_LOOP_EXTENT_STD  = 'ccpp_loop_extent'


def _resolve_one_arg(
    scheme_var,           # MetaVar from scheme metadata
    phase: str,
    host_dict: Dict[str, HostVarEntry],
    suite_vars: Dict[str, SuiteVar],
    scheme_name: str,
    used_local_names: Set[str],
    suite_name: str = '',
    loop_context: Optional[List[Tuple[str, Optional[str]]]] = None,
    const_stds: Set[str] = frozenset(),
) -> ResolvedArg:
    """Resolve one scheme argument against host/control/suite dictionaries.

    Implements the four variable-matching cases from Section 8.4.

    Parameters
    ----------
    scheme_var : MetaVar
        The variable entry from the scheme's phase metadata section.
    phase : str
        The current scheme phase (affects subscripting and error messages).
    host_dict : dict
        Flat host+control variable dict.
    suite_vars : dict
        Accumulated suite-owned variables (mutated if Case 2 applies).
    scheme_name : str
        Name of the enclosing scheme (for error messages).
    used_local_names : set of str
        Already-used local variable names in this group cap function (for
        conflict resolution of temp/pointer names).
    loop_context : list of (str, str or None), optional
        Stack of ``(loop_count_expr, loop_std_name)`` pairs for the
        enclosing ``<subcycle>`` blocks, outermost first.  Empty (or
        omitted) when the call is not inside any subcycle.  Used to
        resolve ``ccpp_loop_counter`` / ``ccpp_loop_extent`` scheme
        args against the generated do-loop locals.

    Returns
    -------
    ResolvedArg

    Raises
    ------
    CCPPError
        Case 3: variable not found and intent is not ``'out'``.
    """
    std_name = scheme_var.standard_name
    intent   = scheme_var.intent or 'in'
    local    = scheme_var.local_name
    optional = scheme_var.optional

    # ---- loop-context std names (ccpp_loop_counter / ccpp_loop_extent) -
    # Per design (doc/redesign_prompt.md §4.2): these are scoped to the
    # body of a ``<subcycle>``.  Resolve them against the generated do-
    # loop locals; outside a subcycle, raise a clear error pointing at
    # the SDF contract rather than the host metadata.
    if std_name in (_LOOP_COUNTER_STD, _LOOP_EXTENT_STD):
        if not loop_context:
            raise CCPPError(
                "Scheme '{scheme}' (phase '{phase}') requests standard "
                "name '{std}' for argument '{local}', but the scheme is "
                "not placed inside a ``<subcycle>`` block in the suite "
                "definition file.\n"
                "\n"
                "'{std}' is a loop-context control variable scoped to a "
                "subcycle do-loop body (see doc/redesign_prompt.md "
                "§4.2).  Either wrap the scheme in ``<subcycle "
                "loop=\"…\">…</subcycle>`` in the SDF, or remove the "
                "'{std}' argument from the scheme metadata.".format(
                    scheme=scheme_name,
                    phase=phase,
                    std=std_name,
                    local=local,
                )
            )
        # Resolve to the OUTERMOST enclosing subcycle.  Per the deferred
        # item in doc/migration.md §8 ("Nested subcycle
        # ccpp_loop_counter semantics"), nested-loop schemes that need
        # the innermost counter aren't supported yet — every cam-sima
        # / SCM use we've audited reads the OUTERMOST counter only.
        outer_count_expr, _outer_std = loop_context[0]
        if std_name == _LOOP_COUNTER_STD:
            # The group cap emits the outermost do-loop with local
            # variable ``ccpp_loop_counter`` (group_cap._loop_counter_name
            # depth 1).  Match that name verbatim — it's in scope wherever
            # this scheme call site is emitted.
            call_expr = 'ccpp_loop_counter'
        else:  # _LOOP_EXTENT_STD
            # ``ccpp_loop_extent`` is the OUTERMOST subcycle's loop
            # count — either an integer literal (e.g. ``'3'``) or a
            # host-resolved local name (e.g. ``'n_sub'``) depending on
            # how the SDF declared ``loop=``.
            call_expr = outer_count_expr
        return ResolvedArg(
            standard_name=std_name,
            scheme_local_name=local,
            intent=intent,
            is_optional=optional,
            active='',
            active_local='',
            source='control',
            host_entry=None,
            suite_var=None,
            base_expr=call_expr,
            subscript='',
            call_expr=call_expr,
            used_dim_std_names=set(),
            needs_unit_transform=False,
            needs_kind_transform=False,
            unit_forward='',
            unit_backward='',
            kind_scheme=scheme_var.kind,
            kind_host='',
            temp_name='',
            ptr_name='',
            transform_case=1,
            scheme_dimensions=list(scheme_var.dimensions),
            used_const_dim_std_names=set(),
        )

    # ---- detect constituent register args (special-cased) ---------------
    # Schemes that register dynamic constituents declare an intent=out
    # ``ccpp_constituent_properties_t`` allocatable array.  Those arguments
    # are NOT promoted to suite-owned data — they are local temporaries in
    # the suite cap's ``<suite>_register`` subroutine, used to count and
    # populate the host's ``ccpp_model_constituents_object`` via the
    # two-pass merge pattern.
    is_constituent = (
        phase == 'register'
        and intent == 'out'
        and scheme_var.type.strip() == _CONST_PROP_TYPE
    )
    if is_constituent:
        return ResolvedArg(
            standard_name=std_name,
            scheme_local_name=local,
            intent=intent,
            is_optional=optional,
            active='',
            active_local='',
            source='constituent',
            host_entry=None,
            suite_var=None,
            base_expr='scheme_consts',
            subscript='',
            call_expr='scheme_consts',
            used_dim_std_names=set(),
            needs_unit_transform=False,
            needs_kind_transform=False,
            unit_forward='',
            unit_backward='',
            kind_scheme=scheme_var.kind,
            kind_host='',
            temp_name='',
            ptr_name='',
            transform_case=1,
            scheme_dimensions=list(scheme_var.dimensions),
            is_constituent_arg=True,
        )

    # ---- detect constituent-sourced scheme args (framework auto-provision)
    # Returns a synthesised ``source='constituent'`` ResolvedArg for:
    #   * scheme args declaring a framework-known std name
    #     (ccpp_constituents, ccpp_constituent_tendencies,
    #     number_of_ccpp_constituents, ccpp_constituent_properties);
    #   * scheme args flagged ``is_constituent`` with intent=in/inout
    #     (routed to ccpp_constituents(<slice>, index_of_<X>));
    #   * scheme args flagged ``is_constituent`` with intent=out and
    #     standard name starting with ``tendency_of_`` (routed to
    #     ccpp_constituent_tendencies(<slice>, index_of_<base>)).
    # Returns ``None`` if the arg is not constituent-related.
    const_arg = _resolve_constituent_arg(
        scheme_var, phase, host_dict, suite_vars, scheme_name, suite_name,
        const_stds=const_stds,
    )
    if const_arg is not None:
        return const_arg

    # ---- determine source -----------------------------------------------
    host_entry: Optional[HostVarEntry] = host_dict.get(std_name)

    # active is a host-model-only attribute; read it from the host entry only.
    # When the scheme arg is optional, the group cap emits the
    # pointer-association pattern (transform_case 2 / 4) so the scheme
    # sees PRESENT()=.false. when the active condition is false.  When
    # the scheme arg is non-optional, the group cap emits a runtime
    # guard before the call: if (.not. (active)) raise errflg and return.
    # The suite designer is responsible for ensuring the active condition
    # holds at call time; the guard converts a silent invalid-memory read
    # into a clear runtime error.
    active = host_entry.active if host_entry is not None else ''
    suite_var: Optional[SuiteVar]             = suite_vars.get(std_name)

    if host_entry is not None and suite_var is None:
        source = 'control' if host_entry.is_control else 'host'
    elif suite_var is not None and host_entry is None:
        source = 'suite'
    elif host_entry is None and suite_var is None:
        # Case 2 or 3.
        if intent == 'out':
            # This scheme is the first (in phase->scheme order) to provide
            # this variable, so it DEFINES the suite-owned storage that the
            # framework allocates in ``ccpp_<suite>_data``.  A character
            # definer must specify a concrete length: ``len=*`` (assumed
            # length) is only valid for a dummy argument, never for stored
            # data, and a later ``len=*`` consumer/writer has no concrete
            # length to inherit.  Reject it here with a clear message rather
            # than emitting an undeclarable ``character(len=*)`` component.
            if ((scheme_var.type or '').strip().lower() == 'character'
                    and (scheme_var.kind or '').strip() == 'len=*'):
                raise CCPPError(
                    "Suite-owned character variable '{}' (standard_name='{}') "
                    "is first defined as intent(out) by scheme '{}' (phase "
                    "'{}') with kind='len=*'; the defining scheme must declare "
                    "a concrete length (e.g. kind=len=512) because the "
                    "framework allocates storage for it in the suite data "
                    "module.  Assumed length (len=*) is permitted only on "
                    "later schemes that consume or re-write the "
                    "variable.".format(
                        local, std_name, scheme_name, phase,
                    )
                )
            inst_entry = host_dict.get('instance_number')
            inst_access = '({})'.format(inst_entry.local_name) if inst_entry else '(1)'
            suite_var = SuiteVar(
                standard_name=std_name,
                local_name=_unique_suite_field(local, std_name, suite_vars),
                type_=scheme_var.type,
                kind=scheme_var.kind,
                units=scheme_var.units,
                dimensions=list(scheme_var.dimensions),
                source_scheme=scheme_name,
                source_phase=phase,
                suite_module_name='ccpp_{}_data'.format(suite_name),
                inst_access=inst_access,
                allocatable=scheme_var.allocatable,
            )
            suite_vars[std_name] = suite_var
            source = 'suite'
        else:
            raise CCPPError(
                "Variable '{}' (standard_name='{}') requested by scheme "
                "'{}' phase '{}' with intent({}) is not provided by the host "
                "metadata or by any prior scheme; "
                "either add it to the host metadata or ensure an earlier "
                "scheme provides it with intent(out)".format(
                    local, std_name, scheme_name, phase, intent
                )
            )
    else:
        # Both found — host takes precedence (suite data shouldn't duplicate host).
        source = 'control' if host_entry.is_control else 'host'

    # ---- build access expression -----------------------------------------
    if host_entry is not None:
        # ``host_entry.access_path`` is the verbatim form from
        # build_flat_host_dict; for DDT-instance arrays it carries the
        # ``(instance_number)`` template that needs codegen-time resolution.
        base_expr = _substitute_instance_idx(host_entry.access_path, host_dict)
        host_dims = host_entry.dimensions
        host_units = host_entry.units
        host_kind  = host_entry.kind
        host_type  = host_entry.type
        host_allocatable = host_entry.allocatable
    else:
        base_expr  = suite_var.access_path
        host_dims  = suite_var.dimensions
        host_units = suite_var.units
        host_kind  = suite_var.kind
        host_type  = suite_var.type_
        host_allocatable = suite_var.allocatable

    # ---- type identity check ---------------------------------------------
    # The defining source (host metadata or the first scheme to write a
    # suite-owned var) sets the variable's type; every subsequent consumer
    # must agree.  Numeric/kind coercion happens via the transform pipeline,
    # but the *type kind* itself (real vs integer vs logical vs DDT) must
    # match identically — there is no transform that crosses those.
    if (host_type or '').strip().lower() != (scheme_var.type or '').strip().lower():
        raise CCPPError(
            "Variable '{}' (standard_name='{}'): {} declares type='{}' but "
            "scheme '{}' declares type='{}'; cross-type assignment is not "
            "supported, the scheme metadata must match the defining type".format(
                local, std_name, source, host_type,
                scheme_name, scheme_var.type,
            )
        )

    # ---- rank check ------------------------------------------------------
    if len(host_dims) != len(scheme_var.dimensions):
        raise CCPPError(
            "Variable '{}' (standard_name='{}'): {} declares rank {} "
            "(dimensions={}) but scheme '{}' declares rank {} "
            "(dimensions={}); the scheme metadata's dimension list must "
            "match the defining rank".format(
                local, std_name, source, len(host_dims),
                list(host_dims), scheme_name, len(scheme_var.dimensions),
                list(scheme_var.dimensions),
            )
        )

    # ---- per-position dimension identity check ---------------------------
    # Each dimension entry is canonicalized to ``lower:upper`` form (bare
    # ``X`` -> ``ccpp_constant_one:X``) and compared for strict identity.
    # No name aliasing happens here by default; the legacy-compat shim
    # rewrites deprecated names at parse time, and the opt-in GFS
    # dim-aliases shim (--gfs-dim-aliases) collapses a small audited
    # list of physically-equivalent upper-bound names inside
    # ``_canonical_dim`` itself.
    for pos, (hdim, sdim) in enumerate(zip(host_dims, scheme_var.dimensions)):
        if _canonical_dim(hdim) != _canonical_dim(sdim):
            raise CCPPError(
                "Variable '{}' (standard_name='{}'): {} declares "
                "dimension {} as '{}' but scheme '{}' declares it as "
                "'{}'; per-position dimension entries must match "
                "(the scheme metadata's dimension list must name the "
                "defining axes; bare names are equivalent to "
                "ccpp_constant_one:<name>, all other lower bounds are "
                "distinct)".format(
                    local, std_name, source, pos, hdim,
                    scheme_name, sdim,
                )
            )

    # ---- allocatable compatibility check ---------------------------------
    # An actual argument that is not allocatable cannot be passed to an
    # allocatable dummy.  The reverse direction (allocatable host -> plain
    # assumed-shape dummy) is legal Fortran and is permitted: the scheme
    # simply forgoes access to the allocation status.
    if scheme_var.allocatable and not host_allocatable:
        raise CCPPError(
            "Variable '{}' (standard_name='{}'): scheme '{}' declares "
            "allocatable=True but {} declares allocatable=False; "
            "an allocatable dummy cannot receive a non-allocatable actual "
            "argument".format(
                local, std_name, scheme_name, source
            )
        )

    # ---- vertical-flip detection (top_at_one mismatch) ------------------
    # Both host and scheme declare a top_at_one attribute (default False).
    # A mismatch triggers a reverse-stride substitution on the host-side
    # subscript at the vertical-dim position so the array section is read
    # (and written) in flipped order.  Only meaningful when the variable
    # actually has a vertical dimension.
    if host_entry is not None:
        host_top_at_one = host_entry.top_at_one
    else:
        host_top_at_one = False
    scheme_top_at_one = bool(getattr(scheme_var, 'top_at_one', False))
    needs_vert_flip = (
        host_top_at_one != scheme_top_at_one
        and any(_dim_has_vertical(d) for d in host_dims)
    )

    if host_allocatable:
        # Allocatable actual arguments must omit explicit dimension ranges:
        # the callee declares the dummy as allocatable too and assumes the
        # array shape from the actual.
        subscript: str = ''
        used_dim_std: Set[str] = set()
    else:
        local_sub = host_entry.local_subscript if host_entry is not None else []
        subscript, used_dim_std = _build_merged_subscript(
            host_dims, local_sub, phase, host_dict, suite_vars=suite_vars,
            flip_vertical=needs_vert_flip,
        )
    call_expr = base_expr + subscript

    # Scalar horizontal_dimension in a physics phase: the scheme is asking
    # for the size of the horizontal slice it actually receives.  During run
    # the host passes a chunk (lb:ub); during the other physics phases the
    # loop bounds collapse to 1:ncols.  In both cases (ub - lb + 1) yields
    # the correct extent, so we synthesise it from the loop-bound control
    # variables and bypass the host's full-domain scalar (e.g. ncols).
    if (phase in _PHYSICS_PHASES
            and std_name == _HORIZ_DIM_STD
            and not scheme_var.dimensions):
        lb = host_dict.get(_HORIZ_BEGIN_STD)
        ub = host_dict.get(_HORIZ_END_STD)
        if lb is None or ub is None:
            raise CCPPError(
                "Scheme '{}' phase '{}' requests scalar '{}' but the host "
                "metadata lacks '{}'/'{}' (required to compute the per-call "
                "horizontal extent)".format(
                    scheme_name, phase, _HORIZ_DIM_STD,
                    _HORIZ_BEGIN_STD, _HORIZ_END_STD
                )
            )
        call_expr = '({} - {} + 1)'.format(ub.local_name, lb.local_name)
        used_dim_std.update({_HORIZ_BEGIN_STD, _HORIZ_END_STD})

    # ---- active expression translation -----------------------------------
    active_local = _translate_active_expr(active, host_dict)

    # ---- transformation detection ----------------------------------------
    # Normalise both unit strings so that equivalent spellings (``m2`` and
    # ``m+2``) compare equal and do not appear as bogus mismatches.
    host_units   = _normalize_unit_string(host_units)
    scheme_units = _normalize_unit_string(scheme_var.units)
    scheme_kind  = scheme_var.kind

    fwd_fn  = find_unit_conversion(host_units, scheme_units) if host_units != scheme_units else None
    bwd_fn  = find_unit_conversion(scheme_units, host_units) if host_units != scheme_units else None

    needs_unit  = fwd_fn is not None or bwd_fn is not None
    # Unit mismatch with no known conversion is an error only if units differ.
    if host_units != scheme_units and not needs_unit:
        raise CCPPError(
            "Variable '{}' (standard_name='{}'): host units '{}' differ from "
            "scheme '{}' units '{}' but no unit conversion is known; "
            "add a conversion to metadata/unit_conversion.py or fix the "
            "metadata".format(
                local, std_name, host_units, scheme_name, scheme_units
            )
        )

    # Character kind handling (len=N / len=*):
    #   - len=* in the scheme is always compatible with any host len=<N>.
    #   - Matching specific len=N values need no transform (naturally equal).
    #   - Mismatched specific lengths (len=N vs len=M) are a metadata error;
    #     the scheme must declare len=* or match the defining metadata exactly.
    _host_is_len  = host_kind.startswith('len=')  if host_kind  else False
    _scheme_is_len = scheme_kind.startswith('len=') if scheme_kind else False
    if _host_is_len or _scheme_is_len:
        if scheme_kind != 'len=*' and host_kind != scheme_kind:
            raise CCPPError(
                "Character variable '{}' (standard_name='{}'): host declares "
                "kind='{}' but scheme '{}' declares kind='{}'; scheme must "
                "use kind=len=* or match the defining kind exactly".format(
                    local, std_name, host_kind, scheme_name, scheme_kind
                )
            )
        needs_kind = False
    else:
        needs_kind = bool(host_kind) and bool(scheme_kind) and host_kind != scheme_kind

    # Forward transformation expression (host/suite → scheme local).
    # ``call_expr`` already carries the flipped vertical subscript when
    # ``needs_vert_flip`` is True, so the unit-conversion formula naturally
    # composes the flip on the host-side RHS.  For a pure-flip case (no
    # unit conversion) we emit a plain copy ``temp = host(...flipped)``.
    # For a pure-kind case (host.kind != scheme.kind, no unit conversion,
    # no vertical flip), we emit an explicit ``TYPE(host, kind=K)`` cast
    # so the temp is actually assigned; without this branch the temp was
    # declared and the call site referenced it, but no assignment was
    # emitted -- gfortran fell back to implicit typing and yielded a
    # garbage / Inf value at runtime.
    unit_forward = ''
    if needs_unit and fwd_fn is not None and intent in ('in', 'inout'):
        unit_forward = _apply_transform_formula(fwd_fn, call_expr, scheme_kind)
    elif needs_kind and intent in ('in', 'inout'):
        unit_forward = _kind_cast_expr(
            scheme_var.type, call_expr, scheme_kind,
            local=local, std_name=std_name, scheme_name=scheme_name,
        )
    elif needs_vert_flip and not needs_unit and intent in ('in', 'inout'):
        unit_forward = call_expr

    # Backward transformation expression (scheme local → host/suite).
    unit_backward = ''
    if needs_unit and bwd_fn is not None and intent in ('out', 'inout'):
        unit_backward_expr = '{}_l'.format(local)
        unit_backward = _apply_transform_formula(bwd_fn, unit_backward_expr, host_kind)
    elif needs_kind and intent in ('out', 'inout'):
        unit_backward = _kind_cast_expr(
            scheme_var.type, '{}_l'.format(local), host_kind,
            local=local, std_name=std_name, scheme_name=scheme_name,
        )
    elif needs_vert_flip and not needs_unit and intent in ('out', 'inout'):
        unit_backward = '{}_l'.format(local)

    needs_transform = needs_unit or needs_kind or needs_vert_flip

    # ---- local variable names (transformation temp + pointer) ------------
    # ``used_local_names`` stores the LOWERCASED names so collision
    # detection is Fortran-case-insensitive (see _local_name_conflict).
    temp_name = ''
    ptr_name  = ''
    if needs_transform:
        candidate = '{}_l'.format(local)
        temp_name = _local_name_conflict(candidate, used_local_names)
        used_local_names.add(temp_name.lower())

    if optional:
        candidate = '{}_p'.format(local)
        ptr_name = _local_name_conflict(candidate, used_local_names)
        used_local_names.add(ptr_name.lower())

    # ---- transform case --------------------------------------------------
    if optional and needs_transform:
        transform_case = 4
    elif optional:
        transform_case = 2
    elif needs_transform:
        transform_case = 3
    else:
        transform_case = 1

    return ResolvedArg(
        standard_name=std_name,
        scheme_local_name=local,
        intent=intent,
        is_optional=optional,
        active=active,
        active_local=active_local,
        source=source,
        host_entry=host_entry,
        suite_var=suite_var if source == 'suite' else None,
        base_expr=base_expr,
        subscript=subscript,
        call_expr=call_expr,
        used_dim_std_names=used_dim_std,
        needs_unit_transform=needs_unit,
        needs_kind_transform=needs_kind,
        unit_forward=unit_forward,
        unit_backward=unit_backward,
        kind_scheme=scheme_kind,
        kind_host=host_kind,
        temp_name=temp_name,
        ptr_name=ptr_name,
        transform_case=transform_case,
        scheme_dimensions=list(scheme_var.dimensions),
        needs_vert_flip=needs_vert_flip,
        is_constituent=scheme_var.is_constituent,
    )


########################################################################
# Constituent-source synthesis (framework auto-provisioning)
########################################################################

def _const_dim_part(
    dim: str,
    phase: str,
    host_dict: Dict[str, HostVarEntry],
    suite_vars: Optional[Dict[str, 'SuiteVar']] = None,
) -> Tuple[str, Set[str], Set[str], Set[str]]:
    """One-dim subscript with framework-constituent dim recognition.

    Returns ``(part, used_host_std, used_const_std, used_const_dim_std)``.

    The trailing dim ``number_of_ccpp_constituents`` is emitted as
    ``':'`` (whole-axis slice).  The std name is added to
    ``used_const_dim_std`` so the introspection routine
    (:func:`generator.host_cap._collect_host_io`) can include it in
    its inputs list — original capgen reports framework-constituent dim
    names there.  No USE statement is emitted for the name: it isn't in
    host_dict (the framework provides it via the per-instance
    constituent object), so ``_collect_group_uses`` and
    ``_extra_dim_ctrl_entries`` both silently skip it.  All other dims
    fall through to :func:`_one_dim_part` and their std names go into
    ``used_host_std``.

    ``used_const_std`` collects framework-constituent *symbols* that
    need a USE statement (currently unused at this layer; reserved for
    future framework-constituent symbols that might appear inside a
    dim expression).
    """
    if dim == _CONST_NUM_STD or dim.endswith(':' + _CONST_NUM_STD):
        return ':', set(), set(), {_CONST_NUM_STD}
    part, used = _one_dim_part(dim, phase, host_dict, suite_vars=suite_vars)
    return part, used, set(), set()


def _build_const_subscript(
    dimensions: List[str],
    phase: str,
    host_dict: Dict[str, HostVarEntry],
    suite_vars: Optional[Dict[str, 'SuiteVar']] = None,
) -> Tuple[str, Set[str], Set[str], Set[str]]:
    """Build a subscript for a constituent-sourced arg.

    Like :func:`_build_call_subscript` but recognises
    ``number_of_ccpp_constituents`` as a whole-axis slice.  Returns
    ``(subscript, used_host_std, used_const_std, used_const_dim_std)``;
    *used_const_std* collects framework-constituent symbols that need
    a USE statement (reserved for future use), and
    *used_const_dim_std* collects framework-constituent dim std names
    (e.g. ``number_of_ccpp_constituents``) for introspection.
    """
    if not dimensions:
        return '', set(), set(), set()
    parts: List[str] = []
    used_host: Set[str] = set()
    used_const: Set[str] = set()
    used_const_dim: Set[str] = set()
    for dim in dimensions:
        part, uh, uc, ucd = _const_dim_part(dim, phase, host_dict, suite_vars)
        parts.append(part)
        used_host.update(uh)
        used_const.update(uc)
        used_const_dim.update(ucd)
    return ('({})'.format(', '.join(parts)),
            used_host, used_const, used_const_dim)


def _resolve_constituent_arg(
    scheme_var,
    phase: str,
    host_dict: Dict[str, HostVarEntry],
    suite_vars: Dict[str, 'SuiteVar'],
    scheme_name: str,
    suite_name: str,
    const_stds: Set[str] = frozenset(),
) -> Optional[ResolvedArg]:
    """Synthesise a ``source='constituent'`` ResolvedArg, or return ``None``.

    Per-instance access pattern: every constituent state lookup goes
    through ``ccpp_model_constituents_obj(<inst_num>)%<member>`` where
    *inst_num* is the host's local name for ``instance_number`` (or
    ``1`` if the host doesn't declare it).  The ``index_of_<X>``
    integers and ``ccpp_model_const_stdnames`` parameter array are
    module-level scalars on ``ccpp_host_constituents`` (identical
    across instances).

    Host metadata always wins: if the host declares ``std_name`` as a
    regular variable, this routine returns ``None`` and normal host-arg
    resolution takes over.  Constituent auto-provisioning is reserved
    for names the host has not claimed.

    Three argument categories are recognised:

    1. **Framework-named std_name** — one of
       :data:`_FRAMEWORK_CONST_STDS` or starts with ``index_of_`` *and*
       not declared by the host.

       * ``ccpp_constituents`` → ``ccpp_model_constituents_obj(inst)%vars_layer<sub>``
       * ``ccpp_constituent_tendencies`` → ``...%vars_layer_tend<sub>``
       * ``ccpp_constituent_properties`` → ``...%const_metadata<sub>``
       * ``number_of_ccpp_constituents`` → ``...%num_layer_vars`` (scalar)
       * ``index_of_<X>`` → ``index_of_<X>`` (module-level integer)

    2. **Base constituent** — ``scheme_var.is_constituent`` true, intent
       in/inout, std_name not a ``tendency_of_*``.  Routed to
       ``...%vars_layer(<slice>, index_of_<std_name>)``.

    3. **Constituent tendency** — ``scheme_var.is_constituent`` true,
       intent=out, std_name=``tendency_of_<X>``.  Routed to
       ``...%vars_layer_tend(<slice>, index_of_<X>)``.

    Mismatched combinations are hard errors (see error messages below).
    The constituent arg always carries ``instance_number`` in
    ``used_dim_std_names`` (when the host declares it) so the group cap
    auto-injects it as a dummy via :func:`_extra_dim_ctrl_entries`.
    """
    std_name = scheme_var.standard_name
    intent   = scheme_var.intent or 'in'
    local    = scheme_var.local_name
    optional = scheme_var.optional
    scheme_dims = list(scheme_var.dimensions)

    is_tendency_name  = std_name.startswith(_TEND_PREFIX)
    is_index_name     = std_name.startswith(_INDEX_PREFIX)
    is_framework_name = std_name in _FRAMEWORK_CONST_STDS or is_index_name

    # Rule (b): an UNFLAGGED consumer of a name that some scheme declares as a
    # constituent -- a base constituent (``advected``, read via vars_layer) or a
    # constituent tendency (``constituent`` on a ``tendency_of_*`` producer, read
    # via vars_layer_tend) -- resolves to the SAME framework column the
    # producer/registration backs.  Consumers must not re-flag it: whether a
    # given standard name is a constituent or an ordinary variable is the host's
    # decision (CAM-SIMA vs CCPP-SCM), so we infer it from the
    # scheme-metadata-wide set instead of the consumer's own metadata.
    # Host/earlier-suite provision WINS: if this host declares the name, or a
    # prior scheme already produced it as an ordinary variable, defer to normal
    # resolution (a genuine constituent is in neither host_dict nor suite_vars).
    is_known_constituent = std_name in const_stds
    inferred_constituent_consumer = (
        is_known_constituent
        and not scheme_var.is_constituent
        and intent in ('in', 'inout')
        and not (host_dict and std_name in host_dict)
        and std_name not in suite_vars
    )

    # Host/suite provides it -> not a framework auto-provision.  Defer to
    # normal host/suite resolution when:
    #
    #   * the host declares this std_name as a regular variable (e.g. a
    #     `protected integer` named `ntcw` with standard_name =
    #     index_of_..._tracer_concentration_array), OR
    #   * an earlier-phase scheme already produced it (it is in suite_vars).
    #     ``suite_vars`` accumulates across phases in chronological order
    #     (register, init, timestep_init, run, ...), so e.g. a band index
    #     produced by ``rrtmgp_inputs_setup_init`` (intent=out) is visible
    #     here when ``rrtmgp_sw_cloud_optics_run`` consumes it (intent=in).
    #
    # Constituent auto-provisioning is reserved for framework-named
    # std_names that nothing else provides.
    if is_framework_name and (
        (host_dict and std_name in host_dict) or std_name in suite_vars
    ):
        return None

    # A scheme that OUTPUTS ``index_of_<X>`` is producing an ordinary index
    # variable (e.g. ``rrtmgp_inputs_setup`` computing the diagnostic
    # shortwave band index), NOT a constituent index -- constituent indices
    # are read-only module integers bound by ``%const_index`` and are never
    # written by a scheme.  Defer so it becomes a suite var that later-phase
    # consumers resolve via the gate above.  (Index names produced by some
    # OTHER scheme but consumed here have already been caught by the
    # suite_vars branch above; this handles the producing arg itself, whose
    # name is not yet in suite_vars on first occurrence.)
    if is_index_name and intent == 'out':
        return None

    constituent_module = _constituent_module_name(suite_name)
    inst_entry = host_dict.get(_INSTANCE_NUM_STD) if host_dict else None
    inst_local = inst_entry.local_name if inst_entry else None
    inst_idx   = inst_local if inst_local else '1'

    def _common_kwargs(base_expr, subscript, call_expr,
                       used_host_std, extra_symbols,
                       used_const_dim_std=None,
                       index_std_names=None):
        used_host_std = set(used_host_std)
        if inst_local:
            used_host_std.add(_INSTANCE_NUM_STD)
        return dict(
            standard_name=std_name,
            scheme_local_name=local,
            intent=intent,
            is_optional=optional,
            active='',
            active_local='',
            source='constituent',
            host_entry=None,
            suite_var=None,
            base_expr=base_expr,
            subscript=subscript,
            call_expr=call_expr,
            used_dim_std_names=used_host_std,
            needs_unit_transform=False,
            needs_kind_transform=False,
            unit_forward='',
            unit_backward='',
            kind_scheme=scheme_var.kind,
            kind_host='',
            temp_name='',
            ptr_name='',
            transform_case=1,
            scheme_dimensions=scheme_dims,
            is_constituent=scheme_var.is_constituent,
            constituent_module_name=constituent_module,
            constituent_extra_symbols=extra_symbols,
            used_const_dim_std_names=(set(used_const_dim_std)
                                      if used_const_dim_std else set()),
            constituent_index_std_names=(set(index_std_names)
                                         if index_std_names else set()),
        )

    # ---- Path 1a: index_of_<X> — module-level integer, no per-instance --
    if is_index_name:
        # Mangle long std_names down to a Fortran-legal 63-char symbol;
        # identity for short names, so existing fixtures are unaffected.
        # ``_INDEX_PREFIX`` is already part of std_name -- strip then
        # re-add via the helper for uniform truncation.
        index_base = std_name[len(_INDEX_PREFIX):]
        index_sym = _index_symbol_name(index_base)
        return ResolvedArg(**_common_kwargs(
            base_expr=index_sym, subscript='', call_expr=index_sym,
            used_host_std=set(), extra_symbols={index_sym},
            index_std_names={index_base},
        ))

    # ---- Path 1b: framework-named std_name → DDT member -----------------
    if is_framework_name:
        member = _FRAMEWORK_NAME_TO_MEMBER[std_name]
        subscript, used_host_std, used_const_std, used_const_dim_std = \
            _build_const_subscript(
                scheme_dims, phase, host_dict, suite_vars,
            )
        base_expr = '{}({})%{}'.format(_CONST_OBJ_VAR, inst_idx, member)
        call_expr = base_expr + subscript if subscript else base_expr
        # Framework-constituent dim refs (e.g. number_of_ccpp_constituents)
        # travel on the dedicated used_const_dim_std_names channel — no
        # USE statement, but surfaced as inputs by the introspection
        # routines in generator.host_cap.
        return ResolvedArg(**_common_kwargs(
            base_expr=base_expr, subscript=subscript, call_expr=call_expr,
            used_host_std=used_host_std,
            extra_symbols={_CONST_OBJ_VAR} | used_const_std,
            used_const_dim_std=used_const_dim_std,
        ))

    if not (scheme_var.is_constituent or inferred_constituent_consumer):
        return None  # not constituent-related

    # ---- Provider gate (mirrors original-capgen ConstituentVarDict.find_variable)
    # A constituent-FLAGGED consumer whose standard name is actually
    # PROVIDED elsewhere is an ordinary interstitial, not a constituent:
    #
    #   * the host declares it in metadata            -> host scope, or
    #   * an earlier scheme produced it intent=out    -> already recorded
    #     in ``suite_vars`` (the resolver walks calls in execution order,
    #     so a producer that runs before this consumer is visible here).
    #
    # Original capgen only auto-creates a constituent when its
    # ``find_variable`` returns None (nothing in host or suite scope
    # provides the name).  Without this gate a dry mixing ratio that is
    # PRODUCED by e.g. ``wet_to_dry_water_vapor`` (intent=out, unflagged)
    # but CONSUMED by ``kessler`` (advected=True) gets split into a suite
    # array (producer) and a constituent column (consumer) AND spuriously
    # auto-registered as a constituent.  Defer to normal host/suite
    # dispatch so producer and consumer share one storage location and the
    # name is never registered as a constituent.  Tendencies (intent=out,
    # ``tendency_of_*``) are genuine constituent-tendency OUTPUTS and are
    # never gated.
    if not is_tendency_name and (
        std_name in suite_vars or std_name in host_dict
    ):
        return None

    # ---- Paths 2/3: is_constituent base or tendency ---------------------
    if intent == 'out':
        if not is_tendency_name:
            raise CCPPError(
                "Constituent-flagged scheme arg '{}' (standard_name='{}', "
                "scheme='{}', phase='{}') has intent=out but its standard "
                "name does not start with 'tendency_of_'.  Physics phases "
                "may only produce constituent tendencies; new base "
                "constituents must be declared via a "
                "ccpp_constituent_properties_t argument in a register-phase "
                "scheme.".format(local, std_name, scheme_name, phase)
            )
        base_std = std_name[len(_TEND_PREFIX):]
        member   = 'vars_layer_tend'
    else:  # in / inout
        if is_tendency_name:
            # Consumer of a constituent tendency (rule b): read the SAME column
            # the producer wrote -- ccpp_constituent_tendencies(<slice>,
            # index_of_<base>).  Reaching here means it is a recognised
            # constituent tendency (flagged, or inferred via const_stds)
            # that neither the host nor an earlier suite var provides.
            base_std = std_name[len(_TEND_PREFIX):]
            member   = 'vars_layer_tend'
        else:
            base_std = std_name
            member   = 'vars_layer'

    leading_sub, used_host_std = _build_call_subscript(
        scheme_dims, phase, host_dict, suite_vars=suite_vars,
    )
    index_sym = _index_symbol_name(base_std)
    if leading_sub:
        subscript = leading_sub[:-1] + ', ' + index_sym + ')'
    else:
        subscript = '(' + index_sym + ')'
    base_expr = '{}({})%{}'.format(_CONST_OBJ_VAR, inst_idx, member)
    call_expr = base_expr + subscript

    return ResolvedArg(**_common_kwargs(
        base_expr=base_expr, subscript=subscript, call_expr=call_expr,
        used_host_std=used_host_std,
        extra_symbols={index_sym, _CONST_OBJ_VAR},
        index_std_names={base_std},
    ))


########################################################################
# Suite resolution
########################################################################

def resolve_suite(
    suite,                            # generator.suite_xml.Suite
    scheme_store,                     # metadata.variable_resolver.SchemeStore
    host_dict: Dict[str, HostVarEntry],
    phases: Optional[List[str]] = None,
) -> SuiteResolution:
    """Resolve all scheme arguments for every group and phase in *suite*.

    Parameters
    ----------
    suite : Suite
        Parsed suite XML object.
    scheme_store : SchemeStore
        Scheme metadata organised for lookup.
    host_dict : dict
        Flat host+control variable dictionary.
    phases : list of str, optional
        Phases to resolve.  Defaults to all six phases in chronological order
        (register first), so that suite-owned variables produced by
        ``_register`` are visible as dimensions or as ``intent(in)`` reads
        in subsequent phases.

    Returns
    -------
    SuiteResolution

    Raises
    ------
    CCPPError
        On any variable matching failure.
    """
    if phases is None:
        phases = ['register', 'init', 'timestep_init', 'run',
                  'timestep_final', 'final']

    # Validate up-front: every scheme name referenced by this suite —
    # in any group (including nested subcycles/subcols) AND the
    # suite-level <init>/<final> hooks — MUST be present in the scheme
    # store.  When a scheme is missing the resolver silently emits
    # empty phase entries, which the cap generator then writes as a
    # syntactically valid but semantically empty group cap (the user
    # gets a successful build with the wrong runtime behaviour).
    # Surface the configuration error here with the full list of
    # missing schemes and a remediation pointer.
    referenced_schemes: List[str] = list(suite.all_scheme_names())
    if suite.init_scheme:
        referenced_schemes.append(suite.init_scheme)
    if suite.final_scheme:
        referenced_schemes.append(suite.final_scheme)
    missing: List[str] = []
    seen_missing: Set[str] = set()
    for sname in referenced_schemes:
        if not scheme_store.has_scheme(sname) and sname not in seen_missing:
            missing.append(sname)
            seen_missing.add(sname)
    if missing:
        raise CCPPError(
            "Suite '{suite}' references {n} scheme(s) whose metadata is "
            "not loaded:\n\n"
            "    {names}\n\n"
            "These schemes are listed in the SDF but no matching "
            "``[ccpp-table-properties] type = scheme`` table is available "
            "in the metadata files passed via ``--scheme-files``.  Add "
            "the missing scheme ``.meta`` files to the generator's "
            "--scheme-files argument.".format(
                suite=suite.name,
                n=len(missing),
                names='\n    '.join(missing),
            )
        )

    # Detect whether any host variable uses the instance dimension
    # specifically (multi-instance API marker on SuiteResolution).  This
    # is narrower than the general "registered scalar-index dim" check —
    # we want to know only about the multi-instance pair here, not
    # number_of_threads or future additions.
    uses_instance = any(
        'number_of_instances' in entry.dimensions
        for entry in host_dict.values()
    )

    suite_vars: Dict[str, SuiteVar] = {}
    # Pre-create one ResolvedGroup per SDF group so the phase-major loop below
    # can append each phase's calls to the right group.
    resolved_groups: List[ResolvedGroup] = [
        ResolvedGroup(group_name=group.name) for group in suite.groups
    ]

    # Resolve PHASE-MAJOR, GROUP-MINOR (groups in SDF order within each phase),
    # mirroring the runtime execution hierarchy: the host completes EVERY
    # group's ``init`` before any group's ``run``, every group's
    # ``timestep_init`` before any ``run``, and so on.  ``suite_vars``
    # accumulates in that true execution order, so a variable produced in an
    # earlier phase by ANY group is visible to a consumer in a later phase of
    # ANY group (cross-group cross-phase provision).  Within a single phase,
    # groups resolve in SDF order, so an earlier group may provide to a later
    # one (e.g. ``physics_before_coupler`` -> ``physics_after_coupler``, which
    # the host runs sequentially with coupling in between); a same-phase
    # consumer that precedes its producer is still correctly rejected.
    #
    # (The previous nesting was group-major — each group through all its
    # phases — which made a variable produced by a later group's ``init``
    # invisible to an earlier group's ``run`` even though, at runtime, all
    # inits precede all runs.)
    for phase in phases:
        for group, resolved_group in zip(suite.groups, resolved_groups):
            used_local_names_phase: Set[str] = set()

            if phase == 'run':
                # Preserve subcycle structure for run-phase loop generation.
                items_for_phase = _resolve_run_phase(
                    group, phase, scheme_store, host_dict, suite_vars,
                    used_local_names_phase,
                    suite_name=suite.name,
                )
            else:
                # Non-run phases: flatten all subcycles and silently
                # deduplicate scheme names within the group.  A scheme that
                # appears multiple times in the suite XML (typically because
                # it runs once per constituent in the ``run`` phase) must
                # still have its register/init/finalize entry points invoked
                # exactly once per group — matches ``design_init_dedup.md``.
                scheme_names_flat = _dedup_scheme_names(
                    _collect_scheme_names(group)
                )
                items_for_phase = _resolve_flat_phase(
                    scheme_names_flat, phase, scheme_store, host_dict,
                    suite_vars, used_local_names_phase,
                    suite_name=suite.name,
                )

            if items_for_phase:
                resolved_group.phase_calls[phase] = items_for_phase

    # Collect dimension USE info once every phase/group is resolved, so
    # ``suite_vars`` is complete (a dimension may reference a suite var
    # produced by any group in any phase).
    for resolved_group in resolved_groups:
        resolved_group.dim_uses = _collect_dim_uses(
            resolved_group, host_dict, suite_vars=suite_vars,
        )

    # Constituent register calls: gather the (scheme_name, scheme_local_name)
    # pairs for every register-phase arg that was flagged as a constituent.
    # The suite cap uses these to emit two-pass merge logic.
    constituent_calls: List[Tuple[str, str]] = []
    for resolved_group in resolved_groups:
        for resolved_call in iter_phase_calls(resolved_group.phase_calls.get('register', [])):
            for arg in resolved_call.args:
                if arg.is_constituent_arg:
                    constituent_calls.append(
                        (resolved_call.scheme_name, arg.scheme_local_name)
                    )
    # Walk every constituent-sourced arg (excluding the legacy
    # register-phase ccpp_constituent_properties_t case) and collect:
    #   * uses_constituents  — whether any constituent state is referenced
    #   * constituent_index_names — base std names X needing an index_of_X
    uses_constituents = False
    index_names: Set[str] = set()
    for resolved_group in resolved_groups:
        for items in resolved_group.phase_calls.values():
            for resolved_call in iter_phase_calls(items):
                for arg in resolved_call.args:
                    if arg.source != 'constituent' or arg.is_constituent_arg:
                        continue
                    uses_constituents = True
                    # Collect the REAL (un-mangled) base standard names, not
                    # the mangled symbol suffix.  ``ccpp_initialize_constituents``
                    # passes these verbatim to ``%const_index``; a mangled key
                    # would never match a registered constituent and leave the
                    # ``index_of_<X>`` integer at its 0 default -> out-of-bounds
                    # subscript at run time.
                    index_names.update(arg.constituent_index_std_names)
    constituent_index_names = sorted(index_names)

    # Under option A the constituent object is generator-owned (lives in
    # the ccpp_host_constituents module), so the host is no longer
    # required to declare ``ccpp_model_constituents_object`` in its
    # type=host metadata.  No validation is needed here.

    # ---- suite-level <init> / <final> schemes ------------------------------
    # SDF v2.0 schema accepts an optional single ``<init>`` and ``<final>``
    # scheme name at the suite root.  Resolve each to a ResolvedCall against
    # the scheme's ``init`` / ``final`` phase metadata respectively.  The
    # local-name dedup set is fresh per call (these calls live outside any
    # group and don't share locals with group phases).
    suite_init_call:  Optional[ResolvedCall] = None
    suite_final_call: Optional[ResolvedCall] = None
    if suite.init_scheme:
        suite_init_locals: Set[str] = set()
        suite_init_call = _resolve_one_call(
            suite.init_scheme, 'init', scheme_store, host_dict,
            suite_vars, suite_init_locals, suite_name=suite.name,
        )
        if suite_init_call is None:
            raise CCPPError(
                "Suite '{}' declares <init>{}</init> but scheme '{}' "
                "has no ``init`` phase in its metadata.".format(
                    suite.name, suite.init_scheme, suite.init_scheme,
                )
            )
    if suite.final_scheme:
        suite_final_locals: Set[str] = set()
        suite_final_call = _resolve_one_call(
            suite.final_scheme, 'final', scheme_store, host_dict,
            suite_vars, suite_final_locals, suite_name=suite.name,
        )
        if suite_final_call is None:
            raise CCPPError(
                "Suite '{}' declares <final>{}</final> but scheme '{}' "
                "has no ``final`` phase in its metadata.".format(
                    suite.name, suite.final_scheme, suite.final_scheme,
                )
            )

    # auto-clone-constituents: collect synthesised %instantiate
    # snapshots when the legacy shim is enabled.  Returns [] when
    # disabled (no-op for default builds).
    auto_cloned = _collect_auto_clone_entries(resolved_groups, scheme_store)

    return SuiteResolution(
        suite_name=suite.name,
        groups=resolved_groups,
        suite_vars=suite_vars,
        constituent_register_calls=constituent_calls,
        uses_instance_dimension=uses_instance,
        constituent_index_names=constituent_index_names,
        uses_constituents=uses_constituents,
        suite_init_call=suite_init_call,
        suite_final_call=suite_final_call,
        # auto-clone-constituents: empty in default builds.
        auto_cloned_constituents=auto_cloned,
    )


def validate_init_dimensions(suite_res: SuiteResolution) -> None:
    """Reject suite-owned vars whose suite-time allocation can't be sized.

    capgen allocates every non-allocatable, dimensioned suite-owned
    variable once in ``suite_data_init_fields``, which runs at the very
    start of ``<suite>_init`` -- before any ``init`` / ``timestep_init`` /
    ``run`` scheme code.  Only the ``register`` phase completes earlier, so
    a dimension whose value is written by a scheme in any later phase is not
    yet set when the allocation happens (the size would be uninitialised
    memory).  Such a variable must instead be declared ``allocatable``
    (Fortran ``allocatable, intent(out)`` + metadata ``allocatable = True``)
    so its producing scheme allocates it once the size is known.

    This is sound (no false positives) but intentionally partial: it sees
    only dimensions a *scheme* writes.  A host that recomputes a host-owned
    dimension in its own driver each step is invisible here -- there is no
    metadata signal for it.

    Raises
    ------
    CCPPError
        If a non-allocatable suite-owned variable is dimensioned by a
        standard name written by a scheme in a phase after ``register``.
    """
    written_after_register: Set[str] = set()
    for resolved_group in suite_res.groups:
        for phase, items in resolved_group.phase_calls.items():
            if phase == 'register':
                continue
            for resolved_call in iter_phase_calls(items):
                for arg in resolved_call.args:
                    if (arg.intent or 'in') in ('out', 'inout'):
                        written_after_register.add(arg.standard_name)

    for suite_var in suite_res.suite_vars.values():
        if suite_var.allocatable or not suite_var.dimensions:
            continue
        for dim in suite_var.dimensions:
            for token in dim.split(':'):
                token = token.strip()
                if token in written_after_register:
                    raise CCPPError(
                        "Suite-owned variable '{var}' is allocated by the "
                        "suite at init time (in suite_data_init_fields), but "
                        "its dimension '{dim}' is written by a scheme in a "
                        "phase after 'register', so its size is not yet known "
                        "when the allocation runs (the allocation would use "
                        "uninitialised memory).\n"
                        "  Declare '{var}' allocatable -- 'allocatable, "
                        "intent(out)' in the producing scheme's Fortran and "
                        "'allocatable = True' in its metadata -- so the scheme "
                        "allocates it once '{dim}' is set.".format(
                            var=suite_var.standard_name, dim=token,
                        )
                    )


# auto-clone-constituents: BEGIN legacy-shim helpers.  Delete this
# block together with the rest of the auto-clone-constituents
# touchpoints; nothing else in the resolver references these.

def _vertical_dim_of(scheme_var) -> str:
    """Extract the vertical-axis standard name from a constituent
    consumer's ``dimensions`` list.

    Returns the upper-bound std name of the first vertical-axis
    dimension entry (matches :data:`_VDIM_STDS`).  Falls back to
    ``'vertical_layer_dimension'`` when the scheme arg carries no
    vertical dim (e.g. a 1-D horizontal-only consumer) — that's the
    framework default and matches original capgen's behaviour.
    """
    for dim in getattr(scheme_var, 'dimensions', ()) or ():
        upper = dim.split(':', 1)[-1].strip().lower() if ':' in dim else dim.strip().lower()
        if upper in _VDIM_STDS:
            return upper
    return 'vertical_layer_dimension'


def _synthesised_long_name_from_std(std_name: str) -> str:
    """auto-clone-constituents: fall back to a human-readable long_name
    derived from the std_name when the metadata doesn't supply one.

    Mirrors original capgen's auto-clone behaviour: replace each
    underscore with a space, then capitalise the first character.
    ``cloud_liquid_dry_mixing_ratio`` → ``Cloud liquid dry mixing ratio``.

    Keeps the auto-clone shim's emitted ``%instantiate(long_name=...)``
    consistent with what original capgen produces so existing legacy
    fixtures (e.g. CAM-SIMA's advection_test) don't need a metadata
    edit just for the long_name property.
    """
    return std_name.replace('_', ' ').capitalize()


def _make_auto_clone_entry(scheme_var) -> 'AutoCloneEntry':
    """Snapshot one scheme MetaVar into an :class:`AutoCloneEntry`.

    Captures every field the emitter needs to synthesise a
    ``%instantiate(...)`` call: the required kwargs (std_name,
    long_name, diag_name, units, vertical_dim) and the optional
    kwargs (advected, molar_mass, default_value, min_value,
    water_species, mixing_ratio_type) with ``None`` for any optional
    the metadata didn't set so the emitter can omit it.
    """
    # ``diagnostic_name`` is a property that falls back to local_name
    # when neither diagnostic_name nor diagnostic_name_fixed was set.
    diag = scheme_var.diagnostic_name or scheme_var.local_name
    # auto-clone-constituents: when the scheme metadata omits the
    # long_name attribute, synthesise one from the std_name (matches
    # original capgen's behaviour — see ``_synthesised_long_name_from_std``).
    long_name = (
        scheme_var.long_name
        or _synthesised_long_name_from_std(scheme_var.standard_name)
    )
    return AutoCloneEntry(
        std_name=scheme_var.standard_name,
        long_name=long_name,
        diag_name=diag,
        units=scheme_var.units,
        vertical_dim=_vertical_dim_of(scheme_var),
        advected=bool(scheme_var.advected),
        molar_mass=float(scheme_var.molar_mass or 0.0),
        default_value=scheme_var.default_value,
        min_value=scheme_var.min_value,
        water_species=scheme_var.water_species,
        mixing_ratio_type=scheme_var.mixing_ratio_type,
    )


def _lookup_scheme_var(scheme_store, scheme_name, phase, scheme_local_name):
    """Return the scheme MetaVar matching *scheme_local_name* in
    *scheme_name*'s *phase*, or ``None`` if not found.

    Used by the auto-clone collector to recover the full scheme
    MetaVar (which carries the constituent-property fields) from a
    ResolvedArg, which only carries a slim subset.
    """
    vars_list = scheme_store.variables_for(scheme_name, phase)
    if not vars_list:
        return None
    for mv in vars_list:
        if mv.local_name == scheme_local_name:
            return mv
    return None


def _collect_auto_clone_entries(resolved_groups, scheme_store):
    """Walk every ``is_constituent`` consumer arg and produce one
    :class:`AutoCloneEntry` per unique standard name.

    Skips:

    * register-phase ``ccpp_constituent_properties_t`` args
      (``is_constituent_arg`` is True for those — the scheme handles
      its own registration explicitly);
    * ``tendency_of_*`` std names (tendencies consume a constituent
      but are not themselves a constituent registration);
    * framework-named std names (``ccpp_constituents``,
      ``ccpp_constituent_tendencies``, ``ccpp_constituent_properties``,
      ``number_of_ccpp_constituents``, and ``index_of_*``) — these
      reference the framework-provided constituent ARRAYS or scalar
      counts, not individual species, and are not registrations;
    * duplicates within the suite (first-occurrence wins; the
      framework's runtime ``is_match`` would dedupe across the
      eventual ``%new_field`` calls anyway).

    No-op when the auto-clone shim is disabled — returns an empty
    list so :class:`SuiteResolution.auto_cloned_constituents` stays
    empty in default builds.
    """
    if not auto_clone_constituents.is_enabled():
        return []
    out: List['AutoCloneEntry'] = []
    seen: Set[str] = set()
    for resolved_group in resolved_groups:
        for items in resolved_group.phase_calls.values():
            for resolved_call in iter_phase_calls(items):
                for arg in resolved_call.args:
                    if arg.source != 'constituent' or arg.is_constituent_arg:
                        continue
                    std = (arg.standard_name or '').strip().lower()
                    if not std or std.startswith('tendency_of_'):
                        continue
                    # auto-clone-constituents: framework-named std
                    # names (the whole constituent buffer / tendency
                    # buffer / properties array / count, and
                    # ``index_of_<X>`` integers) resolve through the
                    # same ``source='constituent'`` channel but they
                    # are NOT individual constituent registrations —
                    # skip them so they don't get a spurious
                    # synthesised %instantiate call.
                    if (std in _FRAMEWORK_CONST_STDS
                            or std.startswith(_INDEX_PREFIX)):
                        continue
                    if std in seen:
                        continue
                    seen.add(std)
                    scheme_var = _lookup_scheme_var(
                        scheme_store, resolved_call.scheme_name,
                        resolved_call.phase, arg.scheme_local_name,
                    )
                    if scheme_var is None:
                        # Should not happen — the resolver only
                        # produces ResolvedArg from a real MetaVar —
                        # but fail loudly rather than silently emit
                        # a malformed %instantiate.
                        raise CCPPError(
                            "auto-clone-constituents: cannot locate "
                            "scheme metadata for '{}' (scheme '{}', "
                            "phase '{}') while collecting auto-clone "
                            "entries".format(
                                arg.scheme_local_name,
                                resolved_call.scheme_name,
                                resolved_call.phase,
                            )
                        )
                    out.append(_make_auto_clone_entry(scheme_var))
    return out

# auto-clone-constituents: END legacy-shim helpers.


def _collect_scheme_names(group) -> List[str]:
    """Return ordered list of scheme names from a group, expanding subcycles/subcols."""
    names: List[str] = []
    from generator.suite_xml import SuiteScheme, SuiteSubcycle, SuiteSubcol
    for item in group.items:
        if isinstance(item, SuiteScheme):
            names.append(item.name)
        elif isinstance(item, (SuiteSubcycle, SuiteSubcol)):
            for scheme_name in item.scheme_names():
                names.append(scheme_name)
    return names


def _dedup_scheme_names(scheme_names: List[str]) -> List[str]:
    """Return *scheme_names* with duplicates removed (first occurrence kept).

    Used by :func:`resolve_suite` for non-run phases: a scheme that appears
    more than once in the suite XML still has its register/init/finalize
    entry points invoked exactly once per group, while preserving the order
    of first appearance.
    """
    seen: Set[str] = set()
    deduped: List[str] = []
    for scheme_name in scheme_names:
        if scheme_name in seen:
            continue
        seen.add(scheme_name)
        deduped.append(scheme_name)
    return deduped


def _resolve_one_call(
    scheme_name: str,
    phase: str,
    scheme_store,
    host_dict: Dict[str, HostVarEntry],
    suite_vars: Dict[str, 'SuiteVar'],
    used_local_names: Set[str],
    suite_name: str = '',
    loop_context: Optional[List[Tuple[str, Optional[str]]]] = None,
) -> Optional[ResolvedCall]:
    """Build a ResolvedCall for one scheme/phase, or return None if not defined.

    *loop_context* is a list of ``(loop_count_expr, loop_std_name)`` tuples
    describing the enclosing ``<subcycle>`` blocks, outermost first.  Empty
    when the call is not inside any subcycle.  Forwarded to
    :func:`_resolve_one_arg` so scheme args declaring ``ccpp_loop_counter``
    or ``ccpp_loop_extent`` can resolve against the generated loop locals.
    """
    vars_list = scheme_store.variables_for(scheme_name, phase)
    if vars_list is None:
        return None
    const_stds = scheme_store.constituent_stdnames()
    resolved_call = ResolvedCall(
        scheme_name=scheme_name, phase=phase,
        scheme_module=scheme_store.module_for(scheme_name),
    )
    for scheme_var in vars_list:
        arg = _resolve_one_arg(
            scheme_var, phase, host_dict, suite_vars, scheme_name, used_local_names,
            suite_name=suite_name, loop_context=loop_context,
            const_stds=const_stds,
        )
        resolved_call.args.append(arg)
    return resolved_call


def _resolve_flat_phase(
    scheme_names: List[str],
    phase: str,
    scheme_store,
    host_dict: Dict[str, HostVarEntry],
    suite_vars: Dict[str, 'SuiteVar'],
    used_local_names: Set[str],
    suite_name: str = '',
) -> List[ResolvedCall]:
    """Resolve a flat (non-subcycle) phase into a list of ResolvedCall."""
    result: List[ResolvedCall] = []
    for scheme_name in scheme_names:
        resolved_call = _resolve_one_call(scheme_name, phase, scheme_store, host_dict,
                               suite_vars, used_local_names,
                               suite_name=suite_name)
        if resolved_call is not None:
            result.append(resolved_call)
    return result


def _resolve_run_phase(
    group,
    phase: str,
    scheme_store,
    host_dict: Dict[str, HostVarEntry],
    suite_vars: Dict[str, 'SuiteVar'],
    used_local_names: Set[str],
    suite_name: str = '',
) -> List[PhaseItem]:
    """Resolve the run phase, preserving subcycle do-loop structure.

    :class:`SuiteScheme` items become :class:`ResolvedCall`.
    :class:`SuiteSubcycle` items become :class:`ResolvedSubcycle`.  When
    a subcycle contains nested ``<subcycle>`` elements, they are
    preserved recursively so the cap emitter renders the corresponding
    nested ``do`` loops (matches original capgen behaviour).
    :class:`SuiteSubcol` items are flattened (treated as plain schemes).
    """
    from generator.suite_xml import SuiteScheme, SuiteSubcycle, SuiteSubcol

    def _resolve_items(
        suite_items,
        loop_context: List[Tuple[str, Optional[str]]],
    ) -> List[PhaseItem]:
        """Recursively turn a list of SuiteScheme/SuiteSubcycle/SuiteSubcol
        children into a list of :data:`PhaseItem`.  Used at the top level
        of a group AND for the body of every (possibly nested) subcycle.

        *loop_context* is the stack of enclosing ``<subcycle>`` blocks
        (outermost first), each as ``(loop_count_expr, loop_std_name)``.
        Empty at the top level of a group; one entry per nested
        subcycle depth.
        """
        out: List[PhaseItem] = []
        for sub in suite_items:
            if isinstance(sub, SuiteScheme):
                resolved_call = _resolve_one_call(
                    sub.name, phase, scheme_store, host_dict,
                    suite_vars, used_local_names,
                    suite_name=suite_name,
                    loop_context=loop_context,
                )
                if resolved_call is not None:
                    out.append(resolved_call)
            elif isinstance(sub, SuiteSubcycle):
                loop_count, loop_std = _resolve_subcycle_loop_bound(
                    sub.loop, host_dict, suite_vars=suite_vars,
                )
                inner = _resolve_items(
                    sub.items, loop_context + [(loop_count, loop_std)],
                )
                if inner:
                    out.append(ResolvedSubcycle(
                        loop=loop_count, calls=inner,
                        loop_std_name=loop_std,
                    ))
            elif isinstance(sub, SuiteSubcol):
                # SuiteSubcol is flattened in place — the framework
                # doesn't render it as a separate loop level.
                for scheme_name in sub.scheme_names():
                    resolved_call = _resolve_one_call(
                        scheme_name, phase, scheme_store, host_dict,
                        suite_vars, used_local_names,
                        suite_name=suite_name,
                        loop_context=loop_context,
                    )
                    if resolved_call is not None:
                        out.append(resolved_call)
        return out

    result: List[PhaseItem] = []

    for item in group.items:
        if isinstance(item, SuiteScheme):
            resolved_call = _resolve_one_call(item.name, phase, scheme_store, host_dict,
                                   suite_vars, used_local_names,
                                   suite_name=suite_name,
                                   loop_context=[])
            if resolved_call is not None:
                result.append(resolved_call)
        elif isinstance(item, SuiteSubcycle):
            loop_count, loop_std = _resolve_subcycle_loop_bound(
                item.loop, host_dict, suite_vars=suite_vars,
            )
            inner = _resolve_items(
                item.items, [(loop_count, loop_std)],
            )
            if inner:
                result.append(ResolvedSubcycle(
                    loop=loop_count, calls=inner,
                    loop_std_name=loop_std,
                ))
        elif isinstance(item, SuiteSubcol):
            for scheme_name in item.scheme_names():
                resolved_call = _resolve_one_call(scheme_name, phase, scheme_store, host_dict,
                                       suite_vars, used_local_names,
                                       suite_name=suite_name,
                                       loop_context=[])
                if resolved_call is not None:
                    result.append(resolved_call)

    return result


def _collect_dim_uses(
    resolved_group: ResolvedGroup,
    host_dict: Dict[str, HostVarEntry],
    suite_vars: Optional[Dict[str, 'SuiteVar']] = None,
) -> Dict[str, Set[str]]:
    """Collect dimension variable USE requirements across all phases of a group.

    Host-module dimensions resolve to ``{host_module: {local_name}}``.
    Suite-owned dimensions (set by ``_register``) resolve to
    ``{ccpp_<suite>_data: {ccpp_suite_data}}`` so the group cap can USE the
    suite data module to access ``ccpp_suite_data(inst)%<local>`` in dimension
    expressions.
    """
    dim_uses: Dict[str, Set[str]] = {}
    for items in resolved_group.phase_calls.values():
        for resolved_call in iter_phase_calls(items):
            for arg in resolved_call.args:
                for dim_std in arg.used_dim_std_names:
                    entry = host_dict.get(dim_std)
                    if entry is not None and entry.module_name is not None:
                        mod = entry.module_name
                        # Walk back to the access-path root so DDT-
                        # component dims (e.g. ``physics%Model%levs``)
                        # USE the top-level instance (``physics``) and
                        # not the leaf (``levs``, which doesn't exist
                        # as a module symbol).  Equivalent to
                        # ``entry.local_name`` for plain host vars.
                        sym = _root_symbol(entry.access_path)
                        dim_uses.setdefault(mod, set()).add(sym)
                    elif suite_vars and dim_std in suite_vars:
                        suite_var = suite_vars[dim_std]
                        dim_uses.setdefault(suite_var.module_name, set()).add(
                            'ccpp_suite_data')
        # Subcycle loop bounds resolved from CCPP standard names also need
        # a USE entry (or, for control vars, a dummy arg — handled elsewhere).
        # USE the *root* of the access path so DDT-component bounds pull
        # in the parent instance (e.g. ``use mod, only: phys_state``)
        # rather than the bare component name.  Walk *every* subcycle in
        # the phase, including nested ones — each level's bound must be
        # in scope at do-loop emission time.
        for item in iter_phase_subcycles(items):
            if not item.loop_std_name:
                continue
            entry = host_dict.get(item.loop_std_name)
            if entry is not None and entry.module_name is not None:
                dim_uses.setdefault(entry.module_name, set()).add(
                    _root_symbol(entry.access_path)
                )
            elif suite_vars and item.loop_std_name in suite_vars:
                suite_var = suite_vars[item.loop_std_name]
                dim_uses.setdefault(suite_var.module_name, set()).add(
                    'ccpp_suite_data'
                )
    return dim_uses
