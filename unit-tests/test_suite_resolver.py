"""Unit and integration tests for generator.suite_resolver and generator.group_cap.

Tests are organized as:
- Unit tests for helper functions (unit conversion, subscript builder, etc.)
- Unit tests for single-argument resolution (Cases 1-4)
- Integration tests: full suite resolution using sample files + suite XML
- Group cap output tests: check generated Fortran source lines
"""

import doctest
import os
import sys
import tempfile
import types
import unittest

from metadata.metadata_table import _parse_lines, parse_metadata_file
from metadata.parse_tools import CCPPError, ParseContext
from metadata.variable_resolver import build_flat_host_dict, SchemeStore

from generator.suite_resolver import (
    _normalize_unit_string,
    _unit_to_id,
    find_unit_conversion,
    _apply_transform_formula,
    _build_call_subscript,
    _build_merged_subscript,
    _one_dim_part,
    _resolve_single_bound,
    _substitute_instance_idx,
    _translate_active_expr,
    _root_symbol,
    _local_name_conflict,
    _resolve_one_arg,
    _dedup_scheme_names,
    resolve_suite,
    iter_phase_calls,
    validate_init_dimensions,
    SuiteVar,
    ResolvedArg,
    ResolvedCall,
    ResolvedGroup,
    ResolvedSubcycle,
    SuiteResolution,
)
from generator.group_cap import (
    _active_required_guard_lines,
    _fortran_type_str,
    _dim_decl,
    _dim_decl_local,
    _use_statements,
    _generate_group_cap,
    _collect_kinds_used,
    _transform_comment,
    write_group_cap,
)

# ---------------------------------------------------------------------------
_TESTS_DIR   = os.path.dirname(os.path.abspath(__file__))
_SAMPLES_DIR = os.path.join(_TESTS_DIR, 'sample_files')
_SUITE_DIR   = os.path.join(_TESTS_DIR, 'sample_suite_files')


def _sf(name):
    return os.path.join(_SAMPLES_DIR, name)


def _suite_file(name):
    return os.path.join(_SUITE_DIR, name)


def _ctx():
    return ParseContext(0, 'test.meta')


def _parse(src, fname='t.meta'):
    return _parse_lines(src.splitlines(keepends=True), fname)


def _load_full_host_dict():
    """Load the host_full + control_full metadata into a flat dict."""
    host_tbls = parse_metadata_file(_sf('host_full.meta'))
    ctrl_tbls = parse_metadata_file(_sf('control_full.meta'))
    return build_flat_host_dict(host_tbls, ctrl_tbls, [])


def _load_scheme_store():
    """Load temp_calc_adjust scheme."""
    tables = parse_metadata_file(_sf('scheme_multipart.meta'))
    return SchemeStore.build_from(tables)


def _parse_suite(name='suite_test_simple.xml'):
    """Parse a suite XML file and return the Suite object."""
    from generator.suite_xml import parse_suite_xml
    import logging
    logger = logging.getLogger('test')
    with tempfile.TemporaryDirectory() as tmpdir:
        return parse_suite_xml(_suite_file(name), tmpdir, logger,
                               skip_validation=True)


########################################################################
# Tests: _normalize_unit_string
########################################################################

class TestNormalizeUnitString(unittest.TestCase):

    def test_bare_positive_exponent_gets_plus(self):
        self.assertEqual(_normalize_unit_string('m2'),       'm+2')
        self.assertEqual(_normalize_unit_string('m2 s-2'),   'm+2 s-2')
        self.assertEqual(_normalize_unit_string('kg m2'),    'kg m+2')

    def test_existing_plus_unchanged(self):
        self.assertEqual(_normalize_unit_string('m+2'),      'm+2')
        self.assertEqual(_normalize_unit_string('m+2 s-2'),  'm+2 s-2')

    def test_negative_exponent_unchanged(self):
        self.assertEqual(_normalize_unit_string('m s-1'),    'm s-1')
        self.assertEqual(_normalize_unit_string('kg kg-1'),  'kg kg-1')
        self.assertEqual(_normalize_unit_string('kg m-3'),   'kg m-3')

    def test_no_exponent_unchanged(self):
        self.assertEqual(_normalize_unit_string('Pa'),       'Pa')
        self.assertEqual(_normalize_unit_string('kg m s-2'), 'kg m s-2')
        self.assertEqual(_normalize_unit_string(''),         '')

    def test_idempotent(self):
        once  = _normalize_unit_string('m2 s-2')
        twice = _normalize_unit_string(once)
        self.assertEqual(once, twice)


########################################################################
# Tests: _unit_to_id
########################################################################

class TestUnitToId(unittest.TestCase):

    def test_simple(self):
        self.assertEqual(_unit_to_id('Pa'), 'Pa')
        self.assertEqual(_unit_to_id('K'), 'K')

    def test_space_to_underscore(self):
        self.assertEqual(_unit_to_id('m s-1'), 'm_s_minus_1')
        self.assertEqual(_unit_to_id('kg kg-1'), 'kg_kg_minus_1')

    def test_positive_exponent(self):
        self.assertEqual(_unit_to_id('m2 s-2'), 'm_plus_2_s_minus_2')

    def test_explicit_plus(self):
        # hypothetical 'm+3'
        self.assertEqual(_unit_to_id('m+3'), 'm_plus_3')

    def test_no_change(self):
        self.assertEqual(_unit_to_id('radian'), 'radian')
        self.assertEqual(_unit_to_id('degree'), 'degree')


########################################################################
# Tests: find_unit_conversion
########################################################################

class TestFindUnitConversion(unittest.TestCase):

    def test_known_conversion(self):
        fn = find_unit_conversion('Pa', 'hPa')
        self.assertIsNotNone(fn)
        formula = fn()
        self.assertIn('{var}', formula)

    def test_same_unit_no_conversion(self):
        self.assertIsNone(find_unit_conversion('K', 'K'))
        self.assertIsNone(find_unit_conversion('Pa', 'Pa'))

    def test_equivalent_exponent_forms_no_conversion(self):
        # ``m2`` and ``m+2`` are equivalent; the resolver must not treat
        # them as a unit mismatch.  See _normalize_unit_string.
        self.assertIsNone(find_unit_conversion('m2 s-2', 'm+2 s-2'))
        self.assertIsNone(find_unit_conversion('m+2 s-2', 'm2 s-2'))
        self.assertIsNone(find_unit_conversion('m2', 'm+2'))

    def test_unknown_pair(self):
        self.assertIsNone(find_unit_conversion('XYZ', 'ABC'))

    def test_reverse_conversion(self):
        fwd = find_unit_conversion('Pa', 'hPa')
        bwd = find_unit_conversion('hPa', 'Pa')
        self.assertIsNotNone(fwd)
        self.assertIsNotNone(bwd)
        # Forward and backward are different formulae.
        self.assertNotEqual(fwd(), bwd())

    def test_m_s_conversion(self):
        fn = find_unit_conversion('m s-1', 'km h-1')
        self.assertIsNotNone(fn)


########################################################################
# Tests: _apply_transform_formula
########################################################################

class TestApplyTransformFormula(unittest.TestCase):

    def test_with_kind(self):
        from metadata.unit_conversion import Pa__to__hPa
        result = _apply_transform_formula(Pa__to__hPa, 'pressure', 'kind_phys')
        self.assertEqual(result, '1.0E-2_kind_phys*pressure')

    def test_without_kind(self):
        from metadata.unit_conversion import Pa__to__hPa
        result = _apply_transform_formula(Pa__to__hPa, 'pressure', '')
        self.assertEqual(result, '1.0E-2*pressure')

    def test_complex_expr(self):
        from metadata.unit_conversion import mm__to__m
        result = _apply_transform_formula(mm__to__m, 'arr(lb:ub, 1:nlev)', 'k')
        self.assertEqual(result, '1.0E-3_k*arr(lb:ub, 1:nlev)')


########################################################################
# Tests: _build_call_subscript
########################################################################

_HOST_DICT_SRC = '''
[ccpp-table-properties]
  name = hm
  type = host
[ccpp-arg-table]
  name = hm
  type = host
[ lb ]
  standard_name = horizontal_loop_begin
  units = count
  dimensions = ()
  type = integer
  protected = True
[ ub ]
  standard_name = horizontal_loop_end
  units = count
  dimensions = ()
  type = integer
  protected = True
[ ncols ]
  standard_name = horizontal_dimension
  units = count
  dimensions = ()
  type = integer
[ nlev ]
  standard_name = vertical_layer_dimension
  units = count
  dimensions = ()
  type = integer
[ nlevp1 ]
  standard_name = vertical_interface_dimension
  units = count
  dimensions = ()
  type = integer
'''


class TestBuildCallSubscript(unittest.TestCase):

    def _hd(self):
        tbls = _parse(_HOST_DICT_SRC)
        return build_flat_host_dict(tbls, [], [])

    def test_scalar(self):
        hd = self._hd()
        sub, used = _build_call_subscript([], 'run', hd)
        self.assertEqual(sub, '')
        self.assertEqual(used, set())

    def test_horizontal_run(self):
        hd = self._hd()
        sub, used = _build_call_subscript(['horizontal_dimension'], 'run', hd)
        self.assertEqual(sub, '(lb:ub)')
        self.assertIn('horizontal_loop_begin', used)
        self.assertIn('horizontal_loop_end', used)

    def test_horizontal_init(self):
        hd = self._hd()
        sub, used = _build_call_subscript(['horizontal_dimension'], 'init', hd)
        self.assertEqual(sub, '(lb:ub)')

    def test_vertical(self):
        hd = self._hd()
        sub, used = _build_call_subscript(['vertical_layer_dimension'], 'run', hd)
        self.assertEqual(sub, '(1:nlev)')
        self.assertIn('vertical_layer_dimension', used)

    def test_vertical_interface(self):
        hd = self._hd()
        sub, used = _build_call_subscript(['vertical_interface_dimension'], 'run', hd)
        self.assertEqual(sub, '(1:nlevp1)')

    def test_2d_array_run(self):
        hd = self._hd()
        sub, used = _build_call_subscript(
            ['horizontal_dimension', 'vertical_layer_dimension'], 'run', hd
        )
        self.assertEqual(sub, '(lb:ub, 1:nlev)')

    def test_2d_array_init(self):
        hd = self._hd()
        sub, used = _build_call_subscript(
            ['horizontal_dimension', 'vertical_layer_dimension'], 'init', hd
        )
        self.assertEqual(sub, '(lb:ub, 1:nlev)')

    def test_arbitrary_dim(self):
        src = _HOST_DICT_SRC + '''[ nspecies ]
  standard_name = number_of_species
  units = count
  dimensions = ()
  type = integer
'''
        hd = build_flat_host_dict(_parse(src), [], [])
        sub, used = _build_call_subscript(['number_of_species'], 'run', hd)
        self.assertEqual(sub, '(1:nspecies)')
        self.assertIn('number_of_species', used)

    def test_unknown_dim_raises(self):
        hd = self._hd()
        with self.assertRaises(CCPPError) as cm:
            _build_call_subscript(['unknown_dimension_xyz'], 'run', hd)
        self.assertIn('unknown_dimension_xyz', str(cm.exception))

    def test_unknown_dim_lists_available_std_names(self):
        """When a dim lookup fails, the error message must enumerate
        every standard name the resolver can see — sorted, with source
        annotation — so the user can spot typos / case mismatches /
        missing declarations at a glance."""
        hd = self._hd()
        with self.assertRaises(CCPPError) as cm:
            _build_call_subscript(['unknown_dimension_xyz'], 'run', hd)
        msg = str(cm.exception)
        self.assertIn('Available standard names', msg)
        # The host_dict from _HOST_DICT_SRC contains horizontal_dimension
        # at minimum — must be listed with a source tag.
        self.assertIn('horizontal_dimension', msg)
        self.assertIn('[host:', msg)

    def test_unknown_dim_did_you_mean_for_close_match(self):
        """A near-miss spelling (case difference) surfaces a 'did you
        mean' section above the full listing."""
        # Host has 'horizontal_dimension'; query with mixed case to
        # trigger close-match detection.
        hd = self._hd()
        with self.assertRaises(CCPPError) as cm:
            _build_call_subscript(['Horizontal_Dimension'], 'run', hd)
        msg = str(cm.exception)
        self.assertIn('Did you mean', msg)
        self.assertIn('horizontal_dimension', msg)

    def test_instance_dim_without_instance_number_raises(self):
        """Host metadata referencing an instance dim must declare
        ``instance_number``; otherwise the resolver should error with a
        clear, actionable message rather than silently mis-resolving.
        """
        # Host dict lacks both instance_number AND number_of_instances.
        # Asking the resolver to subscript a (number_of_instances) dim
        # must raise CCPPError.
        hd = self._hd()
        with self.assertRaises(CCPPError) as cm:
            _build_call_subscript(['number_of_instances'], 'run', hd)
        msg = str(cm.exception)
        self.assertIn('instance_number', msg)
        self.assertIn('number_of_instances', msg)

    def test_missing_horiz_bounds_raises(self):
        """Missing horizontal_loop_begin/end in host dict → CCPPError."""
        src = '''
[ccpp-table-properties]
  name = hm2
  type = host
[ccpp-arg-table]
  name = hm2
  type = host
[ ncols ]
  standard_name = horizontal_dimension
  units = count
  dimensions = ()
  type = integer
'''
        hd = build_flat_host_dict(_parse(src), [], [])
        with self.assertRaises(CCPPError):
            _build_call_subscript(['horizontal_dimension'], 'run', hd)

    def test_horiz_range_ccpp_constant_one(self):
        """ccpp_constant_one:horizontal_dimension resolves to lb:ub."""
        hd = self._hd()
        sub, used = _build_call_subscript(
            ['ccpp_constant_one:horizontal_dimension'], 'run', hd
        )
        self.assertEqual(sub, '(lb:ub)')
        self.assertIn('horizontal_loop_begin', used)
        self.assertIn('horizontal_loop_end', used)
        self.assertIn('horizontal_dimension', used)

    def test_horiz_range_integer_one(self):
        """1:horizontal_dimension resolves to lb:ub."""
        hd = self._hd()
        sub, used = _build_call_subscript(
            ['1:horizontal_dimension'], 'run', hd
        )
        self.assertEqual(sub, '(lb:ub)')
        self.assertIn('horizontal_loop_begin', used)
        self.assertIn('horizontal_loop_end', used)

    def test_horiz_range_bad_lower_raises(self):
        """A lower bound other than 1/ccpp_constant_one for horizontal_dimension raises CCPPError."""
        hd = self._hd()
        with self.assertRaises(CCPPError) as cm:
            _build_call_subscript(['2:horizontal_dimension'], 'run', hd)
        self.assertIn('horizontal_dimension', str(cm.exception))
        self.assertIn('ccpp_constant_one', str(cm.exception))

    def test_horiz_range_named_lower_raises(self):
        """A named lower bound for horizontal_dimension (not resolving to 1) raises CCPPError."""
        hd = self._hd()
        with self.assertRaises(CCPPError) as cm:
            _build_call_subscript(['vertical_layer_dimension:horizontal_dimension'], 'run', hd)
        self.assertIn('horizontal_dimension', str(cm.exception))

    def test_vertical_explicit_range(self):
        """General lower:upper range resolved via host_dict for vertical dims."""
        src = _HOST_DICT_SRC + '''[ bot ]
  standard_name = bottom_vertical_interface_index
  units = count
  dimensions = ()
  type = integer
'''
        hd = build_flat_host_dict(_parse(src), [], [])
        sub, used = _build_call_subscript(
            ['bottom_vertical_interface_index:vertical_interface_dimension'], 'run', hd
        )
        self.assertEqual(sub, '(bot:nlevp1)')
        self.assertIn('bottom_vertical_interface_index', used)
        self.assertIn('vertical_interface_dimension', used)

    def test_ccpp_constant_one_as_lower_vertical(self):
        """ccpp_constant_one:vertical_layer_dimension is equivalent to vertical_layer_dimension."""
        hd = self._hd()
        sub, _ = _build_call_subscript(
            ['ccpp_constant_one:vertical_layer_dimension'], 'run', hd
        )
        self.assertEqual(sub, '(1:nlev)')


########################################################################
# Tests: _build_merged_subscript (sliced local_name with std-name indices)
########################################################################

_HOST_SLICE_SRC = _HOST_DICT_SRC + '''[ index_qv ]
  standard_name = index_of_water_vapor_specific_HUMidity
  units = index
  dimensions = ()
  type = integer
  protected = True
'''


class TestResolveSingleBoundSubstitutesScalarIdx(unittest.TestCase):
    """``_resolve_single_bound`` returns the host entry's access path
    for a DDT-component dim bound.  The access path may carry baked-in
    registered scalar-index placeholders (``(instance_number)``,
    ``(thread_number)``) — those MUST be resolved to the host's local
    Fortran names before the bound is spliced into a generated cap
    subscript.  Otherwise the cap leaks the std-name placeholder
    verbatim and the Fortran compiler rejects it as "no IMPLICIT
    type".  Regression for the SCM phys_ps cap bug where
    ``physics%Interstitial(thread_number)%nvdiff`` appeared inside the
    ``vdftra`` slice expression."""

    def _build_dict(self):
        from metadata.metadata_table import _parse_lines
        from metadata.variable_resolver import build_flat_host_dict
        ddt_src = (
            "[ccpp-table-properties]\n  name = GFS_interstitial_type\n  type = ddt\n"
            "[ccpp-arg-table]\n  name = GFS_interstitial_type\n  type = ddt\n"
            "[ nvdiff ]\n  standard_name = number_of_vertical_diffusion_tracers\n"
            "  units = count\n  dimensions = ()\n  type = integer\n"
            "\n"
            "[ccpp-table-properties]\n  name = scm_phys_type\n  type = ddt\n"
            "[ccpp-arg-table]\n  name = scm_phys_type\n  type = ddt\n"
            "[ Interstitial ]\n  standard_name = GFS_interstitial_type_instance\n"
            "  units = DDT\n  dimensions = (number_of_threads)\n"
            "  type = GFS_interstitial_type\n"
        )
        host_src = (
            "[ccpp-table-properties]\n  name = scm_type_defs\n  type = host\n"
            "[ccpp-arg-table]\n  name = scm_type_defs\n  type = host\n"
            "[ physics ]\n  standard_name = scm_physics_type_instance\n"
            "  units = DDT\n  dimensions = ()\n  type = scm_phys_type\n"
        )
        ctrl_src = (
            "[ccpp-table-properties]\n  name = ctrl_mod\n  type = control\n"
            "[ccpp-arg-table]\n  name = ctrl_mod\n  type = control\n"
            "[ mythread ]\n  standard_name = thread_number\n  units = index\n"
            "  dimensions = ()\n  type = integer\n"
        )
        return build_flat_host_dict(
            _parse_lines(host_src.splitlines(keepends=True), 'host.meta'),
            _parse_lines(ctrl_src.splitlines(keepends=True), 'ctrl.meta'),
            _parse_lines(ddt_src.splitlines(keepends=True), 'ddt.meta'),
        )

    def test_thread_number_placeholder_substituted_to_host_local(self):
        hd = self._build_dict()
        # Pre-condition: the baked access path carries the std-name
        # placeholder (``thread_number``), not yet ``mythread``.
        self.assertEqual(
            hd['number_of_vertical_diffusion_tracers'].access_path,
            'physics%Interstitial(thread_number)%nvdiff',
        )
        used = set()
        resolved = _resolve_single_bound(
            'number_of_vertical_diffusion_tracers', hd, used,
        )
        # The substitution must fire: ``thread_number`` → host local
        # ``mythread``.  Without it the cap leaks the std-name placeholder.
        self.assertEqual(
            resolved, 'physics%Interstitial(mythread)%nvdiff',
        )
        self.assertNotIn('thread_number', resolved)
        # The bound std name is recorded in *used* for USE-list tracking.
        self.assertIn('number_of_vertical_diffusion_tracers', used)


class TestBuildMergedSubscript(unittest.TestCase):
    """Cover the slicing-with-standard-name-index code path.

    A host local_name like ``q(:,:,index_of_water_vapor_specific_HUMidity)``
    parses into ``local_subscript=[':', ':', 'index_of_water_vapor_specific_HUMidity']``.
    The mixed-case token is the CCPP standard name of the index variable; the
    cap must emit the host's local name (``index_qv``) and import it.
    """

    def _hd(self):
        return build_flat_host_dict(_parse(_HOST_SLICE_SRC), [], [])

    def test_resolves_mixed_case_standard_name(self):
        hd = self._hd()
        sub, used = _build_merged_subscript(
            ['horizontal_dimension', 'vertical_layer_dimension'],
            [':', ':', 'index_of_water_vapor_specific_HUMidity'],
            'run', hd,
        )
        self.assertEqual(sub, '(lb:ub, 1:nlev, index_qv)')
        # Lowercased standard name is reported as used so the cap emits a
        # `use test_host_mod, only: index_qv` line.
        self.assertIn('index_of_water_vapor_specific_humidity', used)

    def test_integer_literal_passthrough(self):
        hd = self._hd()
        sub, used = _build_merged_subscript(
            ['horizontal_dimension', 'vertical_layer_dimension'],
            [':', ':', '1'],
            'run', hd,
        )
        self.assertEqual(sub, '(lb:ub, 1:nlev, 1)')

    def test_unknown_index_raises(self):
        hd = self._hd()
        with self.assertRaises(CCPPError):
            _build_merged_subscript(
                ['horizontal_dimension', 'vertical_layer_dimension'],
                [':', ':', 'no_such_standard_name'],
                'run', hd,
            )

    def test_explicit_index_substitutes_scalar_idx_placeholder(self):
        """Regression: an explicit subscript token like
        ``index_of_water_vapor_specific_humidity`` whose ``access_path``
        carries a baked ``(instance_number)`` DDT-instance placeholder
        must have that placeholder resolved to the host's local name
        before being spliced into the subscript.  Without the
        substitution the generator emits Fortran like
        ``qgrs(lb:ub, 1:nlev, GFS_Control(instance_number)%ntqv)`` and
        the compiler rejects ``instance_number`` as untyped.  Found
        2026-05-15 in the NEPTUNE phys_ps cap.
        """
        from metadata.metadata_table import _parse_lines
        ddt_src = (
            "[ccpp-table-properties]\n  name = GFS_control_type\n  type = ddt\n"
            "[ccpp-arg-table]\n  name = GFS_control_type\n  type = ddt\n"
            "[ ntqv ]\n"
            "  standard_name = index_of_water_vapor_specific_humidity\n"
            "  units = index\n  dimensions = ()\n  type = integer\n"
        )
        host_src = (
            "[ccpp-table-properties]\n  name = scm_type_defs\n  type = host\n"
            "[ccpp-arg-table]\n  name = scm_type_defs\n  type = host\n"
            "[ GFS_Control ]\n  standard_name = GFS_control_type_instance\n"
            "  units = DDT\n  dimensions = (number_of_instances)\n"
            "  type = GFS_control_type\n"
            "[ ncols ]\n  standard_name = horizontal_dimension\n"
            "  units = count\n  dimensions = ()\n  type = integer\n"
            "[ nlev ]\n  standard_name = vertical_layer_dimension\n"
            "  units = count\n  dimensions = ()\n  type = integer\n"
        )
        ctrl_src = (
            "[ccpp-table-properties]\n  name = ctrl_mod\n  type = control\n"
            "[ccpp-arg-table]\n  name = ctrl_mod\n  type = control\n"
            "[ lb ]\n  standard_name = horizontal_loop_begin\n  units = index\n"
            "  dimensions = ()\n  type = integer\n"
            "[ ub ]\n  standard_name = horizontal_loop_end\n  units = index\n"
            "  dimensions = ()\n  type = integer\n"
            "[ instance ]\n  standard_name = instance_number\n  units = index\n"
            "  dimensions = ()\n  type = integer\n"
            "[ ninstances ]\n  standard_name = number_of_instances\n"
            "  units = count\n  dimensions = ()\n  type = integer\n"
        )
        hd = build_flat_host_dict(
            _parse_lines(host_src.splitlines(keepends=True), 'host.meta'),
            _parse_lines(ctrl_src.splitlines(keepends=True), 'ctrl.meta'),
            _parse_lines(ddt_src.splitlines(keepends=True), 'ddt.meta'),
        )
        # Pre-condition: the inner ntqv entry's access path carries the
        # baked ``(instance_number)`` placeholder.
        self.assertEqual(
            hd['index_of_water_vapor_specific_humidity'].access_path,
            'GFS_Control(instance_number)%ntqv',
        )
        sub, _used = _build_merged_subscript(
            ['horizontal_dimension', 'vertical_layer_dimension'],
            [':', ':', 'index_of_water_vapor_specific_humidity'],
            'run', hd,
        )
        # The emitted subscript must substitute the placeholder to the
        # host's local name (``instance``).
        self.assertEqual(
            sub, '(lb:ub, 1:nlev, GFS_Control(instance)%ntqv)',
        )
        self.assertNotIn('instance_number', sub)

    def test_explicit_index_nested_ddt_two_placeholder_levels(self):
        """Recursive variant: the index token's access_path crosses TWO
        DDT levels, each with its own registered scalar-index dim.  The
        baked path contains two distinct placeholders
        (``(instance_number)`` outer + ``(thread_number)`` inner) plus
        a third occurrence of one of them; ``_substitute_scalar_idx``
        must rewrite all of them in a single pass."""
        from metadata.metadata_table import _parse_lines
        ddt_src = (
            # Innermost DDT — defines the leaf index variable.
            "[ccpp-table-properties]\n  name = scratch_type\n  type = ddt\n"
            "[ccpp-arg-table]\n  name = scratch_type\n  type = ddt\n"
            "[ idx_qv ]\n"
            "  standard_name = index_of_water_vapor_specific_humidity\n"
            "  units = index\n  dimensions = ()\n  type = integer\n"
            "\n"
            # Middle DDT — sliced per OpenMP thread.
            "[ccpp-table-properties]\n  name = GFS_interstitial_type\n"
            "  type = ddt\n"
            "[ccpp-arg-table]\n  name = GFS_interstitial_type\n"
            "  type = ddt\n"
            "[ scratch ]\n"
            "  standard_name = scratch_type_instance\n  units = DDT\n"
            "  dimensions = ()\n  type = scratch_type\n"
            "\n"
            # Outer DDT — sliced per model instance and itself carrying
            # a per-thread Interstitial sub-DDT.
            "[ccpp-table-properties]\n  name = GFS_phys_type\n  type = ddt\n"
            "[ccpp-arg-table]\n  name = GFS_phys_type\n  type = ddt\n"
            "[ Interstitial ]\n"
            "  standard_name = GFS_interstitial_type_instance\n"
            "  units = DDT\n  dimensions = (number_of_threads)\n"
            "  type = GFS_interstitial_type\n"
        )
        host_src = (
            "[ccpp-table-properties]\n  name = scm_type_defs\n  type = host\n"
            "[ccpp-arg-table]\n  name = scm_type_defs\n  type = host\n"
            "[ GFS_Phys ]\n  standard_name = GFS_phys_type_instance\n"
            "  units = DDT\n  dimensions = (number_of_instances)\n"
            "  type = GFS_phys_type\n"
            "[ ncols ]\n  standard_name = horizontal_dimension\n"
            "  units = count\n  dimensions = ()\n  type = integer\n"
            "[ nlev ]\n  standard_name = vertical_layer_dimension\n"
            "  units = count\n  dimensions = ()\n  type = integer\n"
        )
        ctrl_src = (
            "[ccpp-table-properties]\n  name = ctrl_mod\n  type = control\n"
            "[ccpp-arg-table]\n  name = ctrl_mod\n  type = control\n"
            "[ lb ]\n  standard_name = horizontal_loop_begin\n  units = index\n"
            "  dimensions = ()\n  type = integer\n"
            "[ ub ]\n  standard_name = horizontal_loop_end\n  units = index\n"
            "  dimensions = ()\n  type = integer\n"
            "[ mythread ]\n  standard_name = thread_number\n  units = index\n"
            "  dimensions = ()\n  type = integer\n"
            "[ nthreads ]\n  standard_name = number_of_threads\n"
            "  units = count\n  dimensions = ()\n  type = integer\n"
            "[ instance ]\n  standard_name = instance_number\n  units = index\n"
            "  dimensions = ()\n  type = integer\n"
            "[ ninstances ]\n  standard_name = number_of_instances\n"
            "  units = count\n  dimensions = ()\n  type = integer\n"
        )
        hd = build_flat_host_dict(
            _parse_lines(host_src.splitlines(keepends=True), 'host.meta'),
            _parse_lines(ctrl_src.splitlines(keepends=True), 'ctrl.meta'),
            _parse_lines(ddt_src.splitlines(keepends=True), 'ddt.meta'),
        )
        # Pre-condition: the leaf entry's access path carries BOTH
        # registered-scalar-index placeholders, one per DDT level.
        self.assertEqual(
            hd['index_of_water_vapor_specific_humidity'].access_path,
            'GFS_Phys(instance_number)%Interstitial(thread_number)%scratch%idx_qv',
        )
        sub, _used = _build_merged_subscript(
            ['horizontal_dimension', 'vertical_layer_dimension'],
            [':', ':', 'index_of_water_vapor_specific_humidity'],
            'run', hd,
        )
        # All placeholders must be resolved in one pass.
        self.assertEqual(
            sub,
            '(lb:ub, 1:nlev, '
            'GFS_Phys(instance)%Interstitial(mythread)%scratch%idx_qv)',
        )
        self.assertNotIn('instance_number', sub)
        self.assertNotIn('thread_number',   sub)

    def test_explicit_index_with_literal_local_subscript(self):
        """Regression 2026-05-15: a subscript-token entry whose declared
        local_name carries a literal subscript (e.g. ``nstf_name(1)``)
        must render the full ``<access>(1)`` form, not bare ``<access>``.
        Companion to the active-expression bug for the same root cause.
        """
        from metadata.metadata_table import _parse_lines
        ddt_src = (
            "[ccpp-table-properties]\n  name = GFS_control_type\n  type = ddt\n"
            "[ccpp-arg-table]\n  name = GFS_control_type\n  type = ddt\n"
            "[ nstf_name(1) ]\n"
            "  standard_name = control_for_nsstm\n"
            "  units = flag\n  dimensions = ()\n  type = integer\n"
        )
        host_src = (
            "[ccpp-table-properties]\n  name = scm_type_defs\n  type = host\n"
            "[ccpp-arg-table]\n  name = scm_type_defs\n  type = host\n"
            "[ GFS_Control ]\n  standard_name = GFS_control_type_instance\n"
            "  units = DDT\n  dimensions = (number_of_instances)\n"
            "  type = GFS_control_type\n"
            "[ ncols ]\n  standard_name = horizontal_dimension\n"
            "  units = count\n  dimensions = ()\n  type = integer\n"
        )
        ctrl_src = (
            "[ccpp-table-properties]\n  name = ctrl_mod\n  type = control\n"
            "[ccpp-arg-table]\n  name = ctrl_mod\n  type = control\n"
            "[ lb ]\n  standard_name = horizontal_loop_begin\n  units = index\n"
            "  dimensions = ()\n  type = integer\n"
            "[ ub ]\n  standard_name = horizontal_loop_end\n  units = index\n"
            "  dimensions = ()\n  type = integer\n"
            "[ instance ]\n  standard_name = instance_number\n  units = index\n"
            "  dimensions = ()\n  type = integer\n"
            "[ ninstances ]\n  standard_name = number_of_instances\n"
            "  units = count\n  dimensions = ()\n  type = integer\n"
        )
        hd = build_flat_host_dict(
            _parse_lines(host_src.splitlines(keepends=True), 'host.meta'),
            _parse_lines(ctrl_src.splitlines(keepends=True), 'ctrl.meta'),
            _parse_lines(ddt_src.splitlines(keepends=True), 'ddt.meta'),
        )
        sub, _used = _build_merged_subscript(
            ['horizontal_dimension'],
            [':', 'control_for_nsstm'],
            'run', hd,
        )
        self.assertEqual(
            sub, '(lb:ub, GFS_Control(instance)%nstf_name(1))',
        )

    def test_multiple_explicit_index_tokens_each_with_placeholder(self):
        """Two distinct scheme-arg subscript tokens, each resolving to a
        DDT-walked access path with its own ``(instance_number)``
        placeholder.  Both must be substituted independently."""
        from metadata.metadata_table import _parse_lines
        ddt_src = (
            "[ccpp-table-properties]\n  name = GFS_control_type\n  type = ddt\n"
            "[ccpp-arg-table]\n  name = GFS_control_type\n  type = ddt\n"
            "[ ntqv ]\n"
            "  standard_name = index_of_water_vapor_specific_humidity\n"
            "  units = index\n  dimensions = ()\n  type = integer\n"
            "[ ntcw ]\n"
            "  standard_name = index_of_cloud_liquid_water_mixing_ratio\n"
            "  units = index\n  dimensions = ()\n  type = integer\n"
        )
        host_src = (
            "[ccpp-table-properties]\n  name = scm_type_defs\n  type = host\n"
            "[ccpp-arg-table]\n  name = scm_type_defs\n  type = host\n"
            "[ GFS_Control ]\n  standard_name = GFS_control_type_instance\n"
            "  units = DDT\n  dimensions = (number_of_instances)\n"
            "  type = GFS_control_type\n"
            "[ ncols ]\n  standard_name = horizontal_dimension\n"
            "  units = count\n  dimensions = ()\n  type = integer\n"
            "[ nlev ]\n  standard_name = vertical_layer_dimension\n"
            "  units = count\n  dimensions = ()\n  type = integer\n"
        )
        ctrl_src = (
            "[ccpp-table-properties]\n  name = ctrl_mod\n  type = control\n"
            "[ccpp-arg-table]\n  name = ctrl_mod\n  type = control\n"
            "[ lb ]\n  standard_name = horizontal_loop_begin\n  units = index\n"
            "  dimensions = ()\n  type = integer\n"
            "[ ub ]\n  standard_name = horizontal_loop_end\n  units = index\n"
            "  dimensions = ()\n  type = integer\n"
            "[ instance ]\n  standard_name = instance_number\n  units = index\n"
            "  dimensions = ()\n  type = integer\n"
            "[ ninstances ]\n  standard_name = number_of_instances\n"
            "  units = count\n  dimensions = ()\n  type = integer\n"
        )
        hd = build_flat_host_dict(
            _parse_lines(host_src.splitlines(keepends=True), 'host.meta'),
            _parse_lines(ctrl_src.splitlines(keepends=True), 'ctrl.meta'),
            _parse_lines(ddt_src.splitlines(keepends=True), 'ddt.meta'),
        )
        # Both index entries should carry the (instance_number) placeholder.
        self.assertEqual(
            hd['index_of_water_vapor_specific_humidity'].access_path,
            'GFS_Control(instance_number)%ntqv',
        )
        self.assertEqual(
            hd['index_of_cloud_liquid_water_mixing_ratio'].access_path,
            'GFS_Control(instance_number)%ntcw',
        )
        # A subscript with TWO explicit index tokens — both placeholders
        # must be rewritten.
        sub, _used = _build_merged_subscript(
            ['horizontal_dimension', 'vertical_layer_dimension'],
            [':', ':', 'index_of_water_vapor_specific_humidity',
             'index_of_cloud_liquid_water_mixing_ratio'],
            'run', hd,
        )
        self.assertEqual(
            sub,
            '(lb:ub, 1:nlev, '
            'GFS_Control(instance)%ntqv, GFS_Control(instance)%ntcw)',
        )
        self.assertNotIn('instance_number', sub)


########################################################################
# Tests: _translate_active_expr
########################################################################

class TestTranslateActiveExpr(unittest.TestCase):

    def _hd(self):
        src = '''
[ccpp-table-properties]
  name = m
  type = host
[ccpp-arg-table]
  name = m
  type = host
[ do_something ]
  standard_name = flag_for_something
  units = flag
  dimensions = ()
  type = logical
'''
        return build_flat_host_dict(_parse(src), [], [])

    def test_empty(self):
        hd = self._hd()
        self.assertEqual(_translate_active_expr('', hd), '')

    def test_simple_replacement(self):
        hd = self._hd()
        result = _translate_active_expr('flag_for_something', hd)
        self.assertEqual(result, 'do_something')

    def test_in_expression(self):
        hd = self._hd()
        result = _translate_active_expr('.not. flag_for_something', hd)
        self.assertEqual(result, '.not. do_something')

    def test_unknown_preserved(self):
        hd = self._hd()
        result = _translate_active_expr('unknown_stdname .eqv. .true.', hd)
        self.assertEqual(result, 'unknown_stdname .eqv. .true.')

    def test_ddt_component_flag_uses_full_access_path(self):
        """A flag declared as a DDT-component must translate to the full
        access path, not the bare component name — otherwise the
        generated cap references an undefined symbol.
        """
        ddt_src = '''
[ccpp-table-properties]
  name = inst_type
  type = ddt
[ccpp-arg-table]
  name = inst_type
  type = ddt
[opt_array_flag]
  standard_name = flag_for_opt_array
  units = 1
  dimensions = ()
  type = logical
'''
        host_src = '''
[ccpp-table-properties]
  name = data_mod
  type = host
[ccpp-arg-table]
  name = data_mod
  type = host
[ncols]
  standard_name = horizontal_dimension
  units = count
  dimensions = ()
  type = integer
[ninstances]
  standard_name = number_of_instances
  units = count
  dimensions = ()
  type = integer
[instance_data]
  standard_name = instance_data
  units = ddt
  dimensions = (number_of_instances)
  type = inst_type
'''
        hd = build_flat_host_dict(_parse(host_src), [], _parse(ddt_src))
        result = _translate_active_expr('(flag_for_opt_array)', hd)
        # No instance_number declared in this fixture → falls back to (1).
        self.assertEqual(result, '(instance_data(1)%opt_array_flag)')

    def test_literal_subscript_in_local_name_preserved(self):
        """Regression 2026-05-15: a DDT-component variable declared with a
        literal subscript in its local_name (e.g. ``nstf_name(1)``) must
        translate to ``<access_path>(1)`` in an active expression — the
        ``(1)`` carries semantic information (selects element 1 of an
        integer array) and must not be dropped.  Found in NEPTUNE
        GFS_Statein metadata where ``tref`` had
        ``active = (control_for_nsstm > 0)`` and ``control_for_nsstm``
        was declared as ``local_name = nstf_name(1)`` on GFS_Control;
        the cap emitted ``GFS_Control(instance)%nstf_name > 0`` (rank
        mismatch) instead of ``GFS_Control(instance)%nstf_name(1) > 0``.
        """
        from metadata.metadata_table import _parse_lines
        ddt_src = (
            "[ccpp-table-properties]\n  name = GFS_control_type\n  type = ddt\n"
            "[ccpp-arg-table]\n  name = GFS_control_type\n  type = ddt\n"
            "[ nstf_name(1) ]\n"
            "  standard_name = control_for_nsstm\n"
            "  units = flag\n  dimensions = ()\n  type = integer\n"
        )
        host_src = (
            "[ccpp-table-properties]\n  name = scm_type_defs\n  type = host\n"
            "[ccpp-arg-table]\n  name = scm_type_defs\n  type = host\n"
            "[ GFS_Control ]\n  standard_name = GFS_control_type_instance\n"
            "  units = DDT\n  dimensions = (number_of_instances)\n"
            "  type = GFS_control_type\n"
        )
        ctrl_src = (
            "[ccpp-table-properties]\n  name = ctrl_mod\n  type = control\n"
            "[ccpp-arg-table]\n  name = ctrl_mod\n  type = control\n"
            "[ instance ]\n  standard_name = instance_number\n  units = index\n"
            "  dimensions = ()\n  type = integer\n"
            "[ ninstances ]\n  standard_name = number_of_instances\n"
            "  units = count\n  dimensions = ()\n  type = integer\n"
        )
        hd = build_flat_host_dict(
            _parse_lines(host_src.splitlines(keepends=True), 'host.meta'),
            _parse_lines(ctrl_src.splitlines(keepends=True), 'ctrl.meta'),
            _parse_lines(ddt_src.splitlines(keepends=True), 'ddt.meta'),
        )
        # Pre-conditions: access_path strips the literal subscript;
        # local_subscript captures it for re-attachment at render time.
        self.assertEqual(
            hd['control_for_nsstm'].access_path,
            'GFS_Control(instance_number)%nstf_name',
        )
        self.assertEqual(hd['control_for_nsstm'].local_subscript, ['1'])
        # Active expression must emit the full ``(1)`` subscript.
        result = _translate_active_expr('(control_for_nsstm > 0)', hd)
        self.assertEqual(result, '(GFS_Control(instance)%nstf_name(1) > 0)')


########################################################################
# Tests: _substitute_instance_idx
########################################################################

class TestSubstituteInstanceIdx(unittest.TestCase):

    def _hd_with_inst(self, inst_local: str = 'inst'):
        src = '''
[ccpp-table-properties]
  name = ctrl
  type = control
[ccpp-arg-table]
  name = ctrl
  type = control
[ {inst} ]
  standard_name = instance_number
  units = 1
  dimensions = ()
  type = integer
'''.format(inst=inst_local)
        return build_flat_host_dict([], _parse(src), [])

    def _hd_without_inst(self):
        # An empty host_dict — no instance_number, no number_of_instances.
        return {}

    def test_no_template_pass_through(self):
        hd = self._hd_with_inst()
        self.assertEqual(
            _substitute_instance_idx('ncols', hd), 'ncols',
        )
        self.assertEqual(
            _substitute_instance_idx('gfs%phii(lb:ub)', hd),
            'gfs%phii(lb:ub)',
        )

    def test_template_resolved_to_local_name(self):
        hd = self._hd_with_inst(inst_local='inst')
        self.assertEqual(
            _substitute_instance_idx(
                'instance_data(instance_number)%data_array2', hd,
            ),
            'instance_data(inst)%data_array2',
        )

    def test_template_uses_local_name_alias(self):
        hd = self._hd_with_inst(inst_local='my_inst')
        self.assertEqual(
            _substitute_instance_idx(
                'instance_data(instance_number)', hd,
            ),
            'instance_data(my_inst)',
        )

    def test_template_resolves_to_one_when_no_pair(self):
        # Host did not declare instance_number; single-instance API,
        # internal arrays sized to 1.
        hd = self._hd_without_inst()
        self.assertEqual(
            _substitute_instance_idx(
                'instance_data(instance_number)%data_array2', hd,
            ),
            'instance_data(1)%data_array2',
        )


########################################################################
# Tests: _root_symbol
########################################################################

class TestRootSymbol(unittest.TestCase):

    def test_plain(self):
        self.assertEqual(_root_symbol('ncols'), 'ncols')

    def test_component(self):
        self.assertEqual(_root_symbol('gfs_statein%phii'), 'gfs_statein')

    def test_subscript(self):
        self.assertEqual(_root_symbol('gfs_statein(instance_number)%phii'), 'gfs_statein')

    def test_deep(self):
        self.assertEqual(_root_symbol('outer%middle%inner'), 'outer')


########################################################################
# Tests: _local_name_conflict
########################################################################

class TestLocalNameConflict(unittest.TestCase):

    def test_no_conflict(self):
        self.assertEqual(_local_name_conflict('phii_l', set()), 'phii_l')

    def test_conflict_adds_2(self):
        self.assertEqual(_local_name_conflict('phii_l', {'phii_l'}), 'phii_2_l')

    def test_conflict_adds_3(self):
        self.assertEqual(
            _local_name_conflict('phii_l', {'phii_l', 'phii_2_l'}), 'phii_3_l'
        )

    def test_case_insensitive_conflict(self):
        """Fortran identifiers are case-insensitive: ``CP_l`` must be
        treated as already-used when ``cp_l`` is in the existing set
        (and vice versa).  Regression: a HAFS_v0_hwrf_phys_ts cap
        emitted both ``cp_l`` and ``CP_l`` side-by-side because the
        check was string-equal rather than case-insensitive."""
        # Lower-then-upper.
        self.assertEqual(
            _local_name_conflict('CP_l', {'cp_l'}), 'CP_2_l',
        )
        # Upper-then-lower.
        self.assertEqual(
            _local_name_conflict('cp_l', {'cp_l'}), 'cp_2_l',
        )
        # Mixed-case existing entry too.
        self.assertEqual(
            _local_name_conflict('cp_l', {'Cp_L'.lower()}), 'cp_2_l',
        )


########################################################################
# Tests: _resolve_one_arg (single argument)
########################################################################

class TestResolveOneArg(unittest.TestCase):

    def _host_dict(self):
        return _load_full_host_dict()

    def _scheme_var(self, local, std_name, intent='in', units='1',
                    dims='()', type_='integer', kind='', optional=False):
        from metadata.metadata_table import MetaVar
        ctx = _ctx()
        v = MetaVar(local, ctx)
        v.set_attr('standard_name', std_name, ctx)
        v.set_attr('units', units, ctx)
        v.set_attr('dimensions', dims, ctx)
        v.set_attr('type', type_, ctx)
        v.set_attr('intent', intent, ctx)
        if kind:
            v.set_attr('kind', kind, ctx)
        if optional:
            v.set_attr('optional', 'True', ctx)
        return v

    def test_case1_direct_host(self):
        """Case 1: scalar host variable, no transform."""
        hd = self._host_dict()
        suite_var = self._scheme_var(
            'im', 'horizontal_dimension', 'in', 'count')
        arg = _resolve_one_arg(suite_var, 'run', hd, {}, 'my_scheme', set())
        self.assertEqual(arg.source, 'host')
        self.assertEqual(arg.transform_case, 1)
        # Scalar horizontal_dimension in run phase is synthesised from
        # the chunk loop bounds (ub - lb + 1) rather than the host's
        # full-domain ncols — see ``_HORIZ_DIM_STD`` in suite_resolver.
        self.assertEqual(arg.call_expr, '(ub - lb + 1)')
        self.assertFalse(arg.needs_transform)

    def test_case1_control_var(self):
        """Control variable → source='control', no USE module."""
        hd = self._host_dict()
        suite_var = self._scheme_var('thread_num', 'thread_number', 'in', '1')
        arg = _resolve_one_arg(suite_var, 'run', hd, {}, 'my_scheme', set())
        self.assertEqual(arg.source, 'control')
        self.assertIsNone(arg.module_name)

    def test_case1_2d_array_run(self):
        """2D array in run phase → subscript applied."""
        hd = self._host_dict()
        suite_var = self._scheme_var('temp', 'air_temperature', 'inout', 'K',
                              '(horizontal_dimension, vertical_layer_dimension)',
                              'real', 'kind_phys')
        arg = _resolve_one_arg(suite_var, 'run', hd, {}, 'my_scheme', set())
        # access_path = 'gt0', subscript = '(lb:ub, 1:nlev)'
        self.assertEqual(arg.call_expr, 'gt0(lb:ub, 1:nlev)')
        self.assertEqual(arg.transform_case, 1)

    def test_case2_suite_owned(self):
        """Case 2: not in host, first use intent(out) → creates SuiteVar."""
        hd = self._host_dict()
        suite_var = self._scheme_var('new_var', 'brand_new_standard_name', 'out', 'K',
                              '()', 'real', 'kind_phys')
        suite_vars: dict = {}
        arg = _resolve_one_arg(suite_var, 'run', hd, suite_vars, 'my_scheme', set())
        self.assertEqual(arg.source, 'suite')
        self.assertIn('brand_new_standard_name', suite_vars)
        self.assertIsNotNone(arg.suite_var)

    def test_case2_suite_owned_character_assumed_length_raises(self):
        """A character variable first defined as intent(out) by a scheme with
        kind=len=* is rejected: the defining scheme must give a concrete
        length because the framework allocates suite-owned storage for it."""
        hd = self._host_dict()
        suite_var = self._scheme_var('name', 'scheme_name', 'out', 'none',
                                     '()', 'character', 'len=*')
        with self.assertRaises(CCPPError) as cm:
            _resolve_one_arg(suite_var, 'run', hd, {}, 'def_scheme', set())
        msg = str(cm.exception)
        self.assertIn('scheme_name', msg)
        self.assertIn('len=*', msg)
        self.assertIn('def_scheme', msg)

    def test_case2_suite_owned_character_concrete_then_assumed_ok(self):
        """A concrete-length definer followed by a len=* consumer/writer is
        accepted: the suite var inherits the defining concrete length and the
        later assumed-length declaration acts as a wildcard."""
        hd = self._host_dict()
        definer = self._scheme_var('name', 'scheme_name', 'out', 'none',
                                   '()', 'character', 'len=512')
        suite_vars: dict = {}
        _resolve_one_arg(definer, 'run', hd, suite_vars, 'def_scheme', set())
        self.assertEqual(suite_vars['scheme_name'].kind, 'len=512')

        consumer = self._scheme_var('nm', 'scheme_name', 'out', 'none',
                                    '()', 'character', 'len=*')
        arg = _resolve_one_arg(consumer, 'run', hd, suite_vars, 'use_scheme',
                               set())
        self.assertEqual(arg.source, 'suite')
        # Storage length stays the defining concrete length.
        self.assertEqual(suite_vars['scheme_name'].kind, 'len=512')

    def test_case3_not_found_intent_in_raises(self):
        """Case 3: not in host, intent(in) → CCPPError."""
        hd = self._host_dict()
        suite_var = self._scheme_var('missing', 'totally_missing_stdname', 'in', 'K')
        with self.assertRaises(CCPPError) as cm:
            _resolve_one_arg(suite_var, 'run', hd, {}, 'bad_scheme', set())
        self.assertIn('totally_missing_stdname', str(cm.exception))

    def test_case4_suite_data_reuse(self):
        """Case 4: variable already in suite_vars (from prior scheme)."""
        hd = self._host_dict()
        sv_creator = self._scheme_var('new_var', 'interstitial_var', 'out', 'K',
                                      '()', 'real', 'kind_phys')
        suite_vars: dict = {}
        _resolve_one_arg(sv_creator, 'run', hd, suite_vars, 'scheme_a', set())

        sv_reader = self._scheme_var('interstitial_in', 'interstitial_var', 'in', 'K',
                                     '()', 'real', 'kind_phys')
        arg = _resolve_one_arg(sv_reader, 'run', hd, suite_vars, 'scheme_b', set())
        self.assertEqual(arg.source, 'suite')
        self.assertIsNotNone(arg.suite_var)

    def test_unit_transform_detected(self):
        """Units differ → transformation required."""
        hd = self._host_dict()
        # air_temperature is in K, request it in Pa... but there's no K→Pa conversion.
        # Use a case with a known conversion: host has 'K', scheme expects 'K' → no xform.
        # Let's not test K→Pa (no conversion), test a successful mismatch.
        # Instead, add a variable with Pa units to the host dict.
        src = '''
[ccpp-table-properties]
  name = press_mod
  type = host
[ccpp-arg-table]
  name = press_mod
  type = host
[ pres ]
  standard_name = air_pressure
  units = Pa
  dimensions = ()
  type = real
  kind = kind_phys
'''
        extra_tbls = _parse(src)
        extra_hd = build_flat_host_dict(extra_tbls, [], {})
        combined = {**hd, **extra_hd}

        suite_var = self._scheme_var('p_hpa', 'air_pressure', 'in', 'hPa', '()', 'real', 'kind_phys')
        arg = _resolve_one_arg(suite_var, 'run', combined, {}, 'my_scheme', set())
        self.assertTrue(arg.needs_unit_transform)
        self.assertEqual(arg.transform_case, 3)
        self.assertIn('temp_name', arg.__dataclass_fields__)  # has temp_name field
        self.assertTrue(arg.temp_name)

    def test_no_transform_same_units(self):
        """Identical units → no transformation."""
        hd = self._host_dict()
        suite_var = self._scheme_var(
            'im', 'horizontal_dimension', 'in', 'count')
        arg = _resolve_one_arg(suite_var, 'run', hd, {}, 'my_scheme', set())
        self.assertFalse(arg.needs_transform)

    def test_unknown_unit_mismatch_raises(self):
        """Units differ but no conversion known → CCPPError."""
        src = '''
[ccpp-table-properties]
  name = mod
  type = host
[ccpp-arg-table]
  name = mod
  type = host
[ val ]
  standard_name = some_value
  units = xyz_unit
  dimensions = ()
  type = real
  kind = kind_phys
'''
        hd = build_flat_host_dict(_parse(src), [], {})
        suite_var = self._scheme_var('v', 'some_value', 'in', 'abc_unit', '()', 'real', 'kind_phys')
        with self.assertRaises(CCPPError) as cm:
            _resolve_one_arg(suite_var, 'run', hd, {}, 'bad_scheme', set())
        self.assertIn('xyz_unit', str(cm.exception))

    def test_optional_sets_ptr_name(self):
        """Optional argument → ptr_name set."""
        hd = self._host_dict()
        suite_var = self._scheme_var(
            'im', 'horizontal_dimension', 'in', 'count', optional=True)
        arg = _resolve_one_arg(suite_var, 'run', hd, {}, 'my_scheme', set())
        self.assertTrue(arg.is_optional)
        self.assertTrue(arg.ptr_name)
        self.assertEqual(arg.transform_case, 2)


########################################################################
# Tests: host vs scheme metadata compatibility
########################################################################

class TestHostSchemeCompatibility(unittest.TestCase):
    """Resolver-level cross-metadata checks: the scheme's metadata must
    agree with the defining source (host or suite) on type, rank, and
    dimension identity.  Units and character kind have their own existing
    tests; numeric kind is intentionally lenient (triggers a transform
    copy, see [[design_numeric_kind_silent_transform]]).
    """

    _HOST_SRC = (
        '[ccpp-table-properties]\n'
        '  name = host_mod\n'
        '  type = host\n'
        '[ccpp-arg-table]\n'
        '  name = host_mod\n'
        '  type = host\n'
        '[ ncols ]\n'
        '  standard_name = horizontal_dimension\n'
        '  units = count\n'
        '  dimensions = ()\n'
        '  type = integer\n'
        '[ nlev ]\n'
        '  standard_name = vertical_layer_dimension\n'
        '  units = count\n'
        '  dimensions = ()\n'
        '  type = integer\n'
        '[ nap ]\n'
        '  standard_name = nap_indices\n'
        '  units = count\n'
        '  dimensions = ()\n'
        '  type = integer\n'
        '[ tair ]\n'
        '  standard_name = air_temperature\n'
        '  units = K\n'
        '  dimensions = (horizontal_dimension, vertical_layer_dimension)\n'
        '  type = real\n'
        '  kind = kind_phys\n'
        '[ scalar_flag ]\n'
        '  standard_name = a_scalar_flag\n'
        '  units = 1\n'
        '  dimensions = ()\n'
        '  type = real\n'
        '  kind = kind_phys\n'
        '[ ap_arr ]\n'
        '  standard_name = ap_indexed_array\n'
        '  units = count\n'
        '  dimensions = (nap_indices)\n'
        '  type = integer\n'
    )

    def _host_dict(self):
        from metadata.metadata_table import _parse_lines
        ctrl_tbls = parse_metadata_file(_sf('control_full.meta'))
        host_tbls = _parse_lines(
            self._HOST_SRC.splitlines(keepends=True), 'h.meta',
        )
        ctrl_only = [t for t in ctrl_tbls if t.table_type == 'control']
        return build_flat_host_dict(host_tbls, ctrl_only, [])

    def _scheme_var(self, std_name, type_, dims, units='K', kind='kind_phys',
                    intent='inout'):
        from metadata.metadata_table import MetaVar
        ctx = _ctx()
        v = MetaVar('x', ctx)
        v.set_attr('standard_name', std_name, ctx)
        v.set_attr('units', units, ctx)
        v.set_attr('dimensions', dims, ctx)
        v.set_attr('type', type_, ctx)
        if kind:
            v.set_attr('kind', kind, ctx)
        v.set_attr('intent', intent, ctx)
        return v

    def test_host_scalar_scheme_rank1_raises(self):
        """Host scalar `()` with scheme `(horizontal_dimension)` is a rank
        mismatch — user-reported gap that the resolver now catches."""
        hd = self._host_dict()
        sv = self._scheme_var(
            'a_scalar_flag', 'real',
            '(horizontal_dimension)', units='1',
        )
        with self.assertRaises(CCPPError) as cm:
            _resolve_one_arg(sv, 'run', hd, {}, 'sch', set())
        msg = str(cm.exception)
        self.assertIn('rank', msg)
        self.assertIn('a_scalar_flag', msg)

    def test_host_dim_mismatch_raises(self):
        """Host `(nap_indices)` with scheme `(horizontal_dimension)` —
        same rank, different axis — is a metadata error."""
        hd = self._host_dict()
        sv = self._scheme_var(
            'ap_indexed_array', 'integer',
            '(horizontal_dimension)', units='count', kind='',
        )
        with self.assertRaises(CCPPError) as cm:
            _resolve_one_arg(sv, 'run', hd, {}, 'sch', set())
        msg = str(cm.exception)
        self.assertIn('dimension', msg.lower())
        self.assertIn('nap_indices', msg)
        self.assertIn('horizontal_dimension', msg)

    def test_type_mismatch_raises(self):
        """Host `integer` with scheme `real` is a type-identity error
        even when units and rank align."""
        hd = self._host_dict()
        sv = self._scheme_var(
            'horizontal_dimension', 'real', '()', units='count', kind='',
        )
        with self.assertRaises(CCPPError) as cm:
            _resolve_one_arg(sv, 'run', hd, {}, 'sch', set())
        msg = str(cm.exception)
        self.assertIn('type', msg.lower())

    def test_default_lower_bound_spellings_equivalent(self):
        """Three spellings of the default lower bound are equivalent:
        bare ``X``, ``1:X``, and ``ccpp_constant_one:X``.  The host's
        bare ``vertical_layer_dimension`` matches any of these forms
        on the scheme side."""
        hd = self._host_dict()
        for sdim in (
            'vertical_layer_dimension',
            '1:vertical_layer_dimension',
            'ccpp_constant_one:vertical_layer_dimension',
        ):
            with self.subTest(scheme_dim=sdim):
                sv = self._scheme_var(
                    'air_temperature', 'real',
                    '(horizontal_dimension, {})'.format(sdim),
                )
                arg = _resolve_one_arg(sv, 'run', hd, {}, 'sch', set())
                self.assertEqual(arg.standard_name, 'air_temperature')

    def test_nondefault_integer_lower_bound_mismatch_raises(self):
        """``2:nlev`` and ``1:nlev`` are NOT the same axis — different
        lower bound means different sub-range, even when both are
        integer literals."""
        hd = self._host_dict()
        sv = self._scheme_var(
            'air_temperature', 'real',
            '(horizontal_dimension, 2:vertical_layer_dimension)',
        )
        with self.assertRaises(CCPPError) as cm:
            _resolve_one_arg(sv, 'run', hd, {}, 'sch', set())
        msg = str(cm.exception)
        self.assertIn('dimension', msg.lower())
        self.assertIn('vertical_layer_dimension', msg)
        self.assertIn('2:', msg)

    def test_nondefault_named_lower_bound_mismatch_raises(self):
        """A non-default standard-name lower bound (``foo:nlev``) is
        distinct from the default ``ccpp_constant_one:nlev``."""
        hd = self._host_dict()
        sv = self._scheme_var(
            'air_temperature', 'real',
            '(horizontal_dimension, '
            'some_made_up_lower_bound:vertical_layer_dimension)',
        )
        with self.assertRaises(CCPPError) as cm:
            _resolve_one_arg(sv, 'run', hd, {}, 'sch', set())
        msg = str(cm.exception)
        self.assertIn('dimension', msg.lower())
        self.assertIn('vertical_layer_dimension', msg)
        self.assertIn('some_made_up_lower_bound', msg)

    def test_horizontal_loop_extent_in_scheme_dims_raises(self):
        """No name aliasing in the compat check: a scheme that uses the
        legacy ``horizontal_loop_extent`` in a dimension list while the
        host declares ``horizontal_dimension`` is a mismatch.  The
        legacy-compat shim is the only place such rewriting belongs."""
        hd = self._host_dict()
        sv = self._scheme_var(
            'air_temperature', 'real',
            '(horizontal_loop_extent, vertical_layer_dimension)',
        )
        with self.assertRaises(CCPPError) as cm:
            _resolve_one_arg(sv, 'run', hd, {}, 'sch', set())
        msg = str(cm.exception)
        self.assertIn('horizontal_dimension', msg)
        self.assertIn('horizontal_loop_extent', msg)

    def test_suite_var_second_reader_mismatch_raises(self):
        """First scheme with intent=out fixes the SuiteVar's type/rank;
        a later scheme that reads it with mismatched dims is rejected."""
        hd = self._host_dict()
        # First scheme creates the suite var.
        writer = self._scheme_var(
            'an_arbitrary_suite_quantity', 'real',
            '(horizontal_dimension, vertical_layer_dimension)',
            units='K', intent='out',
        )
        suite_vars = {}
        _resolve_one_arg(writer, 'run', hd, suite_vars, 'sch_a', set())
        self.assertIn('an_arbitrary_suite_quantity', suite_vars)
        # Second scheme reads it — but with a wrong rank.
        reader = self._scheme_var(
            'an_arbitrary_suite_quantity', 'real',
            '(horizontal_dimension)', units='K', intent='in',
        )
        with self.assertRaises(CCPPError) as cm:
            _resolve_one_arg(reader, 'run', hd, suite_vars, 'sch_b', set())
        msg = str(cm.exception)
        self.assertIn('rank', msg)
        self.assertIn('suite', msg)


########################################################################
# Tests: host active + scheme arg coherence
########################################################################

class TestActiveHostHandling(unittest.TestCase):
    """When the host declares ``active = (<flag>)`` on a variable:

    * Scheme arg ``optional = True`` -> the cap emits the pointer-
      association pattern (transform_case 2 or 4); the scheme observes
      PRESENT()=.false. when the condition is false.
    * Scheme arg non-optional -> the resolver still succeeds (the
      scheme is asserting the variable is mandatory); the group cap
      emits a runtime guard that raises errflg/errmsg if the condition
      is false at call time.  The asymmetric optional rule in the
      validator covers the metadata/Fortran consistency check; this
      class only exercises resolver-level behaviour."""

    _HOST_SRC = (
        '[ccpp-table-properties]\n'
        '  name = active_host\n'
        '  type = host\n'
        '[ccpp-arg-table]\n'
        '  name = active_host\n'
        '  type = host\n'
        '[ ncols ]\n'
        '  standard_name = horizontal_dimension\n'
        '  units = count\n'
        '  dimensions = ()\n'
        '  type = integer\n'
        '[ nlev ]\n'
        '  standard_name = vertical_layer_dimension\n'
        '  units = count\n'
        '  dimensions = ()\n'
        '  type = integer\n'
        '[ flag_passive ]\n'
        '  standard_name = flag_for_passive_check\n'
        '  units = flag\n'
        '  dimensions = ()\n'
        '  type = logical\n'
        '[ gt0 ]\n'
        '  standard_name = air_temperature\n'
        '  units = K\n'
        '  dimensions = (horizontal_dimension, vertical_layer_dimension)\n'
        '  type = real\n'
        '  kind = kind_phys\n'
        '  active = (flag_for_passive_check)\n'
    )

    def _host_dict(self):
        from metadata.metadata_table import _parse_lines
        ctrl_tbls = parse_metadata_file(_sf('control_full.meta'))
        host_tbls = _parse_lines(
            self._HOST_SRC.splitlines(keepends=True), 'h.meta',
        )
        ctrl_only = [t for t in ctrl_tbls if t.table_type == 'control']
        return build_flat_host_dict(host_tbls, ctrl_only, [])

    def _scheme_var(self, optional):
        from metadata.metadata_table import MetaVar
        ctx = _ctx()
        v = MetaVar('temp', ctx)
        v.set_attr('standard_name', 'air_temperature', ctx)
        v.set_attr('units', 'K', ctx)
        v.set_attr('dimensions',
                   '(horizontal_dimension, vertical_layer_dimension)', ctx)
        v.set_attr('type', 'real', ctx)
        v.set_attr('kind', 'kind_phys', ctx)
        v.set_attr('intent', 'inout', ctx)
        if optional:
            v.set_attr('optional', 'True', ctx)
        return v

    def test_optional_scheme_arg_passes(self):
        hd = self._host_dict()
        arg = _resolve_one_arg(self._scheme_var(optional=True), 'run', hd,
                               {}, 'my_scheme', set())
        self.assertEqual(arg.active, '(flag_for_passive_check)')
        self.assertTrue(arg.is_optional)
        # Cap uses pointer-association: transform_case 2 (or 4 with
        # transform).  Both are pointer-pattern paths.
        self.assertIn(arg.transform_case, (2, 4))

    def test_required_scheme_arg_resolves_with_active(self):
        """A non-optional scheme arg paired with a host ``active = (...)``
        variable resolves cleanly: the resolver passes the active
        condition through (so the group cap can emit a runtime guard)
        and selects a non-pointer transform_case (1 or 3)."""
        hd = self._host_dict()
        arg = _resolve_one_arg(self._scheme_var(optional=False), 'run', hd,
                               {}, 'my_scheme', set())
        self.assertEqual(arg.active, '(flag_for_passive_check)')
        # active_local is the same string here since flag_for_passive_check
        # is referenced via its standard name with no rename.
        self.assertIn('flag_passive', arg.active_local)
        self.assertFalse(arg.is_optional)
        # No pointer wrapper for a required arg; case 1 (direct) or 3 (transform).
        self.assertIn(arg.transform_case, (1, 3))
        self.assertEqual(arg.ptr_name, '')

    def test_required_scheme_arg_emits_runtime_guard(self):
        """Group-cap emitter renders the guard block for a non-optional
        arg whose host declares ``active = (...)`` — guard is emitted at
        the call indent, raises errflg/errmsg with a clear message, and
        does *not* wrap the arg in a pointer."""
        hd = self._host_dict()
        arg = _resolve_one_arg(self._scheme_var(optional=False), 'run', hd,
                               {}, 'my_scheme', set())
        guard = _active_required_guard_lines(
            arg, scheme_name='my_scheme', phase='run',
            errflg_local='errflg', errmsg_local='errmsg',
            indent='    ',
        )
        self.assertTrue(guard, "expected a non-empty guard block")
        body = '\n'.join(guard)
        self.assertIn('if (.not. (', body)
        self.assertIn('flag_passive', body)
        self.assertIn("my_scheme", body)
        self.assertIn("air_temperature", body)
        self.assertIn('errflg = 1', body)
        self.assertIn('return', body)

    def test_optional_scheme_arg_skips_runtime_guard(self):
        """The runtime guard is only for non-optional args — optional
        args use the pointer-association pattern and need no guard."""
        hd = self._host_dict()
        arg = _resolve_one_arg(self._scheme_var(optional=True), 'run', hd,
                               {}, 'my_scheme', set())
        guard = _active_required_guard_lines(
            arg, scheme_name='my_scheme', phase='run',
            errflg_local='errflg', errmsg_local='errmsg',
            indent='    ',
        )
        self.assertEqual(guard, [])

    def test_guard_skipped_when_no_error_locals(self):
        """If the host did not declare ccpp_error_code/ccpp_error_message,
        the guard is suppressed — there is no way to report the violation."""
        hd = self._host_dict()
        arg = _resolve_one_arg(self._scheme_var(optional=False), 'run', hd,
                               {}, 'my_scheme', set())
        guard = _active_required_guard_lines(
            arg, scheme_name='my_scheme', phase='run',
            errflg_local=None, errmsg_local=None,
            indent='    ',
        )
        self.assertEqual(guard, [])


########################################################################
# Tests: vertical-flip transform (top_at_one mismatch)
########################################################################

class TestVerticalFlipTransform(unittest.TestCase):
    """When host and scheme disagree on ``top_at_one`` for a variable that
    carries a vertical dimension, the resolver emits a flipped host-side
    subscript and turns on the temp/transform pipeline so the call site
    copies data through a contiguous local in scheme order.
    """

    def _build_host_and_scheme(
        self,
        host_top_at_one: bool = False,
        scheme_top_at_one: bool = False,
        host_units: str = 'K',
        scheme_units: str = 'K',
        intent: str = 'inout',
    ):
        host_src = '''
[ccpp-table-properties]
  name = mod
  type = host
[ccpp-arg-table]
  name = mod
  type = host
[ ncols ]
  standard_name = horizontal_dimension
  units = count
  dimensions = ()
  type = integer
[ nlev ]
  standard_name = vertical_layer_dimension
  units = count
  dimensions = ()
  type = integer
[ gt0 ]
  standard_name = air_temperature
  units = {units}
  dimensions = (horizontal_dimension, vertical_layer_dimension)
  type = real
  kind = kind_phys
  top_at_one = {top}
'''.format(units=host_units, top='True' if host_top_at_one else 'False')

        ctrl_src = '''
[ccpp-table-properties]
  name = ctrl
  type = control
[ccpp-arg-table]
  name = ctrl
  type = control
[ lb ]
  standard_name = horizontal_loop_begin
  units = index
  dimensions = ()
  type = integer
[ ub ]
  standard_name = horizontal_loop_end
  units = index
  dimensions = ()
  type = integer
'''
        hd = build_flat_host_dict(_parse(host_src), _parse(ctrl_src), [])

        from metadata.metadata_table import MetaVar
        ctx = _ctx()
        suite_var = MetaVar('temp', ctx)
        suite_var.set_attr('standard_name', 'air_temperature', ctx)
        suite_var.set_attr('units', scheme_units, ctx)
        suite_var.set_attr('dimensions',
                    '(horizontal_dimension, vertical_layer_dimension)', ctx)
        suite_var.set_attr('type', 'real', ctx)
        suite_var.set_attr('kind', 'kind_phys', ctx)
        suite_var.set_attr('intent', intent, ctx)
        if scheme_top_at_one:
            suite_var.set_attr('top_at_one', 'True', ctx)
        return hd, suite_var

    def test_no_flip_when_both_false(self):
        hd, suite_var = self._build_host_and_scheme(False, False)
        arg = _resolve_one_arg(suite_var, 'run', hd, {}, 'sch', set())
        self.assertFalse(arg.needs_vert_flip)
        self.assertEqual(arg.transform_case, 1)
        self.assertEqual(arg.call_expr, 'gt0(lb:ub, 1:nlev)')

    def test_no_flip_when_both_true(self):
        hd, suite_var = self._build_host_and_scheme(True, True)
        arg = _resolve_one_arg(suite_var, 'run', hd, {}, 'sch', set())
        self.assertFalse(arg.needs_vert_flip)
        self.assertEqual(arg.transform_case, 1)
        self.assertEqual(arg.call_expr, 'gt0(lb:ub, 1:nlev)')

    def test_flip_when_host_false_scheme_true(self):
        hd, suite_var = self._build_host_and_scheme(False, True)
        arg = _resolve_one_arg(suite_var, 'run', hd, {}, 'sch', set())
        self.assertTrue(arg.needs_vert_flip)
        self.assertTrue(arg.needs_transform)
        # Transform pipeline: temp local, transform_case 3, no unit conv.
        self.assertEqual(arg.transform_case, 3)
        self.assertTrue(arg.temp_name)
        self.assertFalse(arg.needs_unit_transform)
        self.assertFalse(arg.needs_kind_transform)
        # Host-side subscript carries reverse stride at the vdim position.
        self.assertEqual(arg.call_expr, 'gt0(lb:ub, nlev:1:-1)')
        # Forward (pre-call) copies host → temp; backward (post-call)
        # copies temp → host using the same flipped LHS.
        self.assertEqual(arg.unit_forward, 'gt0(lb:ub, nlev:1:-1)')
        self.assertEqual(arg.unit_backward, 'temp_l')

    def test_flip_when_host_true_scheme_false(self):
        hd, suite_var = self._build_host_and_scheme(True, False)
        arg = _resolve_one_arg(suite_var, 'run', hd, {}, 'sch', set())
        self.assertTrue(arg.needs_vert_flip)
        self.assertEqual(arg.call_expr, 'gt0(lb:ub, nlev:1:-1)')

    def test_flip_composes_with_unit_conversion(self):
        """Mismatched top_at_one AND a unit conversion → the unit-forward
        formula is applied to the flipped call_expr; the temp pattern is
        a single combined assignment."""
        hd, suite_var = self._build_host_and_scheme(False, True,
                                              host_units='Pa', scheme_units='hPa')
        arg = _resolve_one_arg(suite_var, 'run', hd, {}, 'sch', set())
        self.assertTrue(arg.needs_vert_flip)
        self.assertTrue(arg.needs_unit_transform)
        self.assertEqual(arg.transform_case, 3)
        # The flipped subscript is embedded inside the unit-conversion expr.
        self.assertIn('gt0(lb:ub, nlev:1:-1)', arg.unit_forward)
        # And backward uses the temp's regular order multiplied by the
        # inverse factor; LHS at the post-call site uses the same flipped
        # subscript.
        self.assertIn('temp_l', arg.unit_backward)
        self.assertEqual(arg.call_expr, 'gt0(lb:ub, nlev:1:-1)')

    def test_intent_in_only_emits_forward(self):
        hd, suite_var = self._build_host_and_scheme(False, True, intent='in')
        arg = _resolve_one_arg(suite_var, 'run', hd, {}, 'sch', set())
        self.assertTrue(arg.needs_vert_flip)
        self.assertEqual(arg.unit_forward, 'gt0(lb:ub, nlev:1:-1)')
        self.assertEqual(arg.unit_backward, '')

    def test_intent_out_only_emits_backward(self):
        hd, suite_var = self._build_host_and_scheme(False, True, intent='out')
        arg = _resolve_one_arg(suite_var, 'run', hd, {}, 'sch', set())
        self.assertTrue(arg.needs_vert_flip)
        self.assertEqual(arg.unit_forward, '')
        self.assertEqual(arg.unit_backward, 'temp_l')

    def test_no_flip_on_scalar(self):
        """top_at_one on a scalar (no vertical dim) is a no-op — flip needs
        a vertical-axis dimension to operate on.
        """
        host_src = '''
[ccpp-table-properties]
  name = mod
  type = host
[ccpp-arg-table]
  name = mod
  type = host
[ scalar_thing ]
  standard_name = some_scalar
  units = 1
  dimensions = ()
  type = real
  kind = kind_phys
  top_at_one = True
'''
        hd = build_flat_host_dict(_parse(host_src), [], [])

        from metadata.metadata_table import MetaVar
        ctx = _ctx()
        suite_var = MetaVar('s', ctx)
        suite_var.set_attr('standard_name', 'some_scalar', ctx)
        suite_var.set_attr('units', '1', ctx)
        suite_var.set_attr('dimensions', '()', ctx)
        suite_var.set_attr('type', 'real', ctx)
        suite_var.set_attr('kind', 'kind_phys', ctx)
        suite_var.set_attr('intent', 'in', ctx)
        # Scheme leaves top_at_one at default (False).
        arg = _resolve_one_arg(suite_var, 'run', hd, {}, 'sch', set())
        self.assertFalse(arg.needs_vert_flip)


########################################################################
# Tests: character kind (len=) validation
########################################################################

class TestCharacterKindResolution(unittest.TestCase):
    """len=* in scheme is always compatible; mismatched specific lengths are errors."""

    def _host_with_char(self, kind='len=512'):
        """Build a host dict with a character variable of the given kind."""
        src = '''
[ccpp-table-properties]
  name = hmod
  type = host
[ccpp-arg-table]
  name = hmod
  type = host
[ msg ]
  standard_name = my_message
  units = none
  dimensions = ()
  type = character
  kind = {kind}
'''.format(kind=kind)
        tbls = _parse(src)
        return build_flat_host_dict(tbls, [], [])

    def _scheme_var_char(self, local, std_name, kind):
        from metadata.metadata_table import MetaVar
        ctx = _ctx()
        v = MetaVar(local, ctx)
        v.set_attr('standard_name', std_name, ctx)
        v.set_attr('units', 'none', ctx)
        v.set_attr('dimensions', '()', ctx)
        v.set_attr('type', 'character', ctx)
        v.set_attr('kind', kind, ctx)
        v.set_attr('intent', 'out', ctx)
        return v

    def test_len_star_compatible_with_len_512(self):
        """len=* in scheme is always compatible — no transform, no error."""
        hd = self._host_with_char('len=512')
        suite_var = self._scheme_var_char('msg', 'my_message', 'len=*')
        arg = _resolve_one_arg(suite_var, 'run', hd, {}, 'sch', set())
        self.assertFalse(arg.needs_kind_transform)
        self.assertEqual(arg.transform_case, 1)
        self.assertEqual(arg.temp_name, '')

    def test_len_match_compatible(self):
        """Same specific len=N in both host and scheme — no transform."""
        hd = self._host_with_char('len=512')
        suite_var = self._scheme_var_char('msg', 'my_message', 'len=512')
        arg = _resolve_one_arg(suite_var, 'run', hd, {}, 'sch', set())
        self.assertFalse(arg.needs_kind_transform)

    def test_len_star_in_host_raises(self):
        """len=* in the host is rejected at host-dict construction: a host
        character variable defines storage and must have a concrete length
        (assumed length is valid only for a scheme dummy argument)."""
        with self.assertRaises(CCPPError) as cm:
            self._host_with_char('len=*')
        self.assertIn('len=*', str(cm.exception))

    def test_mismatched_specific_lengths_raises(self):
        """Specific len=128 vs len=512 is a metadata error."""
        hd = self._host_with_char('len=512')
        suite_var = self._scheme_var_char('msg', 'my_message', 'len=128')
        with self.assertRaises(CCPPError) as cm:
            _resolve_one_arg(suite_var, 'run', hd, {}, 'bad_scheme', set())
        self.assertIn('len=512', str(cm.exception))
        self.assertIn('len=128', str(cm.exception))


########################################################################
# Tests: pure-kind transform (real-kind cast)
########################################################################

class TestPureKindTransform(unittest.TestCase):
    """When host and scheme metadata differ in *kind* only (no unit
    mismatch, no vertical flip), the resolver must emit a real/int kind
    cast as ``unit_forward``.  Without this the cap declares the
    transformation temporary but never assigns to it -- gfortran falls
    back to implicit typing at the call site and the call sees garbage
    / Inf.  Regression: SCM_GFS_v17_p8 / bomex started failing with
    ``alon = -Infinity`` in ``setclimaer`` after the host changed
    ``scm_physical_constants`` from ``kind = kind_phys`` to
    ``kind = dp`` -- a pure-kind mismatch against the GFS_rrtmg_pre
    scheme args, which expect ``kind_phys``."""

    _HOST_SRC_TEMPLATE = '''
[ccpp-table-properties]
  name = phys_const
  type = host
[ccpp-arg-table]
  name = phys_const
  type = host
[ con_pi ]
  standard_name = pi
  units = none
  dimensions = ()
  type = {type}
  kind = {kind}
'''

    def _hd(self, kind='dp', type_='real'):
        src = self._HOST_SRC_TEMPLATE.format(kind=kind, type=type_)
        return build_flat_host_dict(_parse(src), [], [])

    def _scheme_var_pi(self, intent='in', kind='kind_phys', type_='real'):
        from metadata.metadata_table import MetaVar
        ctx = _ctx()
        v = MetaVar('con_pi', ctx)
        v.set_attr('standard_name', 'pi', ctx)
        v.set_attr('units', 'none', ctx)
        v.set_attr('dimensions', '()', ctx)
        v.set_attr('type', type_, ctx)
        v.set_attr('kind', kind, ctx)
        v.set_attr('intent', intent, ctx)
        return v

    def test_real_kind_mismatch_emits_real_cast_forward(self):
        hd = self._hd(kind='dp')
        scheme = self._scheme_var_pi(intent='in', kind='kind_phys')
        arg = _resolve_one_arg(scheme, 'run', hd, {}, 'rrtmg', set())
        self.assertTrue(arg.needs_kind_transform)
        self.assertTrue(arg.needs_transform)
        self.assertEqual(arg.transform_case, 3)
        # Temp must be both NAMED and ASSIGNED (the bug was that the temp
        # was named but unit_forward stayed empty, so no assignment was
        # emitted by the cap).
        self.assertEqual(arg.temp_name, 'con_pi_l')
        self.assertEqual(arg.unit_forward, 'real(con_pi, kind=kind_phys)')

    def test_real_kind_mismatch_emits_real_cast_backward_for_inout(self):
        hd = self._hd(kind='dp')
        scheme = self._scheme_var_pi(intent='inout', kind='kind_phys')
        arg = _resolve_one_arg(scheme, 'run', hd, {}, 'rrtmg', set())
        self.assertEqual(arg.unit_forward, 'real(con_pi, kind=kind_phys)')
        self.assertEqual(arg.unit_backward, 'real(con_pi_l, kind=dp)')

    def test_integer_kind_mismatch_emits_int_cast(self):
        hd = self._hd(kind='int_8', type_='integer')
        scheme = self._scheme_var_pi(intent='in', kind='int_4',
                                     type_='integer')
        arg = _resolve_one_arg(scheme, 'run', hd, {}, 'rrtmg', set())
        self.assertEqual(arg.unit_forward, 'int(con_pi, kind=int_4)')

    def test_unsupported_type_for_kind_cast_raises(self):
        """A kind mismatch on a type without a kind-cast intrinsic (DDT,
        logical) should raise a clear CCPPError pointing the user at
        the metadata rather than silently emitting unassigned temps."""
        # Use a logical with two different kind names.
        hd = self._hd(kind='lk1', type_='logical')
        scheme = self._scheme_var_pi(intent='in', kind='lk2',
                                     type_='logical')
        with self.assertRaises(CCPPError) as cm:
            _resolve_one_arg(scheme, 'run', hd, {}, 'rrtmg', set())
        msg = str(cm.exception)
        self.assertIn("kind-cast intrinsic", msg)
        self.assertIn("logical", msg)


########################################################################
# Integration tests: resolve_suite
########################################################################

class TestResolveSuite(unittest.TestCase):

    def _resolve(self):
        hd = _load_full_host_dict()
        store = _load_scheme_store()
        suite = _parse_suite('suite_test_simple.xml')
        return resolve_suite(suite, store, hd), hd

    def test_groups_present(self):
        suite_resolution, _ = self._resolve()
        self.assertEqual(suite_resolution.suite_name, 'test_simple')
        self.assertEqual(len(suite_resolution.groups), 1)
        self.assertEqual(suite_resolution.groups[0].group_name, 'physics')

    def test_run_phase_calls(self):
        suite_resolution, _ = self._resolve()
        resolved_group = suite_resolution.groups[0]
        self.assertIn('run', resolved_group.phase_calls)
        calls = resolved_group.phase_calls['run']
        self.assertEqual(len(calls), 1)
        self.assertEqual(calls[0].scheme_name, 'temp_calc_adjust')

    def test_run_phase_args(self):
        suite_resolution, _ = self._resolve()
        calls = suite_resolution.groups[0].phase_calls['run']
        args = {a.scheme_local_name: a for a in calls[0].args}
        # im = horizontal_dimension: scalar arg is synthesised from the loop
        # bounds so the scheme sees the per-call chunk extent, not host ncols.
        self.assertIn('im', args)
        self.assertEqual(args['im'].call_expr, '(ub - lb + 1)')
        # temp = air_temperature (2D, run phase subscript)
        self.assertIn('temp', args)
        self.assertEqual(args['temp'].call_expr, 'gt0(lb:ub, 1:nlev)')
        # errmsg and errflg
        self.assertIn('errmsg', args)
        self.assertIn('errflg', args)

    def test_init_phase_calls(self):
        suite_resolution, _ = self._resolve()
        resolved_group = suite_resolution.groups[0]
        self.assertIn('init', resolved_group.phase_calls)
        calls = resolved_group.phase_calls['init']
        self.assertEqual(calls[0].scheme_name, 'temp_calc_adjust')

    def test_init_phase_horizontal_subscript(self):
        """In init phase, scalar horizontal_dimension does not produce an lb:ub slice."""
        suite_resolution, _ = self._resolve()
        resolved_group = suite_resolution.groups[0]
        # temp_calc_adjust_init doesn't have temp, but the general rule should hold
        # for any 2D variable in a non-run phase: no lb:ub slice in init args.
        # (Scalar horizontal_dimension args are synthesised as (ub - lb + 1),
        # which collapses to ncols in non-run phases but never contains
        # the substring 'lb:ub'.)
        calls = resolved_group.phase_calls.get('init', [])
        for resolved_call in calls:
            for arg in resolved_call.args:
                self.assertNotIn('lb:ub', arg.call_expr)

    def test_no_suite_vars(self):
        """All variables in temp_calc_adjust are provided by the host."""
        suite_resolution, _ = self._resolve()
        self.assertEqual(suite_resolution.suite_vars, {})

    def test_used_modules(self):
        suite_resolution, _ = self._resolve()
        calls = suite_resolution.groups[0].phase_calls['run']
        mods = calls[0].used_modules
        # host_phys should appear (air_temperature, horizontal_dimension, etc.)
        self.assertIn('host_phys', mods)

    def test_control_args_no_module(self):
        suite_resolution, _ = self._resolve()
        calls = suite_resolution.groups[0].phase_calls['run']
        ctrl = [a for a in calls[0].args if a.source == 'control']
        for c in ctrl:
            self.assertIsNone(c.module_name)


class TestResolveSuiteLoopContextVariables(unittest.TestCase):
    """``ccpp_loop_counter`` and ``ccpp_loop_extent`` are loop-context
    control variables scoped to the body of a ``<subcycle>`` block.
    Scheme args declaring them MUST resolve against the generated
    do-loop locals when inside a subcycle, and raise a clear error
    when outside.  Regression for the SCM GFS_surface_loop_control
    failure where the resolver bailed with the generic 'not provided
    by host' message."""

    def _build_suite_from_xml(self, xml_src: str):
        import tempfile, os
        from generator.suite_xml import parse_suite_xml
        from metadata.parse_tools import init_log
        log = init_log('test_loop_ctx')
        with tempfile.TemporaryDirectory() as tdir:
            path = os.path.join(tdir, 's.xml')
            with open(path, 'w') as fh:
                fh.write(xml_src)
            return parse_suite_xml(path, output_root=tdir, logger=log)

    _LOOP_SCHEME_SRC = '''
[ccpp-table-properties]
  name = loop_scheme
  type = scheme
[ccpp-arg-table]
  name = loop_scheme_run
  type = scheme
[ iter ]
  standard_name = ccpp_loop_counter
  units = index
  dimensions = ()
  type = integer
  intent = in
[ niter ]
  standard_name = ccpp_loop_extent
  units = index
  dimensions = ()
  type = integer
  intent = in
[ errmsg ]
  standard_name = ccpp_error_message
  units = none
  dimensions = ()
  type = character
  kind = len=512
  intent = out
[ errflg ]
  standard_name = ccpp_error_code
  units = 1
  dimensions = ()
  type = integer
  intent = out
'''

    def _store(self):
        from metadata.variable_resolver import SchemeStore
        return SchemeStore.build_from(
            _parse(self._LOOP_SCHEME_SRC, 'loop_scheme.meta')
        )

    def _args_by_name(self, suite_resolution):
        calls = list(iter_phase_calls(suite_resolution.groups[0].phase_calls['run']))
        return {a.scheme_local_name: a for a in calls[0].args}

    def test_counter_inside_subcycle_resolves_to_loop_local(self):
        xml = (
            "<?xml version='1.0' encoding='UTF-8'?>\n"
            "<suite name='loop_simple' version='2.0'>\n"
            "  <group name='physics'>\n"
            "    <subcycle loop='3'>\n"
            "      <scheme>loop_scheme</scheme>\n"
            "    </subcycle>\n"
            "  </group>\n"
            "</suite>\n"
        )
        suite_resolution = resolve_suite(self._build_suite_from_xml(xml),
                           self._store(), _load_full_host_dict())
        args = self._args_by_name(suite_resolution)
        self.assertEqual(args['iter'].standard_name, 'ccpp_loop_counter')
        self.assertEqual(args['iter'].call_expr, 'ccpp_loop_counter')
        self.assertEqual(args['iter'].source, 'control')

    def test_extent_integer_literal(self):
        xml = (
            "<?xml version='1.0' encoding='UTF-8'?>\n"
            "<suite name='loop_extent' version='2.0'>\n"
            "  <group name='physics'>\n"
            "    <subcycle loop='3'>\n"
            "      <scheme>loop_scheme</scheme>\n"
            "    </subcycle>\n"
            "  </group>\n"
            "</suite>\n"
        )
        suite_resolution = resolve_suite(self._build_suite_from_xml(xml),
                           self._store(), _load_full_host_dict())
        args = self._args_by_name(suite_resolution)
        # ``loop=3`` is a literal — extent resolves to the same literal.
        self.assertEqual(args['niter'].call_expr, '3')

    def test_extent_std_name_resolves_to_host_local(self):
        """``loop=<std_name>`` (e.g. host control var ``num_subcycles_for_test``
        with local name ``n_sub``) must resolve ccpp_loop_extent to the
        host's local Fortran name."""
        from metadata.metadata_table import parse_metadata_file
        from metadata.variable_resolver import build_flat_host_dict
        host_tbls = parse_metadata_file(_sf('host_full.meta'))
        ctrl_tbls = parse_metadata_file(_sf('control_full.meta'))
        extra_tbls = parse_metadata_file(_sf('host_subcycle_stdname.meta'))
        hd = build_flat_host_dict(host_tbls + extra_tbls, ctrl_tbls, [])
        xml = (
            "<?xml version='1.0' encoding='UTF-8'?>\n"
            "<suite name='loop_named' version='2.0'>\n"
            "  <group name='physics'>\n"
            "    <subcycle loop='num_subcycles_for_test'>\n"
            "      <scheme>loop_scheme</scheme>\n"
            "    </subcycle>\n"
            "  </group>\n"
            "</suite>\n"
        )
        suite_resolution = resolve_suite(self._build_suite_from_xml(xml),
                           self._store(), hd)
        args = self._args_by_name(suite_resolution)
        self.assertEqual(args['niter'].call_expr, 'n_sub')

    def test_outside_subcycle_raises_clear_error(self):
        xml = (
            "<?xml version='1.0' encoding='UTF-8'?>\n"
            "<suite name='no_subcycle' version='2.0'>\n"
            "  <group name='physics'>\n"
            "    <scheme>loop_scheme</scheme>\n"
            "  </group>\n"
            "</suite>\n"
        )
        with self.assertRaises(CCPPError) as ctx:
            resolve_suite(self._build_suite_from_xml(xml),
                          self._store(), _load_full_host_dict())
        msg = str(ctx.exception)
        # Names the scheme + the offending std_name + the SDF remediation.
        self.assertIn('loop_scheme', msg)
        self.assertIn('ccpp_loop_counter', msg)
        self.assertIn('<subcycle', msg)


class TestResolveSuiteMissingSchemeFailsLoudly(unittest.TestCase):
    """An SDF that references a scheme whose ``.meta`` was not passed
    via ``--scheme-files`` MUST raise ``CCPPError`` at resolve time,
    listing every missing scheme.  Regression for the silent-empty-cap
    bug: capgen would otherwise emit a syntactically valid but
    semantically empty group cap and the build would succeed with the
    wrong runtime behaviour."""

    def _build_suite_from_xml(self, xml_src: str):
        """Parse a suite XML string and return the resulting Suite."""
        import tempfile, os
        from generator.suite_xml import parse_suite_xml
        from metadata.parse_tools import init_log
        log = init_log('test_missing_scheme')
        with tempfile.TemporaryDirectory() as tdir:
            path = os.path.join(tdir, 'suite_missing.xml')
            with open(path, 'w') as fh:
                fh.write(xml_src)
            return parse_suite_xml(path, output_root=tdir, logger=log)

    def test_unknown_scheme_in_group_raises(self):
        hd = _load_full_host_dict()
        store = _load_scheme_store()  # has temp_calc_adjust only
        xml_src = (
            "<?xml version='1.0' encoding='UTF-8'?>\n"
            "<suite name='bad_suite' version='2.0'>\n"
            "  <group name='physics'>\n"
            "    <scheme>temp_calc_adjust</scheme>\n"
            "    <scheme>not_a_real_scheme</scheme>\n"
            "    <scheme>also_missing</scheme>\n"
            "  </group>\n"
            "</suite>\n"
        )
        suite = self._build_suite_from_xml(xml_src)
        with self.assertRaises(CCPPError) as ctx:
            resolve_suite(suite, store, hd)
        msg = str(ctx.exception)
        # Names every missing scheme.
        self.assertIn('not_a_real_scheme', msg)
        self.assertIn('also_missing', msg)
        # Names the suite for context.
        self.assertIn('bad_suite', msg)
        # Points the user at --scheme-files (or the CMake equivalent).
        self.assertIn('--scheme-files', msg)

    def test_unknown_scheme_in_suite_init_raises(self):
        hd = _load_full_host_dict()
        store = _load_scheme_store()
        xml_src = (
            "<?xml version='1.0' encoding='UTF-8'?>\n"
            "<suite name='bad_init_suite' version='2.0'>\n"
            "  <init>nowhere_to_find_me</init>\n"
            "  <group name='physics'>\n"
            "    <scheme>temp_calc_adjust</scheme>\n"
            "  </group>\n"
            "</suite>\n"
        )
        suite = self._build_suite_from_xml(xml_src)
        with self.assertRaises(CCPPError) as ctx:
            resolve_suite(suite, store, hd)
        self.assertIn('nowhere_to_find_me', str(ctx.exception))


class TestDedupSchemeNames(unittest.TestCase):
    """Unit tests for the non-run-phase dedup helper."""

    def test_no_duplicates_passthrough(self):
        self.assertEqual(_dedup_scheme_names(['a', 'b', 'c']), ['a', 'b', 'c'])

    def test_consecutive_duplicate_collapsed(self):
        self.assertEqual(_dedup_scheme_names(['a', 'a', 'b']), ['a', 'b'])

    def test_non_consecutive_duplicate_collapsed(self):
        # First occurrence kept, later ones dropped.
        self.assertEqual(
            _dedup_scheme_names(['a', 'b', 'a', 'c', 'b']),
            ['a', 'b', 'c'],
        )

    def test_empty_input(self):
        self.assertEqual(_dedup_scheme_names([]), [])


class TestResolveSuiteInitFinalSchemes(unittest.TestCase):
    """Suite-level ``<init>`` / ``<final>`` schemes resolve to
    ``ResolvedCall`` objects attached to ``SuiteResolution.suite_init_call``
    and ``.suite_final_call`` respectively.  A missing init/final phase
    on the named scheme raises ``CCPPError`` at resolve time."""

    def _build_store(self, scheme_meta_text: str):
        from metadata.metadata_table import _parse_lines
        from metadata.variable_resolver import SchemeStore
        tbls = _parse_lines(
            scheme_meta_text.splitlines(keepends=True), 'sch.meta',
        )
        return SchemeStore.build_from(tbls)

    def _resolve(self, suite_xml: str, scheme_meta_text: str):
        import tempfile, os, logging
        from test_suite_resolver import _load_full_host_dict
        from generator.suite_xml import parse_suite_xml
        from generator.suite_resolver import resolve_suite
        hd = _load_full_host_dict()
        store = self._build_store(scheme_meta_text)
        with tempfile.TemporaryDirectory() as tmp:
            path = os.path.join(tmp, 's.xml')
            with open(path, 'w') as fh:
                fh.write(suite_xml)
            suite = parse_suite_xml(path, tmp, logging.getLogger('t'),
                                    skip_validation=True)
        return resolve_suite(suite, store, hd)

    _SCHEME_META = (
        '[ccpp-table-properties]\n'
        '  name = init_final_test\n'
        '  type = scheme\n'
        '[ccpp-arg-table]\n'
        '  name = init_final_test_init\n'
        '  type = scheme\n'
        '[errmsg]\n'
        '  standard_name = ccpp_error_message\n'
        '  units = none\n'
        '  dimensions = ()\n'
        '  type = character\n'
        '  kind = len=512\n'
        '  intent = out\n'
        '[errflg]\n'
        '  standard_name = ccpp_error_code\n'
        '  units = 1\n'
        '  dimensions = ()\n'
        '  type = integer\n'
        '  intent = out\n'
        '[ccpp-arg-table]\n'
        '  name = init_final_test_final\n'
        '  type = scheme\n'
        '[errmsg]\n'
        '  standard_name = ccpp_error_message\n'
        '  units = none\n'
        '  dimensions = ()\n'
        '  type = character\n'
        '  kind = len=512\n'
        '  intent = out\n'
        '[errflg]\n'
        '  standard_name = ccpp_error_code\n'
        '  units = 1\n'
        '  dimensions = ()\n'
        '  type = integer\n'
        '  intent = out\n'
    )

    def test_init_call_attached(self):
        suite_xml = (
            '<?xml version="1.0" encoding="UTF-8"?>\n'
            '<suite name="s" version="2.0">\n'
            '  <init>init_final_test</init>\n'
            '  <group name="g"></group>\n'
            '</suite>\n'
        )
        suite_resolution = self._resolve(suite_xml, self._SCHEME_META)
        self.assertIsNotNone(suite_resolution.suite_init_call)
        self.assertEqual(suite_resolution.suite_init_call.scheme_name, 'init_final_test')
        self.assertEqual(suite_resolution.suite_init_call.phase, 'init')
        self.assertIsNone(suite_resolution.suite_final_call)

    def test_final_call_attached(self):
        suite_xml = (
            '<?xml version="1.0" encoding="UTF-8"?>\n'
            '<suite name="s" version="2.0">\n'
            '  <group name="g"></group>\n'
            '  <final>init_final_test</final>\n'
            '</suite>\n'
        )
        suite_resolution = self._resolve(suite_xml, self._SCHEME_META)
        self.assertIsNone(suite_resolution.suite_init_call)
        self.assertIsNotNone(suite_resolution.suite_final_call)
        self.assertEqual(suite_resolution.suite_final_call.scheme_name, 'init_final_test')
        self.assertEqual(suite_resolution.suite_final_call.phase, 'final')

    def test_both_attached(self):
        suite_xml = (
            '<?xml version="1.0" encoding="UTF-8"?>\n'
            '<suite name="s" version="2.0">\n'
            '  <init>init_final_test</init>\n'
            '  <group name="g"></group>\n'
            '  <final>init_final_test</final>\n'
            '</suite>\n'
        )
        suite_resolution = self._resolve(suite_xml, self._SCHEME_META)
        self.assertIsNotNone(suite_resolution.suite_init_call)
        self.assertIsNotNone(suite_resolution.suite_final_call)

    def test_init_scheme_without_init_phase_raises(self):
        """If the named scheme has no ``init`` phase in its metadata,
        the resolver errors out with a clear message — silent drop
        would let the SDF declaration go unused at runtime."""
        # Same scheme metadata but with init removed: only ``final``.
        run_only = (
            '[ccpp-table-properties]\n'
            '  name = init_final_test\n'
            '  type = scheme\n'
            '[ccpp-arg-table]\n'
            '  name = init_final_test_run\n'
            '  type = scheme\n'
            '[errmsg]\n'
            '  standard_name = ccpp_error_message\n'
            '  units = none\n'
            '  dimensions = ()\n'
            '  type = character\n'
            '  kind = len=512\n'
            '  intent = out\n'
            '[errflg]\n'
            '  standard_name = ccpp_error_code\n'
            '  units = 1\n'
            '  dimensions = ()\n'
            '  type = integer\n'
            '  intent = out\n'
        )
        suite_xml = (
            '<?xml version="1.0" encoding="UTF-8"?>\n'
            '<suite name="s" version="2.0">\n'
            '  <init>init_final_test</init>\n'
            '  <group name="g"></group>\n'
            '</suite>\n'
        )
        with self.assertRaises(CCPPError) as cm:
            self._resolve(suite_xml, run_only)
        msg = str(cm.exception)
        self.assertIn('init_final_test', msg)
        self.assertIn('init', msg)


class TestResolveSuiteDuplicateScheme(unittest.TestCase):
    """Resolve a suite where one scheme appears twice in the same group.

    Run phase must preserve both call sites (the scheme runs once per
    iteration, e.g. once per constituent); non-run phases must collapse to
    a single call so register/init/final entry points fire exactly once
    per group.  Matches the advection-test pattern where
    ``apply_constituent_tendencies`` is listed twice in ``physics``.
    """

    _SUITE_XML = (
        '<?xml version="1.0" encoding="UTF-8"?>\n'
        '<suite name="dup_scheme" version="1.0">\n'
        '  <group name="physics">\n'
        '    <scheme>temp_calc_adjust</scheme>\n'
        '    <scheme>temp_calc_adjust</scheme>\n'
        '  </group>\n'
        '</suite>\n'
    )

    def _resolve(self):
        hd    = _load_full_host_dict()
        store = _load_scheme_store()
        from generator.suite_xml import parse_suite_xml
        import logging
        logger = logging.getLogger('test')
        with tempfile.TemporaryDirectory() as tmpdir:
            xml_path = os.path.join(tmpdir, 'suite_dup.xml')
            with open(xml_path, 'w') as fh:
                fh.write(self._SUITE_XML)
            suite = parse_suite_xml(xml_path, tmpdir, logger,
                                    skip_validation=True)
        return resolve_suite(suite, store, hd)

    def test_resolve_does_not_raise(self):
        # The pre-fix behaviour was a CCPPError on the second occurrence.
        self.assertIsNotNone(self._resolve())

    def test_run_phase_preserves_both_calls(self):
        suite_resolution = self._resolve()
        run_calls = list(iter_phase_calls(suite_resolution.groups[0].phase_calls['run']))
        self.assertEqual(len(run_calls), 2)
        self.assertEqual(
            [c.scheme_name for c in run_calls],
            ['temp_calc_adjust', 'temp_calc_adjust'],
        )

    def test_init_phase_dedupes(self):
        suite_resolution = self._resolve()
        init_calls = list(iter_phase_calls(suite_resolution.groups[0].phase_calls['init']))
        self.assertEqual(len(init_calls), 1)
        self.assertEqual(init_calls[0].scheme_name, 'temp_calc_adjust')

    def test_final_phase_dedupes(self):
        suite_resolution = self._resolve()
        final_calls = list(iter_phase_calls(suite_resolution.groups[0].phase_calls['final']))
        self.assertEqual(len(final_calls), 1)


########################################################################
# Tests: group cap output
########################################################################

class TestDimDeclLocal(unittest.TestCase):
    """``_dim_decl_local`` — local-name dim resolution with horizontal-chunk
    special case.  The horizontal dim must emit ``lb:ub`` (using the
    host's local names for horizontal_loop_begin / horizontal_loop_end)
    because the temp must match the chunk slice the scheme receives at
    the call site, not the full extent."""

    def setUp(self):
        self.hd = _load_full_host_dict()

    def test_empty(self):
        self.assertEqual(_dim_decl_local([], self.hd), '')

    def test_horizontal_dimension_uses_chunk_bounds(self):
        # control_full.meta has horizontal_loop_begin → 'lb',
        # horizontal_loop_end → 'ub'.
        self.assertEqual(
            _dim_decl_local(['horizontal_dimension'], self.hd),
            ', dimension(lb:ub)',
        )

    def test_vertical_dim_uses_local_name(self):
        # No special case for vertical dims — host's local name only.
        self.assertEqual(
            _dim_decl_local(['vertical_layer_dimension'], self.hd),
            ', dimension(nlev)',
        )

    def test_mixed_horiz_vert(self):
        self.assertEqual(
            _dim_decl_local(
                ['horizontal_dimension', 'vertical_layer_dimension'], self.hd,
            ),
            ', dimension(lb:ub, nlev)',
        )

    def test_unknown_dim_falls_back_to_std_name(self):
        # No entry in host_dict → use the std name verbatim.
        self.assertEqual(
            _dim_decl_local(['some_unknown_dim'], self.hd),
            ', dimension(some_unknown_dim)',
        )


class TestConstituentCountDimSubscript(unittest.TestCase):
    """``number_of_ccpp_constituents`` as a *call subscript* axis.

    Any variable (host, suite-owned, or scheme) may be dimensioned by the
    framework constituent count; ``_one_dim_part`` must emit a whole-axis
    ``:`` for it -- not only framework-constituent args (which go through
    ``_const_dim_part``).  Regression for the CAM-SIMA se_cslam failure where
    host vars (cflx/qbot/fracis) are dimensioned by number_of_ccpp_constituents.
    """

    def test_bare_count_is_whole_axis(self):
        part, used = _one_dim_part('number_of_ccpp_constituents', 'run', {})
        self.assertEqual(part, ':')
        self.assertEqual(used, set())

    def test_explicit_lower_bound_count_is_whole_axis(self):
        part, used = _one_dim_part(
            'ccpp_constant_one:number_of_ccpp_constituents', 'run', {})
        self.assertEqual(part, ':')
        self.assertEqual(used, set())


class TestCollectKindsUsed(unittest.TestCase):
    """``_collect_kinds_used`` must collect only kind *symbols* — integer
    literals and ``len=...`` specifiers are NOT module symbols and must
    not enter the ``use ccpp_kinds, only: ...`` list, even though they
    do flow through to the temp declarations and numeric-literal kind
    suffixes (which is correct Fortran)."""

    def _fake_arg(self, kind_scheme: str = '', kind_host: str = '',
                  temp_name: str = 'foo_l'):
        """Build a minimal ResolvedArg stand-in with the fields
        ``_collect_kinds_used`` reads."""
        from unittest.mock import MagicMock
        a = MagicMock()
        a.temp_name = temp_name
        a.kind_scheme = kind_scheme
        host = MagicMock()
        host.kind = kind_host
        a.host_entry = host
        return a

    def _fake_rg(self, args):
        from unittest.mock import MagicMock
        # ``iter_phase_calls`` does ``isinstance(item, ResolvedCall)`` so a
        # MagicMock won't pass; build a real ResolvedCall (the only field
        # ``_collect_kinds_used`` reads is ``args``).
        resolved_call = ResolvedCall(scheme_name='s', phase='run', args=args)
        resolved_group = MagicMock()
        resolved_group.phase_calls = {'run': [resolved_call]}
        return resolved_group

    def test_keeps_kind_symbols(self):
        args = [self._fake_arg(kind_scheme='kind_phys'),
                self._fake_arg(kind_scheme='kind_dyn')]
        self.assertEqual(_collect_kinds_used(self._fake_rg(args)),
                         ['kind_dyn', 'kind_phys'])

    def test_drops_integer_literal_kinds(self):
        """``kind = 8`` is valid Fortran but not a module symbol — must
        not appear in the USE list."""
        args = [self._fake_arg(kind_scheme='8')]
        self.assertEqual(_collect_kinds_used(self._fake_rg(args)), [])

    def test_drops_character_len_specifiers(self):
        args = [self._fake_arg(kind_scheme='len=512')]
        self.assertEqual(_collect_kinds_used(self._fake_rg(args)), [])

    def test_mixed_set(self):
        args = [self._fake_arg(kind_scheme='kind_phys'),
                self._fake_arg(kind_scheme='8'),
                self._fake_arg(kind_scheme='len=*'),
                self._fake_arg(kind_scheme='kind_phys')]
        self.assertEqual(_collect_kinds_used(self._fake_rg(args)),
                         ['kind_phys'])

    def test_no_temp_name_skipped(self):
        # Args without a transformation temp are irrelevant — no kind
        # parameter is emitted into a declaration for them.
        args = [self._fake_arg(kind_scheme='kind_dyn', temp_name='')]
        self.assertEqual(_collect_kinds_used(self._fake_rg(args)), [])


class TestTransformComment(unittest.TestCase):
    """The trailing inline comment must list every active transform, but
    must suppress "unit conversion" when the rendered formula is the
    identity (formula ``'{var}'`` for dimensionally-equivalent units).
    """

    def _arg(self, **kwargs):
        from unittest.mock import MagicMock
        a = MagicMock()
        a.needs_unit_transform = kwargs.get('needs_unit_transform', False)
        a.needs_kind_transform = kwargs.get('needs_kind_transform', False)
        a.needs_vert_flip      = kwargs.get('needs_vert_flip', False)
        a.unit_forward         = kwargs.get('unit_forward', '')
        a.unit_backward        = kwargs.get('unit_backward', '')
        a.call_expr            = kwargs.get('call_expr', '')
        a.temp_name            = kwargs.get('temp_name', '')
        a.kind_host            = kwargs.get('kind_host', '')
        a.kind_scheme          = kwargs.get('kind_scheme', '')
        return a

    def test_no_transforms_returns_empty(self):
        self.assertEqual(_transform_comment(self._arg()), '')

    def test_identity_forward_suppressed(self):
        """Forward formula returns the call_expr unchanged → no comment."""
        a = self._arg(
            needs_unit_transform=True,
            unit_forward='gt0(lb:ub, 1:nlev)',
            call_expr='gt0(lb:ub, 1:nlev)',
            kind_host='kind_phys', kind_scheme='kind_phys',
        )
        self.assertEqual(_transform_comment(a, reverse=False), '')

    def test_identity_backward_suppressed(self):
        """Backward formula returns the temp_name unchanged → no comment."""
        a = self._arg(
            needs_unit_transform=True,
            unit_backward='foo_l',
            temp_name='foo_l',
            kind_host='kind_phys', kind_scheme='kind_phys',
        )
        self.assertEqual(_transform_comment(a, reverse=True), '')

    def test_non_identity_forward_emitted(self):
        """Forward formula scales the call_expr → comment lists the
        unit conversion."""
        a = self._arg(
            needs_unit_transform=True,
            unit_forward='1.0E-3_kind_phys*gt0(lb:ub)',
            call_expr='gt0(lb:ub)',
            kind_host='kind_phys', kind_scheme='kind_phys',
        )
        self.assertIn('unit conversion', _transform_comment(a, reverse=False))

    def test_non_identity_backward_emitted(self):
        a = self._arg(
            needs_unit_transform=True,
            unit_backward='1.0E+3_kind_phys*foo_l',
            temp_name='foo_l',
            kind_host='kind_phys', kind_scheme='kind_phys',
        )
        self.assertIn('unit conversion', _transform_comment(a, reverse=True))

    def test_vert_flip_alone_emits_flip_only(self):
        """A pure vertical flip (identity unit conversion, no kind change)
        still gets a comment — and it mentions only the flip, not a
        spurious "unit conversion"."""
        a = self._arg(
            needs_vert_flip=True,
            unit_forward='gt0(lb:ub, nlev:1:-1)',
            call_expr='gt0(lb:ub, nlev:1:-1)',
        )
        self.assertEqual(_transform_comment(a, reverse=False),
                         '! vertical flip (top_at_one mismatch)')

    def test_unit_and_flip_both_listed(self):
        a = self._arg(
            needs_unit_transform=True,
            needs_vert_flip=True,
            unit_forward='1.0E-3_kind_phys*gt0(lb:ub, nlev:1:-1)',
            call_expr='gt0(lb:ub, nlev:1:-1)',
            kind_host='kind_phys', kind_scheme='kind_phys',
        )
        comment = _transform_comment(a, reverse=False)
        self.assertIn('unit conversion', comment)
        self.assertIn('vertical flip', comment)


class TestFortranTypeStr(unittest.TestCase):

    def test_real_with_kind(self):
        self.assertEqual(_fortran_type_str('real', 'kind_phys'), 'real(kind=kind_phys)')

    def test_integer(self):
        self.assertEqual(_fortran_type_str('integer', ''), 'integer')

    def test_character_with_len(self):
        self.assertEqual(_fortran_type_str('character', 'len=512'), 'character(len=512)')

    def test_ddt_gets_type_wrap(self):
        self.assertEqual(_fortran_type_str('my_ddt_type', ''), 'type(my_ddt_type)')

    def test_ddt_already_wrapped(self):
        self.assertEqual(_fortran_type_str('type(my_ddt)', ''), 'type(my_ddt)')


class TestGenerateGroupCap(unittest.TestCase):

    def _resolve_and_generate(self):
        hd = _load_full_host_dict()
        store = _load_scheme_store()
        suite = _parse_suite('suite_test_simple.xml')
        suite_resolution = resolve_suite(suite, store, hd)
        resolved_group = suite_resolution.groups[0]
        lines = _generate_group_cap('test_simple', 'physics', resolved_group, hd)
        return lines

    def test_module_header(self):
        lines = self._resolve_and_generate()
        text = '\n'.join(lines)
        self.assertIn('module ccpp_test_simple_physics_cap', text)
        self.assertIn('end module ccpp_test_simple_physics_cap', text)

    def test_header_comment(self):
        lines = self._resolve_and_generate()
        self.assertTrue(lines[0].startswith('!'))

    def test_use_statements_present(self):
        lines = self._resolve_and_generate()
        text = '\n'.join(lines)
        self.assertIn('use host_phys', text)

    def test_implicit_none_private(self):
        lines = self._resolve_and_generate()
        text = '\n'.join(lines)
        self.assertIn('implicit none', text)
        self.assertIn('private', text)

    def test_public_subroutines(self):
        lines = self._resolve_and_generate()
        text = '\n'.join(lines)
        self.assertIn('public :: physics_run', text)

    def test_contains_block(self):
        lines = self._resolve_and_generate()
        self.assertIn('contains', lines)

    def test_run_subroutine(self):
        lines = self._resolve_and_generate()
        text = '\n'.join(lines)
        self.assertIn('subroutine physics_run', text)
        self.assertIn('end subroutine physics_run', text)

    def test_scheme_call_present(self):
        lines = self._resolve_and_generate()
        text = '\n'.join(lines)
        self.assertIn('call temp_calc_adjust_run', text)

    def test_keyword_args_in_call(self):
        lines = self._resolve_and_generate()
        text = '\n'.join(lines)
        # scheme local name 'im' (horizontal_dimension) is synthesised from
        # the loop-bound control vars, not taken directly from host ncols.
        self.assertIn('im=(ub - lb + 1)', text)
        self.assertIn('timestep=dt', text)
        self.assertIn('temp=gt0(lb:ub, 1:nlev)', text)

    def test_errflg_check(self):
        lines = self._resolve_and_generate()
        text = '\n'.join(lines)
        self.assertIn('if (errflg /= 0) return', text)

    def test_init_subroutine(self):
        lines = self._resolve_and_generate()
        text = '\n'.join(lines)
        self.assertIn('subroutine physics_init', text)
        self.assertIn('call temp_calc_adjust_init', text)

    def test_write_group_cap(self):
        """write_group_cap writes the file and returns its path."""
        hd = _load_full_host_dict()
        store = _load_scheme_store()
        suite = _parse_suite('suite_test_simple.xml')
        suite_resolution = resolve_suite(suite, store, hd)
        resolved_group = suite_resolution.groups[0]
        with tempfile.TemporaryDirectory() as tmpdir:
            path = write_group_cap('test_simple', 'physics', resolved_group, hd, tmpdir)
            self.assertTrue(os.path.isfile(path))
            self.assertEqual(os.path.basename(path), 'ccpp_test_simple_physics_cap.F90')
            with open(path) as fh:
                content = fh.read()
            self.assertIn('module ccpp_test_simple_physics_cap', content)
            self.assertIn('call temp_calc_adjust_run', content)


########################################################################
# Tests: subcycle resolution
########################################################################

class TestSubcycleResolution(unittest.TestCase):

    def _resolve_subcycle(self):
        hd    = _load_full_host_dict()
        store = _load_scheme_store()
        suite = _parse_suite('suite_test_subcycle.xml')
        return resolve_suite(suite, store, hd)

    def test_run_phase_has_subcycle(self):
        suite_resolution = self._resolve_subcycle()
        resolved_group = suite_resolution.groups[0]
        run_items = resolved_group.phase_calls['run']
        self.assertEqual(len(run_items), 1)
        self.assertIsInstance(run_items[0], ResolvedSubcycle)

    def test_subcycle_loop_count(self):
        suite_resolution = self._resolve_subcycle()
        sub = suite_resolution.groups[0].phase_calls['run'][0]
        self.assertEqual(sub.loop, '3')

    def test_subcycle_contains_scheme(self):
        suite_resolution = self._resolve_subcycle()
        sub = suite_resolution.groups[0].phase_calls['run'][0]
        self.assertEqual(len(sub.calls), 1)
        self.assertEqual(sub.calls[0].scheme_name, 'temp_calc_adjust')

    def test_init_phase_is_flat(self):
        """Init phase flattens subcycles — no ResolvedSubcycle in init."""
        suite_resolution = self._resolve_subcycle()
        resolved_group = suite_resolution.groups[0]
        for item in resolved_group.phase_calls.get('init', []):
            self.assertNotIsInstance(item, ResolvedSubcycle)

    def test_iter_phase_calls_flattens(self):
        suite_resolution = self._resolve_subcycle()
        resolved_group = suite_resolution.groups[0]
        all_calls = list(iter_phase_calls(resolved_group.phase_calls['run']))
        self.assertEqual(len(all_calls), 1)
        self.assertEqual(all_calls[0].scheme_name, 'temp_calc_adjust')


class TestNestedSubcycleResolution(unittest.TestCase):
    """SDFs may nest ``<subcycle>`` inside ``<subcycle>``.  The resolver
    must preserve the nesting (it determines the effective iteration
    count product: ``outer * inner1 * inner2 * ...``).  Original capgen
    semantics."""

    def _resolve_nested(self, suite_xml: str):
        import tempfile, os, logging
        from test_suite_resolver import (
            _load_full_host_dict, _load_scheme_store,
        )
        from generator.suite_xml import parse_suite_xml
        from generator.suite_resolver import resolve_suite
        hd = _load_full_host_dict()
        store = _load_scheme_store()
        with tempfile.TemporaryDirectory() as tmp:
            xml_path = os.path.join(tmp, 's.xml')
            with open(xml_path, 'w') as fh:
                fh.write(suite_xml)
            suite = parse_suite_xml(xml_path, tmp, logging.getLogger('t'),
                                    skip_validation=True)
        return resolve_suite(suite, store, hd)

    def test_two_deep_nesting_preserved(self):
        suite_xml = (
            '<?xml version="1.0" encoding="UTF-8"?>\n'
            '<suite name="s" version="2.0">\n'
            '  <group name="g">\n'
            '    <subcycle loop="3">\n'
            '      <subcycle loop="2">\n'
            '        <scheme>temp_calc_adjust</scheme>\n'
            '      </subcycle>\n'
            '    </subcycle>\n'
            '  </group>\n'
            '</suite>\n'
        )
        suite_resolution = self._resolve_nested(suite_xml)
        run = suite_resolution.groups[0].phase_calls['run']
        # Outer subcycle.
        self.assertEqual(len(run), 1)
        outer = run[0]
        self.assertIsInstance(outer, ResolvedSubcycle)
        self.assertEqual(outer.loop, '3')
        # Inside the outer is one inner subcycle, NOT a flat scheme list.
        self.assertEqual(len(outer.calls), 1)
        inner = outer.calls[0]
        self.assertIsInstance(inner, ResolvedSubcycle)
        self.assertEqual(inner.loop, '2')
        # And the scheme lives inside the inner subcycle.
        self.assertEqual(len(inner.calls), 1)
        self.assertIsInstance(inner.calls[0], ResolvedCall)
        self.assertEqual(inner.calls[0].scheme_name, 'temp_calc_adjust')

    def test_three_deep_nesting_preserved(self):
        suite_xml = (
            '<?xml version="1.0" encoding="UTF-8"?>\n'
            '<suite name="s" version="2.0">\n'
            '  <group name="g">\n'
            '    <subcycle loop="3">\n'
            '      <subcycle loop="2">\n'
            '        <subcycle loop="2">\n'
            '          <scheme>temp_calc_adjust</scheme>\n'
            '        </subcycle>\n'
            '      </subcycle>\n'
            '    </subcycle>\n'
            '  </group>\n'
            '</suite>\n'
        )
        suite_resolution = self._resolve_nested(suite_xml)
        run = suite_resolution.groups[0].phase_calls['run']
        outer = run[0]
        mid   = outer.calls[0]
        inner = mid.calls[0]
        self.assertEqual([outer.loop, mid.loop, inner.loop], ['3', '2', '2'])
        # Leaf is the scheme call.
        self.assertEqual(len(inner.calls), 1)
        self.assertIsInstance(inner.calls[0], ResolvedCall)

    def test_iter_phase_calls_recurses_through_nesting(self):
        """``iter_phase_calls`` must walk through every nesting level
        and yield the leaf scheme calls — used everywhere a phase needs
        a flat view of its scheme calls (USE collection, etc.)."""
        suite_xml = (
            '<?xml version="1.0" encoding="UTF-8"?>\n'
            '<suite name="s" version="2.0">\n'
            '  <group name="g">\n'
            '    <subcycle loop="3">\n'
            '      <subcycle loop="2">\n'
            '        <scheme>temp_calc_adjust</scheme>\n'
            '      </subcycle>\n'
            '    </subcycle>\n'
            '  </group>\n'
            '</suite>\n'
        )
        suite_resolution = self._resolve_nested(suite_xml)
        run = suite_resolution.groups[0].phase_calls['run']
        calls = list(iter_phase_calls(run))
        # One scheme call, reachable through two subcycle wrappers.
        self.assertEqual(len(calls), 1)
        self.assertEqual(calls[0].scheme_name, 'temp_calc_adjust')


########################################################################
# Tests: _resolve_subcycle_loop_bound
########################################################################

class TestResolveSubcycleLoopBound(unittest.TestCase):
    """Subcycle ``loop=`` attribute resolution.

    ``loop="<integer>"`` passes through verbatim; ``loop="<std_name>"``
    must resolve to the host's local Fortran name (or fail with a clear
    error).  The returned standard name drives USE-statement / dummy-arg
    threading in the group cap.
    """

    def _hd(self):
        from metadata.metadata_table import _parse_lines
        host_src = '''
[ccpp-table-properties]
  name = host_mod
  type = host
[ccpp-arg-table]
  name = host_mod
  type = host
[ ncols ]
  standard_name = horizontal_dimension
  units = count
  dimensions = ()
  type = integer
[ n_sub ]
  standard_name = num_subcycles_for_effr
  units = count
  dimensions = ()
  type = integer
'''
        ctrl_src = '''
[ccpp-table-properties]
  name = ctrl
  type = control
[ccpp-arg-table]
  name = ctrl
  type = control
[ n_ctrl ]
  standard_name = number_of_iterations
  units = count
  dimensions = ()
  type = integer
'''
        return build_flat_host_dict(
            _parse_lines(host_src.splitlines(keepends=True), 'h.meta'),
            _parse_lines(ctrl_src.splitlines(keepends=True), 'c.meta'),
            [],
        )

    def test_none_returns_one(self):
        from generator.suite_resolver import _resolve_subcycle_loop_bound
        self.assertEqual(
            _resolve_subcycle_loop_bound(None, self._hd()),
            ('1', ''),
        )

    def test_empty_string_returns_one(self):
        from generator.suite_resolver import _resolve_subcycle_loop_bound
        self.assertEqual(
            _resolve_subcycle_loop_bound('   ', self._hd()),
            ('1', ''),
        )

    def test_integer_literal_passes_through(self):
        from generator.suite_resolver import _resolve_subcycle_loop_bound
        self.assertEqual(
            _resolve_subcycle_loop_bound('3', self._hd()),
            ('3', ''),
        )
        self.assertEqual(
            _resolve_subcycle_loop_bound('  42  ', self._hd()),
            ('42', ''),
        )

    def test_std_name_resolves_to_host_local(self):
        from generator.suite_resolver import _resolve_subcycle_loop_bound
        local, std = _resolve_subcycle_loop_bound(
            'num_subcycles_for_effr', self._hd(),
        )
        self.assertEqual(local, 'n_sub')
        self.assertEqual(std, 'num_subcycles_for_effr')

    def test_std_name_case_insensitive(self):
        from generator.suite_resolver import _resolve_subcycle_loop_bound
        local, std = _resolve_subcycle_loop_bound(
            'Num_Subcycles_For_Effr', self._hd(),
        )
        self.assertEqual(local, 'n_sub')
        self.assertEqual(std, 'num_subcycles_for_effr')

    def test_std_name_resolves_to_control_local(self):
        from generator.suite_resolver import _resolve_subcycle_loop_bound
        local, std = _resolve_subcycle_loop_bound(
            'number_of_iterations', self._hd(),
        )
        self.assertEqual(local, 'n_ctrl')
        self.assertEqual(std, 'number_of_iterations')

    def test_unresolved_std_name_raises(self):
        from generator.suite_resolver import _resolve_subcycle_loop_bound
        with self.assertRaises(CCPPError) as cm:
            _resolve_subcycle_loop_bound('totally_made_up_name', self._hd())
        msg = str(cm.exception)
        self.assertIn('totally_made_up_name', msg)
        self.assertIn('standard name', msg)

    def test_std_name_resolving_to_ddt_component_uses_access_path(self):
        """When a CCPP standard name resolves to a DDT-component entry
        (i.e. the host declares the variable inside a DDT instance), the
        generated Fortran must use the *full access path*
        (``phys_state%num_subcycles``), not just the bare component name
        (``num_subcycles``).  The bare local_name wouldn't be in scope
        from inside the cap."""
        from metadata.metadata_table import _parse_lines
        from generator.suite_resolver import _resolve_subcycle_loop_bound

        ddt_src = '''
[ccpp-table-properties]
  name = physics_state
  type = ddt
[ccpp-arg-table]
  name = physics_state
  type = ddt
[num_sub]
  standard_name = num_subcycles_for_effr
  units = count
  dimensions = ()
  type = integer
'''
        host_src = '''
[ccpp-table-properties]
  name = test_host_mod
  type = host
[ccpp-arg-table]
  name = test_host_mod
  type = host
[ phys_state ]
  standard_name = physics_state_ddt_instance
  units = ddt
  dimensions = ()
  type = physics_state
'''
        hd = build_flat_host_dict(
            _parse_lines(host_src.splitlines(keepends=True), 'h.meta'),
            [],
            _parse_lines(ddt_src.splitlines(keepends=True), 'd.meta'),
        )
        expr, std = _resolve_subcycle_loop_bound(
            'num_subcycles_for_effr', hd,
        )
        self.assertEqual(expr, 'phys_state%num_sub')
        self.assertEqual(std, 'num_subcycles_for_effr')


class TestSubcycleGroupCapOutput(unittest.TestCase):

    def setUp(self):
        hd     = _load_full_host_dict()
        store  = _load_scheme_store()
        suite  = _parse_suite('suite_test_subcycle.xml')
        suite_resolution     = resolve_suite(suite, store, hd)
        resolved_group     = suite_resolution.groups[0]
        self.lines = _generate_group_cap('test_subcycle', 'physics', resolved_group, hd)
        self.text  = '\n'.join(self.lines)

    def test_do_loop_present(self):
        self.assertIn('do ccpp_loop_counter = 1, 3', self.text)

    def test_end_do_present(self):
        self.assertIn('end do', self.text)

    def test_loop_counter_declared(self):
        self.assertIn('integer :: ccpp_loop_counter', self.text)

    def test_scheme_call_inside_loop(self):
        loop_start = self.text.index('do ccpp_loop_counter = 1, 3')
        loop_end   = self.text.index('end do')
        loop_body  = self.text[loop_start:loop_end]
        self.assertIn('call temp_calc_adjust_run', loop_body)

    def test_init_not_in_do_loop(self):
        """Init phase is flat — no do loop."""
        self.assertNotIn('do ccpp_loop_counter', self.text.split('subroutine physics_init')[1].split('end subroutine')[0])


########################################################################
# Tests: state machine in group cap
########################################################################

class TestStateMachineGroupCap(unittest.TestCase):

    def setUp(self):
        hd    = _load_full_host_dict()
        store = _load_scheme_store()
        suite = _parse_suite('suite_test_simple.xml')
        suite_resolution    = resolve_suite(suite, store, hd)
        resolved_group    = suite_resolution.groups[0]
        self.lines = _generate_group_cap('test_simple', 'physics', resolved_group, hd)
        self.text  = '\n'.join(self.lines)

    def test_state_constants_declared(self):
        self.assertIn('CCPP_GROUP_UNINITIALIZED = 0', self.text)
        self.assertIn('CCPP_GROUP_INITIALIZED   = 1', self.text)
        self.assertIn('CCPP_GROUP_IN_TIMESTEP   = 2', self.text)

    def test_state_array_declared(self):
        self.assertIn('integer, private, allocatable :: ccpp_group_state(:)', self.text)

    def test_state_alloc_public(self):
        self.assertIn('public :: physics_state_alloc', self.text)

    def test_state_dealloc_public(self):
        self.assertIn('public :: physics_state_dealloc', self.text)

    def test_init_idempotent_skip(self):
        # init returns silently when already INITIALIZED.
        self.assertIn(
            'if (ccpp_group_state(inst_num) == CCPP_GROUP_INITIALIZED) return',
            self.text,
        )

    def test_init_errors_on_invalid_state(self):
        # init must error if the state is anything other than UNINITIALIZED
        # or INITIALIZED (idempotent skip).
        init_sub = self.text.split('subroutine physics_init')[1]
        init_sub = init_sub.split('end subroutine')[0]
        self.assertIn(
            'ccpp_group_state(inst_num) /= CCPP_GROUP_UNINITIALIZED', init_sub
        )
        self.assertIn('errflg = 1', init_sub)

    def test_init_sets_state(self):
        self.assertIn('ccpp_group_state(inst_num) = CCPP_GROUP_INITIALIZED', self.text)

    def test_final_resets_state(self):
        self.assertIn('ccpp_group_state(inst_num) = CCPP_GROUP_UNINITIALIZED', self.text)

    def test_run_guards_on_in_timestep(self):
        # run requires IN_TIMESTEP; otherwise sets errflg and returns.
        run_sub = self.text.split('subroutine physics_run')[1]
        run_sub = run_sub.split('end subroutine')[0]
        self.assertIn(
            'ccpp_group_state(inst_num) /= CCPP_GROUP_IN_TIMESTEP', run_sub
        )
        self.assertIn('errflg = 1', run_sub)

    def test_state_alloc_subroutine(self):
        # state_alloc always takes number_of_instances as explicit arg.
        self.assertIn(
            'subroutine physics_state_alloc(number_of_instances, errmsg, errflg)',
            self.text,
        )
        self.assertIn('allocate(ccpp_group_state(number_of_instances))', self.text)

    def test_ninstances_not_used_in_group_cap(self):
        # number_of_instances is no longer USEd by the group cap module;
        # it is passed as an explicit argument to state_alloc instead.
        preamble = self.text.split('contains')[0]
        self.assertNotIn('ninstances', preamble)

    def test_state_dealloc_subroutine(self):
        self.assertIn('subroutine physics_state_dealloc(errmsg, errflg)', self.text)
        self.assertIn('if (allocated(ccpp_group_state)) deallocate(ccpp_group_state)', self.text)

    def test_inst_num_in_init_args(self):
        # inst_num (the local name for instance_number) must be a dummy arg of init.
        init_sub = self.text.split('subroutine physics_init')[1]
        init_sub = init_sub.split('end subroutine')[0]
        self.assertIn('inst_num', init_sub)

    def test_inst_num_in_final_args(self):
        final_sub = self.text.split('subroutine physics_final')[1]
        final_sub = final_sub.split('end subroutine')[0]
        self.assertIn('inst_num', final_sub)


########################################################################
# Tests: suite cap calls state_alloc/dealloc
########################################################################

class TestSuiteCapStateCalls(unittest.TestCase):
    """Suite cap calls state_alloc/dealloc — multi-instance host (host_full.meta)."""

    def setUp(self):
        from generator.suite_cap import _generate_suite_cap
        hd    = _load_full_host_dict()
        store = _load_scheme_store()
        suite = _parse_suite('suite_test_simple.xml')
        suite_resolution    = resolve_suite(suite, store, hd)
        # Pass host_dict so number_of_instances flows through.
        lines = _generate_suite_cap('test_simple', suite_resolution, store, hd)
        self.text = '\n'.join(lines)

    def test_init_calls_state_alloc_with_ninstances(self):
        # host_full.meta has ninstances → number_of_instances.
        self.assertIn(
            'call physics_state_alloc(ninstances, errmsg, errflg)', self.text
        )

    def test_init_subroutine_has_ninstances_arg(self):
        init_sub = self.text.split('subroutine test_simple_init')[1].split('end subroutine')[0]
        self.assertIn('ninstances', init_sub)

    def test_final_calls_state_dealloc(self):
        self.assertIn(
            'call physics_state_dealloc(errmsg, errflg)', self.text
        )

    def test_state_alloc_imported_in_suite_cap(self):
        self.assertIn('physics_state_alloc', self.text.split('contains')[0])

    def test_state_dealloc_imported_in_suite_cap(self):
        self.assertIn('physics_state_dealloc', self.text.split('contains')[0])


class TestSuiteCapStateCallsSingleInstance(unittest.TestCase):
    """Suite cap falls back to passing literal 1 when host has no number_of_instances."""

    def setUp(self):
        from generator.suite_cap import _generate_suite_cap
        # Use full host_dict but remove number_of_instances to simulate a
        # single-instance host model.
        hd_full = _load_full_host_dict()
        hd = {k: v for k, v in hd_full.items() if k != 'number_of_instances'}
        store = _load_scheme_store()
        suite = _parse_suite('suite_test_simple.xml')
        suite_resolution    = resolve_suite(suite, store, hd)
        lines = _generate_suite_cap('test_simple', suite_resolution, store, hd)
        self.text = '\n'.join(lines)

    def test_init_calls_state_alloc_with_literal_1(self):
        self.assertIn(
            'call physics_state_alloc(1, errmsg, errflg)', self.text
        )

    def test_init_subroutine_has_no_ninstances_arg(self):
        init_sub = self.text.split('subroutine test_simple_init')[1].split('end subroutine')[0]
        self.assertNotIn('number_of_instances', init_sub)


########################################################################
# Register-phase + suite-owned scalar dimension flow
########################################################################

def _load_register_dim_scheme_store():
    """Load the register_dim_producer + register_dim_consumer schemes."""
    tables = []
    tables.extend(parse_metadata_file(_sf('scheme_register_dim_producer.meta')))
    tables.extend(parse_metadata_file(_sf('scheme_register_dim_consumer.meta')))
    return SchemeStore.build_from(tables)


class TestRegisterPhaseSuiteOwnedDim(unittest.TestCase):
    """A scheme writes a suite-owned scalar dimension during ``_register``;
    a later scheme uses it as a dimension for an interstitial array."""

    def setUp(self):
        self.hd     = _load_full_host_dict()
        self.store  = _load_register_dim_scheme_store()
        self.suite  = _parse_suite('suite_register_dim.xml')
        self.suite_resolution     = resolve_suite(self.suite, self.store, self.hd)

    def test_dim_inter_promoted_to_suite_var(self):
        # The register-phase intent=out arg becomes a suite-owned variable.
        self.assertIn(
            'dimension_for_interstitial_variable', self.suite_resolution.suite_vars,
        )
        suite_var = self.suite_resolution.suite_vars['dimension_for_interstitial_variable']
        self.assertEqual(suite_var.type_, 'integer')
        self.assertEqual(suite_var.dimensions, [])
        self.assertEqual(suite_var.source_phase, 'register')

    def test_interstitial_var_promoted_to_suite_var(self):
        # The run-phase intent=out array also becomes a suite var, dimensioned
        # by the register-set scalar.
        self.assertIn(
            'output_only_interstitial_variable', self.suite_resolution.suite_vars,
        )
        suite_var = self.suite_resolution.suite_vars['output_only_interstitial_variable']
        self.assertEqual(suite_var.dimensions, ['dimension_for_interstitial_variable'])

    def test_register_phase_call_resolved(self):
        # Group's register phase has a ResolvedCall for the producer scheme.
        resolved_group = self.suite_resolution.groups[0]
        register_calls = list(iter_phase_calls(resolved_group.phase_calls.get('register', [])))
        self.assertEqual(len(register_calls), 1)
        self.assertEqual(register_calls[0].scheme_name, 'register_dim_producer')

    def test_run_phase_dim_resolves_via_suite_var(self):
        # The run-phase consumer call's interstitial_var arg's call_expr
        # must reference ccpp_suite_data(...)%dim_inter as the upper bound.
        resolved_group = self.suite_resolution.groups[0]
        run_calls = list(iter_phase_calls(resolved_group.phase_calls.get('run', [])))
        consumer = next(resolved_call for resolved_call in run_calls
                        if resolved_call.scheme_name == 'register_dim_consumer')
        inter_arg = next(a for a in consumer.args
                         if a.standard_name == 'output_only_interstitial_variable')
        self.assertIn('1:ccpp_suite_data', inter_arg.subscript)
        self.assertIn('dim_inter', inter_arg.subscript)


class TestRegisterPhaseSuiteCapEmission(unittest.TestCase):
    """Suite cap emits the register-phase scheme call inside <suite>_register."""

    def setUp(self):
        from generator.suite_cap import _generate_suite_cap
        self.hd    = _load_full_host_dict()
        self.store = _load_register_dim_scheme_store()
        self.suite = _parse_suite('suite_register_dim.xml')
        self.suite_resolution    = resolve_suite(self.suite, self.store, self.hd)
        self.text  = '\n'.join(
            _generate_suite_cap('reg_dim', self.suite_resolution, self.store, self.hd)
        )

    def test_register_subroutine_emits_scheme_call(self):
        register_body = self.text.split('subroutine reg_dim_register')[1].split(
            'end subroutine reg_dim_register'
        )[0]
        self.assertIn('call register_dim_producer_register', register_body)

    def test_register_subroutine_uses_scheme_module(self):
        # USE statement is required so the suite cap can reference _register.
        self.assertIn('use register_dim_producer', self.text)

    def test_register_subroutine_uses_suite_data(self):
        # Suite-owned vars (dim_inter) are accessed through ccpp_<suite>_data.
        self.assertIn('use ccpp_reg_dim_data', self.text)

    def test_state_transitions_to_registered(self):
        register_body = self.text.split('subroutine reg_dim_register')[1].split(
            'end subroutine reg_dim_register'
        )[0]
        self.assertIn(
            'ccpp_suite_state(inst_num) = CCPP_SUITE_REGISTERED',
            register_body,
        )


########################################################################
# Constituent registration (opt-in via type=host)
########################################################################

def _load_constituent_host_dict():
    """Load host_with_constituents.meta + control_full.meta + the framework
    constituent DDT metadata into a host dict."""
    host_tbls = parse_metadata_file(_sf('host_with_constituents.meta'))
    ctrl_tbls = parse_metadata_file(_sf('control_full.meta'))
    # Pull in the framework's DDT definitions for ccpp_constituent_properties_t
    # / ccpp_model_constituents_t so the host's ccpp_model_constituents_object
    # resolves cleanly.  These are passed as DDT tables, not host tables.
    ddt_tbls = []
    fw_meta = os.path.join(
        os.path.dirname(os.path.dirname(__file__)),
        'capgen', 'src', 'ccpp_constituent_prop_mod.meta',
    )
    if os.path.isfile(fw_meta):
        ddt_tbls = parse_metadata_file(fw_meta)
    return build_flat_host_dict(host_tbls, ctrl_tbls, ddt_tbls)


def _load_constituent_scheme_store():
    tables = parse_metadata_file(_sf('scheme_register_constituents.meta'))
    return SchemeStore.build_from(tables)


class TestRegisterConstituentsResolver(unittest.TestCase):
    """Resolver detects intent=out ccpp_constituent_properties_t register args
    and records them in suite_res.constituent_register_calls without promoting
    to suite_vars."""

    def setUp(self):
        self.hd    = _load_constituent_host_dict()
        self.store = _load_constituent_scheme_store()
        self.suite = _parse_suite('suite_register_constituents.xml')
        self.suite_resolution    = resolve_suite(self.suite, self.store, self.hd)

    def test_constituent_register_calls_recorded(self):
        self.assertEqual(
            self.suite_resolution.constituent_register_calls,
            [('register_constituents', 'dyn_const')],
        )

    def test_constituent_arg_not_promoted_to_suite_var(self):
        # The constituent array is per-scheme transient — never a SuiteVar.
        self.assertNotIn(
            'dynamic_constituents_for_register_test', self.suite_resolution.suite_vars,
        )

    def test_constituent_arg_marked(self):
        resolved_group = self.suite_resolution.groups[0]
        register_call = list(iter_phase_calls(resolved_group.phase_calls['register']))[0]
        const_arg = next(a for a in register_call.args if a.is_constituent_arg)
        self.assertEqual(const_arg.scheme_local_name, 'dyn_const')
        self.assertEqual(const_arg.call_expr, 'scheme_consts')


class TestRegisterConstituentsNoHostObjectRequired(unittest.TestCase):
    """Under option A the constituent object is generator-owned, so the host
    is NOT required to declare ``ccpp_model_constituents_object`` — a suite
    that registers constituents resolves cleanly against a regular host."""

    def test_missing_host_object_does_not_raise(self):
        hd    = _load_full_host_dict()
        store = _load_constituent_scheme_store()
        suite = _parse_suite('suite_register_constituents.xml')
        suite_resolution = resolve_suite(suite, store, hd)
        # The register-phase scheme is still recorded for the suite cap to
        # populate the per-suite dynamic-constituent buffer.
        self.assertEqual(
            suite_resolution.constituent_register_calls,
            [('register_constituents', 'dyn_const')],
        )


class TestRegisterConstituentsSuiteCap(unittest.TestCase):
    """Under option A the suite cap packs each register-phase scheme's
    constituent array into the per-suite ``<suite>_dynamic_constituents``
    buffer (owned by ccpp_host_constituents).  The actual merge into the
    host-wide constituent object happens later in
    ``ccpp_register_constituents`` — NOT in the suite cap.
    """

    def setUp(self):
        from generator.suite_cap import _generate_suite_cap
        self.hd    = _load_constituent_host_dict()
        self.store = _load_constituent_scheme_store()
        self.suite = _parse_suite('suite_register_constituents.xml')
        self.suite_resolution    = resolve_suite(self.suite, self.store, self.hd)
        self.text  = '\n'.join(
            _generate_suite_cap('reg_consts', self.suite_resolution, self.store, self.hd)
        )

    def test_uses_constituent_prop_type(self):
        # Only the property type is USE'd; the model_constituents_t DDT is
        # owned by ccpp_host_constituents.
        self.assertIn(
            'use ccpp_constituent_prop_mod, only: ccpp_constituent_properties_t',
            self.text,
        )

    def test_uses_per_suite_buffer(self):
        # The per-suite dynamic-constituent buffer is imported from the
        # host_constituents module.
        self.assertIn(
            'use ccpp_host_constituents, only: reg_consts_dynamic_constituents',
            self.text,
        )

    def test_no_host_object_referenced(self):
        # Suite cap no longer references the host-wide constituent object
        # directly; that's the host_constituents module's job.
        self.assertNotIn('host_consts_obj', self.text)
        self.assertNotIn('%initialize_table', self.text)
        self.assertNotIn('%lock_table', self.text)
        self.assertNotIn('%new_field', self.text)

    def test_register_called_once_per_scheme(self):
        register_body = self.text.split('subroutine reg_consts_register')[1].split(
            'end subroutine reg_consts_register'
        )[0]
        # Single-pass append: each constituent scheme's _register is called
        # EXACTLY ONCE (the old count+copy two-pass called it twice and broke
        # non-idempotent schemes such as prescribed_aerosols_register).
        self.assertEqual(
            register_body.count('call register_constituents_register'), 1,
        )
        self.assertNotIn('First pass', register_body)
        self.assertNotIn('Second pass', register_body)

    def test_buffer_allocate(self):
        # Outer wrapper-DDT array is sized to number_of_instances on first
        # call; each instance starts its own slot empty (``%items(0)``) and
        # appends each scheme's constituents.
        self.assertIn(
            'allocate(reg_consts_dynamic_constituents(',
            self.text,
        )
        self.assertIn('%items(0))', self.text)

    def test_buffer_append(self):
        # Each scheme's returned array is appended to the per-instance slot.
        self.assertIn(
            '%items = [reg_consts_dynamic_constituents(inst_num)%items, scheme_consts]',
            self.text,
        )

    def test_final_tears_down_per_suite_buffer(self):
        # The per-suite dynamic-constituent buffer is owned by the
        # suite-cap lifecycle: filled in <suite>_register, torn down in
        # <suite>_final's last-to-leave block.  ccpp_deallocate_dynamic_
        # constituents must NOT touch it (would break re-register).
        final_body = self.text.split('subroutine reg_consts_final')[1].split(
            'end subroutine reg_consts_final'
        )[0]
        self.assertIn(
            'use ccpp_host_constituents, only: reg_consts_dynamic_constituents',
            final_body,
        )
        self.assertIn('if (all(ccpp_suite_state == CCPP_SUITE_UNREGISTERED))',
                      final_body)
        self.assertIn(
            'if (allocated(reg_consts_dynamic_constituents)) '
            'deallocate(reg_consts_dynamic_constituents)',
            final_body,
        )

    def test_scheme_consts_temp_declared(self):
        # Local temporary in the register subroutine.
        register_body = self.text.split('subroutine reg_consts_register')[1].split(
            'end subroutine reg_consts_register'
        )[0]
        self.assertIn(
            'type(ccpp_constituent_properties_t), allocatable :: scheme_consts(:)',
            register_body,
        )


########################################################################
# Provider gate: a constituent-flagged consumer that is actually
# provided by an earlier scheme's intent=out output is an interstitial,
# not a constituent (mirrors original-capgen find_variable).
########################################################################

_PROVIDER_GATE_SCHEMES = '''
[ccpp-table-properties]
  name = make_dry
  type = scheme
[ccpp-arg-table]
  name = make_dry_run
  type = scheme
[ ncol ]
  standard_name = horizontal_dimension
  units = count
  dimensions = ()
  type = integer
  intent = in
[ nz ]
  standard_name = vertical_layer_dimension
  units = count
  dimensions = ()
  type = integer
  intent = in
[ qv_dry ]
  standard_name = water_vapor_mixing_ratio_wrt_dry_air
  units = kg kg-1
  dimensions = (horizontal_dimension, vertical_layer_dimension)
  type = real | kind = kind_phys
  intent = out
[ errmsg ]
  standard_name = ccpp_error_message
  units = none
  dimensions = ()
  type = character
  kind = len=512
  intent = out
[ errflg ]
  standard_name = ccpp_error_code
  units = 1
  dimensions = ()
  type = integer
  intent = out

[ccpp-table-properties]
  name = use_dry
  type = scheme
[ccpp-arg-table]
  name = use_dry_run
  type = scheme
[ ncol ]
  standard_name = horizontal_dimension
  units = count
  dimensions = ()
  type = integer
  intent = in
[ nz ]
  standard_name = vertical_layer_dimension
  units = count
  dimensions = ()
  type = integer
  intent = in
[ qv ]
  standard_name = water_vapor_mixing_ratio_wrt_moist_air_and_condensed_water
  advected = .true.
  units = kg kg-1
  dimensions = (horizontal_dimension, vertical_layer_dimension)
  type = real | kind = kind_phys
  intent = in
[ qv_dry ]
  standard_name = water_vapor_mixing_ratio_wrt_dry_air
  advected = .true.
  units = kg kg-1
  dimensions = (horizontal_dimension, vertical_layer_dimension)
  type = real | kind = kind_phys
  intent = inout
[ errmsg ]
  standard_name = ccpp_error_message
  units = none
  dimensions = ()
  type = character
  kind = len=512
  intent = out
[ errflg ]
  standard_name = ccpp_error_code
  units = 1
  dimensions = ()
  type = integer
  intent = out
'''

_PROVIDER_GATE_SUITE = (
    '<?xml version="1.0"?>\n'
    '<suite name="provgate" version="1.0">\n'
    '  <group name="phys">\n'
    '    <scheme>make_dry</scheme>\n'
    '    <scheme>use_dry</scheme>\n'
    '  </group>\n'
    '</suite>\n'
)


class TestConstituentProviderGate(unittest.TestCase):
    """A constituent-FLAGGED consumer arg whose standard name is produced by
    an earlier scheme (intent=out, unflagged) must resolve as an ordinary
    suite variable -- shared with the producer -- NOT as a constituent
    column, and must not be registered as a constituent.  A constituent
    with no provider stays a constituent.  Regression for the cam-sima
    kessler dry/wet mixing-ratio over-registration."""

    @classmethod
    def setUpClass(cls):
        import logging
        from generator.suite_xml import parse_suite_xml
        hd = _load_constituent_host_dict()
        store = SchemeStore.build_from(_parse(_PROVIDER_GATE_SCHEMES))
        with tempfile.TemporaryDirectory() as tmp:
            sx = os.path.join(tmp, 'suite_provgate.xml')
            with open(sx, 'w') as fh:
                fh.write(_PROVIDER_GATE_SUITE)
            suite = parse_suite_xml(sx, tmp, logging.getLogger('test'),
                                    skip_validation=True)
            cls.sr = resolve_suite(suite, store, hd)
        run_calls = list(iter_phase_calls(cls.sr.groups[0].phase_calls['run']))
        cls.producer = {a.scheme_local_name: a for a in run_calls[0].args}
        cls.consumer = {a.scheme_local_name: a for a in run_calls[1].args}

    def test_producer_output_is_suite_var(self):
        self.assertEqual(self.producer['qv_dry'].source, 'suite')

    def test_provided_consumer_resolves_as_suite_not_constituent(self):
        # qv_dry is flagged advected on the consumer, but make_dry provides
        # it -> must be the shared suite var, not a constituent column.
        qv_dry = self.consumer['qv_dry']
        self.assertEqual(qv_dry.source, 'suite')
        self.assertNotIn('vars_layer', qv_dry.call_expr)

    def test_unprovided_constituent_stays_constituent(self):
        qv = self.consumer['qv']
        self.assertEqual(qv.source, 'constituent')
        self.assertIn('vars_layer', qv.call_expr)

    def test_index_names_exclude_provided_dry_var(self):
        self.assertEqual(
            self.sr.constituent_index_names,
            ['water_vapor_mixing_ratio_wrt_moist_air_and_condensed_water'],
        )
        self.assertNotIn(
            'water_vapor_mixing_ratio_wrt_dry_air',
            self.sr.constituent_index_names,
        )


########################################################################
# index_of_* disambiguation: a scheme-produced index (cross-phase) is a
# suite var, NOT a constituent index (mirrors rrtmgp band indices).
########################################################################

_INDEX_OF_SCHEMES = '''
[ccpp-table-properties]
  name = band_setup
  type = scheme
[ccpp-arg-table]
  name = band_setup_init
  type = scheme
[ idx_sw ]
  standard_name = index_of_shortwave_band
  units = index
  dimensions = ()
  type = integer
  intent = out
[ errmsg ]
  standard_name = ccpp_error_message
  units = none
  dimensions = ()
  type = character
  kind = len=512
  intent = out
[ errflg ]
  standard_name = ccpp_error_code
  units = 1
  dimensions = ()
  type = integer
  intent = out

[ccpp-table-properties]
  name = band_user
  type = scheme
[ccpp-arg-table]
  name = band_user_run
  type = scheme
[ idx_sw ]
  standard_name = index_of_shortwave_band
  units = index
  dimensions = ()
  type = integer
  intent = in
[ idx_const ]
  standard_name = index_of_test_constituent
  units = index
  dimensions = ()
  type = integer
  intent = in
[ errmsg ]
  standard_name = ccpp_error_message
  units = none
  dimensions = ()
  type = character
  kind = len=512
  intent = out
[ errflg ]
  standard_name = ccpp_error_code
  units = 1
  dimensions = ()
  type = integer
  intent = out
'''

# Consumer scheme deliberately listed BEFORE the producer in the SDF, to
# prove resolution is phase-driven (init before run) and NOT dependent on
# scheme text order -- exactly the rrtmgp inputs_setup / sw_cloud_optics
# arrangement.
_INDEX_OF_SUITE = (
    '<?xml version="1.0"?>\n'
    '<suite name="bandtest" version="1.0">\n'
    '  <group name="phys">\n'
    '    <scheme>band_user</scheme>\n'
    '    <scheme>band_setup</scheme>\n'
    '  </group>\n'
    '</suite>\n'
)


class TestIndexOfSchemeProducedVsConstituent(unittest.TestCase):
    """``index_of_<X>`` produced by a scheme (intent=out, here in the init
    phase) is an ordinary suite var; a later-phase consumer (run) resolves
    it from suite_vars as ``source='suite'``.  A genuine ``index_of_<X>``
    that no scheme produces stays a constituent index.  Regression for the
    cam-sima rrtmgp ``index_of_shortwave_band`` 'missing host variable'
    failure."""

    @classmethod
    def setUpClass(cls):
        import logging
        from generator.suite_xml import parse_suite_xml
        hd = _load_constituent_host_dict()
        store = SchemeStore.build_from(_parse(_INDEX_OF_SCHEMES))
        with tempfile.TemporaryDirectory() as tmp:
            sx = os.path.join(tmp, 'suite_bandtest.xml')
            with open(sx, 'w') as fh:
                fh.write(_INDEX_OF_SUITE)
            suite = parse_suite_xml(sx, tmp, logging.getLogger('test'),
                                    skip_validation=True)
            cls.sr = resolve_suite(suite, store, hd)
        init_calls = list(iter_phase_calls(cls.sr.groups[0].phase_calls['init']))
        run_calls = list(iter_phase_calls(cls.sr.groups[0].phase_calls['run']))
        cls.producer = {a.scheme_local_name: a for a in init_calls[0].args}
        cls.consumer = {a.scheme_local_name: a for a in run_calls[0].args}

    def test_init_producer_is_suite_var(self):
        # The init-phase intent=out index is a regular suite var, not a
        # constituent index written by a scheme.
        idx = self.producer['idx_sw']
        self.assertEqual(idx.source, 'suite')

    def test_run_consumer_resolves_from_suite_vars(self):
        # Cross-phase: produced in init, consumed in run -> source='suite'.
        idx = self.consumer['idx_sw']
        self.assertEqual(idx.source, 'suite')
        self.assertNotEqual(idx.source, 'constituent')

    def test_genuine_constituent_index_unchanged(self):
        # index_of_test_constituent is produced by no scheme -> still a
        # constituent index.
        idx = self.consumer['idx_const']
        self.assertEqual(idx.source, 'constituent')

    def test_band_index_not_registered_as_constituent(self):
        self.assertNotIn('shortwave_band', ' '.join(self.sr.constituent_index_names))
        self.assertIn('test_constituent', self.sr.constituent_index_names)


########################################################################
# Cross-group cross-phase provision: a variable produced by a LATER
# group's init phase must be visible to an EARLIER group's run phase,
# because at runtime all groups' init complete before any group's run.
########################################################################

_XGROUP_SCHEMES = '''
[ccpp-table-properties]
  name = consumer_a
  type = scheme
[ccpp-arg-table]
  name = consumer_a_run
  type = scheme
[ val ]
  standard_name = some_setup_value
  units = 1
  dimensions = ()
  type = real | kind = kind_phys
  intent = in
[ errmsg ]
  standard_name = ccpp_error_message
  units = none
  dimensions = ()
  type = character
  kind = len=512
  intent = out
[ errflg ]
  standard_name = ccpp_error_code
  units = 1
  dimensions = ()
  type = integer
  intent = out

[ccpp-table-properties]
  name = producer_b
  type = scheme
[ccpp-arg-table]
  name = producer_b_init
  type = scheme
[ val ]
  standard_name = some_setup_value
  units = 1
  dimensions = ()
  type = real | kind = kind_phys
  intent = out
[ errmsg ]
  standard_name = ccpp_error_message
  units = none
  dimensions = ()
  type = character
  kind = len=512
  intent = out
[ errflg ]
  standard_name = ccpp_error_code
  units = 1
  dimensions = ()
  type = integer
  intent = out
'''

# The CONSUMER's group is listed FIRST and the PRODUCER's group SECOND,
# and the producer provides in the init phase while the consumer reads in
# the run phase.  At runtime every group's init precedes every group's
# run, so this is valid; the resolver must reflect that (phase-major).
_XGROUP_SUITE = (
    '<?xml version="1.0"?>\n'
    '<suite name="xgroup" version="1.0">\n'
    '  <group name="grpA">\n'
    '    <scheme>consumer_a</scheme>\n'
    '  </group>\n'
    '  <group name="grpB">\n'
    '    <scheme>producer_b</scheme>\n'
    '  </group>\n'
    '</suite>\n'
)


class TestCrossGroupCrossPhaseProvision(unittest.TestCase):
    """A variable produced by a later group's init phase is visible to an
    earlier group's run phase (all inits precede all runs at runtime).
    Resolution is phase-major, group-minor.  Regression for the group-major
    nesting that raised 'not provided by any prior scheme'."""

    @classmethod
    def setUpClass(cls):
        import logging
        from generator.suite_xml import parse_suite_xml
        hd = _load_constituent_host_dict()
        store = SchemeStore.build_from(_parse(_XGROUP_SCHEMES))
        with tempfile.TemporaryDirectory() as tmp:
            sx = os.path.join(tmp, 'suite_xgroup.xml')
            with open(sx, 'w') as fh:
                fh.write(_XGROUP_SUITE)
            suite = parse_suite_xml(sx, tmp, logging.getLogger('test'),
                                    skip_validation=True)
            cls.sr = resolve_suite(suite, store, hd)

    def test_consumer_resolves_producer_from_later_group(self):
        grpA = self.sr.groups[0]
        run_calls = list(iter_phase_calls(grpA.phase_calls['run']))
        val = {a.scheme_local_name: a for a in run_calls[0].args}['val']
        self.assertEqual(val.source, 'suite')

    def test_producer_is_suite_var(self):
        grpB = self.sr.groups[1]
        init_calls = list(iter_phase_calls(grpB.phase_calls['init']))
        val = {a.scheme_local_name: a for a in init_calls[0].args}['val']
        self.assertEqual(val.source, 'suite')


########################################################################
# Suite-var field-name uniqueness: two distinct suite vars whose
# producing schemes share a local name must get distinct suite_data
# component names (rrtmgp lw/sw `kdist`, `hrate`).
########################################################################

_SHARED_LOCALNAME_SCHEMES = '''
[ccpp-table-properties]
  name = prod_lw
  type = scheme
[ccpp-arg-table]
  name = prod_lw_run
  type = scheme
[ obj ]
  standard_name = longwave_optics_object
  units = none
  dimensions = ()
  type = real | kind = kind_phys
  intent = out
[ errflg ]
  standard_name = ccpp_error_code
  units = 1
  dimensions = ()
  type = integer
  intent = out

[ccpp-table-properties]
  name = prod_sw
  type = scheme
[ccpp-arg-table]
  name = prod_sw_run
  type = scheme
[ obj ]
  standard_name = shortwave_optics_object
  units = none
  dimensions = ()
  type = real | kind = kind_phys
  intent = out
[ errflg ]
  standard_name = ccpp_error_code
  units = 1
  dimensions = ()
  type = integer
  intent = out
'''

_SHARED_LOCALNAME_SUITE = (
    '<?xml version="1.0"?>\n'
    '<suite name="sharedln" version="1.0">\n'
    '  <group name="phys">\n'
    '    <scheme>prod_lw</scheme>\n'
    '    <scheme>prod_sw</scheme>\n'
    '  </group>\n'
    '</suite>\n'
)


class TestSuiteVarFieldNameUniqueness(unittest.TestCase):
    """Two distinct suite-owned vars (different std names) first produced by
    scheme args sharing a local name must get UNIQUE suite_data field names,
    else the generated DDT has a duplicate component.  Regression for the
    rrtmgp lw/sw ``kdist`` / ``hrate`` collision."""

    @classmethod
    def setUpClass(cls):
        import logging
        from generator.suite_xml import parse_suite_xml
        from generator.suite_data import _generate_suite_data
        hd = _load_constituent_host_dict()
        store = SchemeStore.build_from(_parse(_SHARED_LOCALNAME_SCHEMES))
        with tempfile.TemporaryDirectory() as tmp:
            sx = os.path.join(tmp, 'suite_sharedln.xml')
            with open(sx, 'w') as fh:
                fh.write(_SHARED_LOCALNAME_SUITE)
            suite = parse_suite_xml(sx, tmp, logging.getLogger('test'),
                                    skip_validation=True)
            cls.sr = resolve_suite(suite, store, hd)
        run_calls = list(iter_phase_calls(cls.sr.groups[0].phase_calls['run']))
        cls.svars = cls.sr.suite_vars
        cls.data_lines = _generate_suite_data('sharedln', cls.svars)

    def test_both_suite_vars_present(self):
        self.assertIn('longwave_optics_object', self.svars)
        self.assertIn('shortwave_optics_object', self.svars)

    def test_field_names_distinct(self):
        lw = self.svars['longwave_optics_object'].local_name
        sw = self.svars['shortwave_optics_object'].local_name
        self.assertNotEqual(lw, sw)
        # One keeps the bare name; the other is disambiguated.
        self.assertIn('obj', (lw, sw))

    def test_no_duplicate_component_in_generated_type(self):
        decls = [l for l in self.data_lines
                 if 'allocatable ::' in l or l.strip().startswith('real')]
        names = [l.split('::')[1].strip().split('(')[0].strip()
                 for l in decls if '::' in l]
        self.assertEqual(len(names), len(set(names)),
                         'duplicate suite_data component: {}'.format(names))


########################################################################
# Constituent auto-resolution (cam-sima-style consumer schemes)
########################################################################

def _load_constituent_consumer_store():
    tables = parse_metadata_file(_sf('scheme_consume_constituent.meta'))
    return SchemeStore.build_from(tables)


class TestConstituentAutoResolution(unittest.TestCase):
    """Resolver routes constituent-flagged scheme args without a host or
    earlier-scheme provider through the framework's ``ccpp_constituents``
    and ``ccpp_constituent_tendencies`` arrays.  The suite cap will own
    those arrays; the resolver emits ``source='constituent'`` ResolvedArgs.
    """

    def setUp(self):
        self.hd    = _load_constituent_host_dict()
        self.store = _load_constituent_consumer_store()
        self.suite = _parse_suite('suite_consume_constituent.xml')
        self.suite_resolution    = resolve_suite(self.suite, self.store, self.hd)
        run_calls  = list(iter_phase_calls(self.suite_resolution.groups[0].phase_calls['run']))
        self.run_args = {a.scheme_local_name: a for a in run_calls[0].args}

    def test_uses_constituents_flag_set(self):
        self.assertTrue(self.suite_resolution.uses_constituents)

    def test_constituent_index_names_enumerated(self):
        # Both the base read and the tendency write reference the same
        # base std name.
        self.assertEqual(
            self.suite_resolution.constituent_index_names,
            ['cloud_liquid_water_mixing_ratio'],
        )

    def test_base_constituent_call_expr(self):
        cldliq = self.run_args['cldliq']
        self.assertEqual(cldliq.source, 'constituent')
        # Per-instance access: ccpp_model_constituents_obj(<inst>)%vars_layer(...).
        # host_with_constituents.meta declares instance_number with local
        # name ``inst_num`` (via control_full.meta), and number_of_instances
        # with local name ``ninstances`` — but only inst_num appears here.
        self.assertEqual(
            cldliq.call_expr,
            'ccpp_model_constituents_obj(inst_num)%vars_layer(lb:ub, '
            '1:nlev, index_of_cloud_liquid_water_mixing_ratio)',
        )

    def test_tendency_call_expr(self):
        tend = self.run_args['tend_cldliq']
        self.assertEqual(tend.source, 'constituent')
        self.assertEqual(
            tend.call_expr,
            'ccpp_model_constituents_obj(inst_num)%vars_layer_tend(lb:ub, '
            '1:nlev, index_of_cloud_liquid_water_mixing_ratio)',
        )


    def test_instance_number_in_used_dim_std_names(self):
        # Drives the group cap to inject instance_number as a dummy arg.
        for name in ('cldliq', 'tend_cldliq'):
            self.assertIn('instance_number',
                          self.run_args[name].used_dim_std_names)

    def test_constituent_module_name_set(self):
        cldliq = self.run_args['cldliq']
        # Under option A all constituent symbols live in a single
        # host-wide module, not per-suite.
        self.assertEqual(cldliq.constituent_module_name, 'ccpp_host_constituents')

    def test_index_symbol_in_extras(self):
        cldliq = self.run_args['cldliq']
        self.assertIn(
            'index_of_cloud_liquid_water_mixing_ratio',
            cldliq.constituent_extra_symbols,
        )

    def test_constituent_args_excluded_from_introspection(self):
        # source != 'host' — constituent args do not appear in suite
        # input/output lists (validated indirectly via _collect_host_io
        # in host_cap tests; here we just confirm the source).
        for arg in self.run_args.values():
            if arg.scheme_local_name in ('cldliq', 'tend_cldliq'):
                self.assertNotEqual(arg.source, 'host')

    def test_no_number_of_ccpp_constituents_in_extras(self):
        # Regression: under the per-instance design, the
        # ``number_of_ccpp_constituents`` symbol is no longer module-level
        # — its value is reached as ccpp_model_constituents_obj(inst)%
        # num_layer_vars.  Even when a scheme dim references it, the
        # resolver MUST NOT add it to constituent_extra_symbols (it
        # doesn't exist as a USE'd symbol in ccpp_host_constituents and
        # would produce an "Symbol ... not found" Fortran error).  It
        # must also NOT leak into used_dim_std_names (would trigger
        # spurious USE/dummy-arg plumbing in the group cap).
        for arg in self.run_args.values():
            self.assertNotIn(
                'number_of_ccpp_constituents',
                arg.constituent_extra_symbols,
                'arg {!r} leaked number_of_ccpp_constituents into '
                'constituent_extra_symbols'.format(arg.scheme_local_name),
            )
            self.assertNotIn(
                'number_of_ccpp_constituents',
                arg.used_dim_std_names,
                'arg {!r} leaked number_of_ccpp_constituents into '
                'used_dim_std_names (should live on '
                'used_const_dim_std_names instead)'.format(
                    arg.scheme_local_name),
            )


class TestUsedConstDimStdNames(unittest.TestCase):
    """``ResolvedArg.used_const_dim_std_names`` carries framework-
    constituent dim refs (notably ``number_of_ccpp_constituents``) so
    the introspection routines in :mod:`generator.host_cap` can list
    them as inputs without polluting the host-side
    :attr:`used_dim_std_names` channel."""

    def _scheme_var(self, local, std_name, dims, intent='in'):
        from metadata.metadata_table import MetaVar
        ctx = _ctx()
        v = MetaVar(local, ctx)
        v.set_attr('standard_name', std_name, ctx)
        v.set_attr('units', 'none', ctx)
        v.set_attr('dimensions', dims, ctx)
        v.set_attr('type', 'real', ctx)
        v.set_attr('kind', 'kind_phys', ctx)
        v.set_attr('intent', intent, ctx)
        return v

    def test_ccpp_constituents_dim_lands_on_dedicated_field(self):
        # Uses _load_full_host_dict (no ccpp_model_constituents_t DDT
        # instance), so the host-wins gate does NOT fire and the
        # resolver routes through capgen's auto-provisioning path.
        from generator.suite_resolver import _resolve_constituent_arg
        hd = _load_full_host_dict()
        suite_var = self._scheme_var(
            'consts', 'ccpp_constituents',
            '(horizontal_dimension, vertical_layer_dimension, '
            'number_of_ccpp_constituents)',
            intent='in',
        )
        arg = _resolve_constituent_arg(
            suite_var, 'run', hd, {}, 'consts_user', 'mysuite',
        )
        self.assertIsNotNone(arg)
        self.assertEqual(arg.source, 'constituent')
        # Goes on the dedicated channel.
        self.assertEqual(arg.used_const_dim_std_names,
                         {'number_of_ccpp_constituents'})
        # Does NOT leak into the host-side channels.
        self.assertNotIn('number_of_ccpp_constituents',
                         arg.used_dim_std_names)
        self.assertNotIn('number_of_ccpp_constituents',
                         arg.constituent_extra_symbols)

    def test_no_const_dim_when_arg_does_not_reference_it(self):
        # cldliq is 2D — no framework-constituent dim refs.  Uses
        # _load_full_host_dict so the resolver routes through the
        # constituent auto-provisioning path (Path 2: is_constituent +
        # intent=in).
        from generator.suite_resolver import _resolve_constituent_arg
        hd = _load_full_host_dict()
        suite_var = self._scheme_var(
            'cldliq', 'cloud_liquid_water_mixing_ratio',
            '(horizontal_dimension, vertical_layer_dimension)',
            intent='in',
        )
        suite_var.set_attr('advected', 'True', _ctx())
        arg = _resolve_constituent_arg(
            suite_var, 'run', hd, {}, 'cldliq_user', 'mysuite',
        )
        self.assertIsNotNone(arg)
        self.assertEqual(arg.used_const_dim_std_names, set())

    def test_minimum_values_routes_to_vars_minvalue(self):
        # ``ccpp_constituent_minimum_values`` is a framework-named std
        # whose value is per-constituent and lives on
        # ``ccpp_model_constituents_t%vars_minvalue(:)``.  The resolver
        # must route it through Path 1b (framework-name) — the
        # ``vars_minvalue`` member, not Path 2 (constituent auto-
        # provisioning).  Drives cam-sima's ``qneg`` scheme: under the
        # original capgen contract this was a host-USE'd module array;
        # capgen exposes it through the per-instance object.
        from generator.suite_resolver import _resolve_constituent_arg
        hd = _load_full_host_dict()
        suite_var = self._scheme_var(
            'qmin', 'ccpp_constituent_minimum_values',
            '(number_of_ccpp_constituents)',
            intent='in',
        )
        arg = _resolve_constituent_arg(
            suite_var, 'run', hd, {}, 'qneg', 'mysuite',
        )
        self.assertIsNotNone(arg)
        self.assertEqual(arg.source, 'constituent')
        inst_local = hd['instance_number'].local_name
        self.assertEqual(
            arg.call_expr,
            'ccpp_model_constituents_obj({})%vars_minvalue(:)'.format(
                inst_local),
        )
        # number_of_ccpp_constituents goes on the dedicated channel.
        self.assertEqual(arg.used_const_dim_std_names,
                         {'number_of_ccpp_constituents'})
        self.assertNotIn('number_of_ccpp_constituents',
                         arg.used_dim_std_names)
        self.assertNotIn('number_of_ccpp_constituents',
                         arg.constituent_extra_symbols)


class TestIndexSymbolNameMangling(unittest.TestCase):
    """``_index_symbol_name`` keeps short ``index_of_<X>`` names
    intact, but mangles overlong CAM-SIMA-style names down to the
    Fortran 63-char identifier limit with a deterministic SHA hash so
    every emit/reference site agrees on the symbol."""

    def test_short_name_passes_through(self):
        from generator.suite_resolver import _index_symbol_name
        self.assertEqual(_index_symbol_name('water_vapor'),
                         'index_of_water_vapor')

    def test_overlong_name_truncated_and_hashed(self):
        from generator.suite_resolver import _index_symbol_name
        long_base = ('cloud_liquid_water_mixing_ratio_'
                     'wrt_moist_air_and_condensed_water')
        sym = _index_symbol_name(long_base)
        # Must be a Fortran-legal identifier (≤ 63 chars), prefixed
        # with index_of_, and stable across calls.
        self.assertLessEqual(len(sym), 63)
        self.assertTrue(sym.startswith('index_of_'))
        self.assertEqual(sym, _index_symbol_name(long_base))

    def test_distinct_bases_distinct_symbols(self):
        # Two CAM-SIMA constituents share the same long suffix; the
        # hash component must keep their symbols distinct so the
        # framework's per-constituent integer storage doesn't alias.
        from generator.suite_resolver import _index_symbol_name
        a = _index_symbol_name(
            'cloud_liquid_water_mixing_ratio_'
            'wrt_moist_air_and_condensed_water')
        b = _index_symbol_name(
            'water_vapor_mixing_ratio_'
            'wrt_moist_air_and_condensed_water')
        self.assertNotEqual(a, b)

    def test_used_in_auto_provisioned_call_expr(self):
        # End-to-end check: the auto-provisioning subscript path
        # (Path 2) routes the index_of_<X> token through the helper,
        # so the call_expr that lands in the group cap is a legal
        # Fortran symbol.
        from generator.suite_resolver import (
            _resolve_constituent_arg, _index_symbol_name,
        )
        long_base = ('cloud_liquid_water_mixing_ratio_'
                     'wrt_moist_air_and_condensed_water')
        hd = _load_full_host_dict()
        scheme_var = self._scheme_var_for_mangling(
            'cldliq', long_base,
            '(horizontal_dimension, vertical_layer_dimension)',
        )
        scheme_var.set_attr('advected', 'True', _ctx())
        arg = _resolve_constituent_arg(
            scheme_var, 'run', hd, {}, 'consumer', 'mysuite',
        )
        self.assertIsNotNone(arg)
        expected_index_sym = _index_symbol_name(long_base)
        self.assertIn(expected_index_sym, arg.constituent_extra_symbols)
        # The long raw form must NOT appear (would blow the Fortran limit).
        self.assertNotIn('index_of_' + long_base,
                         arg.constituent_extra_symbols)
        self.assertIn(expected_index_sym, arg.call_expr)

    @staticmethod
    def _scheme_var_for_mangling(local, std_name, dims, intent='in'):
        from metadata.metadata_table import MetaVar
        ctx = _ctx()
        v = MetaVar(local, ctx)
        v.set_attr('standard_name', std_name, ctx)
        v.set_attr('units', 'none', ctx)
        v.set_attr('dimensions', dims, ctx)
        v.set_attr('type', 'real', ctx)
        v.set_attr('kind', 'kind_phys', ctx)
        v.set_attr('intent', intent, ctx)
        return v


class TestConstSubscriptHelper(unittest.TestCase):
    """``_const_dim_part`` / ``_build_const_subscript``:
    ``number_of_ccpp_constituents`` becomes ``':'`` and is routed
    through the dedicated ``used_const_dim_std`` channel — NOT through
    ``used_host_std`` (which would imply USE/dummy-arg plumbing) and
    NOT through ``used_const_std`` (which would imply a USE'd symbol).
    """

    def setUp(self):
        from generator.suite_resolver import (
            _const_dim_part, _build_const_subscript,
        )
        self._const_dim_part = _const_dim_part
        self._build_const_subscript = _build_const_subscript
        self.hd = _load_full_host_dict()

    def test_dim_part_returns_colon(self):
        part, used_host, used_const, used_const_dim = self._const_dim_part(
            'number_of_ccpp_constituents', 'run', self.hd,
        )
        self.assertEqual(part, ':')
        # NOT a host dim — _collect_group_uses / _extra_dim_ctrl_entries
        # would mishandle it if it leaked here.
        self.assertEqual(used_host, set())
        # NOT a USE'd symbol — it isn't a public name on
        # ccpp_host_constituents in the per-instance design.
        self.assertEqual(used_const, set())
        # The introspection-only channel.
        self.assertEqual(used_const_dim, {'number_of_ccpp_constituents'})

    def test_dim_part_handles_explicit_lower_bound(self):
        part, used_host, used_const, used_const_dim = self._const_dim_part(
            'ccpp_constant_one:number_of_ccpp_constituents', 'run', self.hd,
        )
        self.assertEqual(part, ':')
        self.assertEqual(used_host, set())
        self.assertEqual(used_const, set())
        self.assertEqual(used_const_dim, {'number_of_ccpp_constituents'})

    def test_full_3d_subscript(self):
        # Mirrors apply_constituent_tendencies.meta's
        # (horizontal_dimension, vertical_layer_dimension,
        #  number_of_ccpp_constituents).
        sub, used_host, used_const, used_const_dim = self._build_const_subscript(
            ['horizontal_dimension', 'vertical_layer_dimension',
             'number_of_ccpp_constituents'],
            'run', self.hd,
        )
        self.assertEqual(sub, '(lb:ub, 1:nlev, :)')
        # Host dims (horizontal_*, vertical_*) live in used_host;
        # number_of_ccpp_constituents lives in used_const_dim.
        self.assertNotIn('number_of_ccpp_constituents', used_host)
        self.assertEqual(used_const, set())
        self.assertEqual(used_const_dim, {'number_of_ccpp_constituents'})


# auto-clone-constituents: the entire class below exists because the
# legacy auto-clone shim introduces a second source for "this suite
# needs a per-suite ``<suite>_dynamic_constituents`` buffer".  When
# the shim retires, this class can be deleted -- the property collapses
# to ``bool(self.constituent_register_calls)`` and the existing
# register-path tests elsewhere already cover that case.
class TestNeedsDynamicConstituentsBufferProperty(unittest.TestCase):
    """``SuiteResolution.needs_dynamic_constituents_buffer`` is the
    single source of truth for "this suite needs a
    ``<suite>_dynamic_constituents`` buffer".  Centralising the rule
    here keeps legacy-shim state (``auto_cloned_constituents``) out of
    every generator emitter; consumers reference the property instead
    of OR-ing the two underlying fields.  When the legacy auto-clone
    shim retires, only this property's body changes."""

    def test_false_when_neither(self):
        # auto-clone-constituents: empty-state baseline.
        from generator.suite_resolver import SuiteResolution
        sr = SuiteResolution(suite_name='s')
        self.assertFalse(sr.needs_dynamic_constituents_buffer)

    def test_true_for_register_calls(self):
        # auto-clone-constituents: register-only path -- verifies the
        # property still fires correctly for non-shim registrations
        # after the OR-abstraction landed.
        from generator.suite_resolver import SuiteResolution
        sr = SuiteResolution(
            suite_name='s',
            constituent_register_calls=[('register_constituents', 'register')],
        )
        self.assertTrue(sr.needs_dynamic_constituents_buffer)

    def test_true_for_auto_cloned_only(self):
        # auto-clone-constituents: shim-only path -- regression for
        # the CAM-SIMA kessler_test build (2026-06-03).
        from generator.suite_resolver import SuiteResolution, AutoCloneEntry
        sr = SuiteResolution(
            suite_name='s',
            auto_cloned_constituents=[AutoCloneEntry(
                std_name='water_vapor', long_name='', diag_name='qv',
                units='kg kg-1', vertical_dim='vertical_layer_dimension',
                advected=True, molar_mass=0.0, default_value=None,
                min_value=None, water_species=None, mixing_ratio_type=None,
            )],
        )
        self.assertTrue(sr.needs_dynamic_constituents_buffer)


class TestConstituentResolverErrors(unittest.TestCase):
    """Mismatched constituent-flag + intent + std-name combinations error."""

    _SCHEME_TEMPLATE = (
        '[ccpp-table-properties]\n'
        '  name = bad_scheme\n'
        '  type = scheme\n'
        '[ccpp-arg-table]\n'
        '  name = bad_scheme_run\n'
        '  type = scheme\n'
        '[ x ]\n'
        '  standard_name = {std}\n'
        '  units = kg kg-1\n'
        '  dimensions = (horizontal_dimension, vertical_layer_dimension)\n'
        '  type = real | kind = kind_phys\n'
        '  intent = {intent}\n'
        '  {flag} = .true.\n'
    )

    _SUITE_XML = (
        '<?xml version="1.0" encoding="UTF-8"?>\n'
        '<suite name="badcs" version="1.0">\n'
        '  <group name="g"><scheme>bad_scheme</scheme></group>\n'
        '</suite>\n'
    )

    def _build_store(self, std, intent, flag):
        with tempfile.NamedTemporaryFile('w', suffix='.meta', delete=False) as fh:
            fh.write(self._SCHEME_TEMPLATE.format(
                std=std, intent=intent, flag=flag,
            ))
            path = fh.name
        try:
            tables = parse_metadata_file(path)
        finally:
            os.unlink(path)
        return SchemeStore.build_from(tables)

    def _resolve(self, std, intent, flag):
        hd    = _load_constituent_host_dict()
        store = self._build_store(std, intent, flag)
        with tempfile.TemporaryDirectory() as tmpdir:
            xml = os.path.join(tmpdir, 's.xml')
            with open(xml, 'w') as f:
                f.write(self._SUITE_XML)
            from generator.suite_xml import parse_suite_xml
            import logging
            suite = parse_suite_xml(xml, tmpdir, logging.getLogger('t'),
                                    skip_validation=True)
        return resolve_suite(suite, store, hd)

    def test_base_constituent_intent_out_raises(self):
        # advected + intent=out on a non-tendency std name → reject.
        with self.assertRaises(CCPPError) as ctx:
            self._resolve('air_temperature_extra', 'out', 'advected')
        self.assertIn("'tendency_of_'", str(ctx.exception))

    def test_tendency_intent_in_resolves_to_tendency_array(self):
        # Rule (b): a constituent tendency may be CONSUMED (e.g. a diagnostics
        # scheme).  intent=in on a tendency_of_* std name now reads the framework
        # tendency column instead of erroring.
        res = self._resolve('tendency_of_air_temperature', 'in', 'constituent')
        arg = list(iter_phase_calls(res.groups[0].phase_calls['run']))[0].args[0]
        self.assertEqual(arg.source, 'constituent')
        self.assertIn('vars_layer_tend', arg.call_expr)

    def test_tendency_intent_inout_resolves_to_tendency_array(self):
        res = self._resolve('tendency_of_air_temperature', 'inout', 'constituent')
        arg = list(iter_phase_calls(res.groups[0].phase_calls['run']))[0].args[0]
        self.assertEqual(arg.source, 'constituent')
        self.assertIn('vars_layer_tend', arg.call_expr)


class TestConstituentConsumerInferenceRuleB(unittest.TestCase):
    """Rule (b): an UNFLAGGED scheme may CONSUME a constituent that another
    scheme flags -- a base constituent (``advected``) read via ``vars_layer``, or
    a constituent tendency (``constituent`` on a ``tendency_of_*`` producer) read
    via ``vars_layer_tend``.  The consumer infers it from the scheme-wide flag
    set and never re-flags.  Whether a name is a constituent is the host's call,
    so a name NO scheme flags stays an ordinary variable.
    """

    _PRODUCER_CONSUMER = (
        '[ccpp-table-properties]\n'
        '  name = tend_producer\n'
        '  type = scheme\n'
        '[ccpp-arg-table]\n'
        '  name = tend_producer_run\n'
        '  type = scheme\n'
        '[ qt ]\n'
        '  standard_name = tendency_of_air_temperature\n'
        '  units = K s-1\n'
        '  dimensions = (horizontal_dimension, vertical_layer_dimension)\n'
        '  type = real | kind = kind_phys\n'
        '  intent = out\n'
        '  constituent = .true.\n'
        '\n'
        '[ccpp-table-properties]\n'
        '  name = tend_consumer\n'
        '  type = scheme\n'
        '[ccpp-arg-table]\n'
        '  name = tend_consumer_run\n'
        '  type = scheme\n'
        '[ qt ]\n'
        '  standard_name = tendency_of_air_temperature\n'
        '  units = K s-1\n'
        '  dimensions = (horizontal_dimension, vertical_layer_dimension)\n'
        '  type = real | kind = kind_phys\n'
        '  intent = in\n'
    )

    # Consumer only (no scheme flags the tendency as a constituent).
    _CONSUMER_ONLY = (
        '[ccpp-table-properties]\n'
        '  name = tend_consumer\n'
        '  type = scheme\n'
        '[ccpp-arg-table]\n'
        '  name = tend_consumer_run\n'
        '  type = scheme\n'
        '[ qt ]\n'
        '  standard_name = tendency_of_air_temperature\n'
        '  units = K s-1\n'
        '  dimensions = (horizontal_dimension, vertical_layer_dimension)\n'
        '  type = real | kind = kind_phys\n'
        '  intent = in\n'
    )

    # A base constituent (advected) flagged by one scheme and consumed unflagged
    # by another; nothing else provides it.
    _BASE_FLAGGED_AND_UNFLAGGED = (
        '[ccpp-table-properties]\n'
        '  name = base_flagged\n'
        '  type = scheme\n'
        '[ccpp-arg-table]\n'
        '  name = base_flagged_run\n'
        '  type = scheme\n'
        '[ q ]\n'
        '  standard_name = made_up_dry_mixing_ratio\n'
        '  units = kg kg-1\n'
        '  dimensions = (horizontal_dimension, vertical_layer_dimension)\n'
        '  type = real | kind = kind_phys\n'
        '  intent = in\n'
        '  advected = .true.\n'
        '\n'
        '[ccpp-table-properties]\n'
        '  name = base_unflagged\n'
        '  type = scheme\n'
        '[ccpp-arg-table]\n'
        '  name = base_unflagged_run\n'
        '  type = scheme\n'
        '[ q ]\n'
        '  standard_name = made_up_dry_mixing_ratio\n'
        '  units = kg kg-1\n'
        '  dimensions = (horizontal_dimension, vertical_layer_dimension)\n'
        '  type = real | kind = kind_phys\n'
        '  intent = in\n'
    )

    def _resolve(self, meta, schemes):
        hd = _load_constituent_host_dict()
        with tempfile.NamedTemporaryFile('w', suffix='.meta', delete=False) as fh:
            fh.write(meta)
            path = fh.name
        try:
            store = SchemeStore.build_from(parse_metadata_file(path))
        finally:
            os.unlink(path)
        xml = ('<?xml version="1.0" encoding="UTF-8"?>\n'
               '<suite name="tendcs" version="1.0">\n'
               '  <group name="g">\n'
               + ''.join('    <scheme>{}</scheme>\n'.format(s) for s in schemes)
               + '  </group>\n</suite>\n')
        with tempfile.TemporaryDirectory() as tmpdir:
            xpath = os.path.join(tmpdir, 's.xml')
            with open(xpath, 'w') as f:
                f.write(xml)
            from generator.suite_xml import parse_suite_xml
            import logging
            suite = parse_suite_xml(xpath, tmpdir, logging.getLogger('t'),
                                    skip_validation=True)
        return resolve_suite(suite, store, hd)

    def test_unflagged_consumer_reads_tendency_array(self):
        res = self._resolve(self._PRODUCER_CONSUMER,
                            ['tend_producer', 'tend_consumer'])
        calls = list(iter_phase_calls(res.groups[0].phase_calls['run']))
        producer_arg = calls[0].args[0]
        consumer_arg = calls[1].args[0]
        self.assertEqual(producer_arg.source, 'constituent')
        self.assertIn('vars_layer_tend', producer_arg.call_expr)
        # The consumer carries NO constituent flag, yet routes to the SAME
        # framework tendency column (rule b inference from the producer).
        self.assertEqual(consumer_arg.source, 'constituent')
        self.assertIn('vars_layer_tend', consumer_arg.call_expr)

    def test_unflagged_tendency_without_producer_is_not_constituent(self):
        # Nothing flags the tendency, so it stays an ordinary variable: an
        # unprovided consumer is a normal "not provided" error, never silently
        # routed to the constituent tendency array.
        with self.assertRaises(CCPPError) as ctx:
            self._resolve(self._CONSUMER_ONLY, ['tend_consumer'])
        self.assertIn('not provided', str(ctx.exception))

    def test_unflagged_base_consumer_reads_vars_layer(self):
        # Symmetric to the tendency case: a base constituent flagged advected by
        # one scheme is read by an unflagged consumer via the SAME base column
        # (vars_layer, NOT the tendency array).
        res = self._resolve(self._BASE_FLAGGED_AND_UNFLAGGED,
                            ['base_flagged', 'base_unflagged'])
        calls = list(iter_phase_calls(res.groups[0].phase_calls['run']))
        flagged = calls[0].args[0]
        unflagged = calls[1].args[0]
        self.assertEqual(flagged.source, 'constituent')
        self.assertIn('%vars_layer(', flagged.call_expr)
        self.assertEqual(unflagged.source, 'constituent')
        self.assertIn('%vars_layer(', unflagged.call_expr)
        self.assertNotIn('vars_layer_tend', unflagged.call_expr)


class TestHostDeclaredIndexOfWinsOverConstituents(unittest.TestCase):
    """Regression: when the host declares an ``index_of_<X>`` integer as a
    regular host variable, scheme references to that std_name must resolve
    to the host's short local name — NOT route through the constituent
    auto-provisioning path (which would emit a parallel module-level
    integer named after the full std_name in ``ccpp_host_constituents``
    and, for SCM-style long std_names, blow the Fortran 63-char identifier
    limit).
    """

    _HOST_SRC = (
        '[ccpp-table-properties]\n'
        '  name = scm_host_mod\n'
        '  type = host\n'
        '[ccpp-arg-table]\n'
        '  name = scm_host_mod\n'
        '  type = host\n'
        # Mirrors SCM's GFS_typedefs: short Fortran local name `ntcw`
        # paired with a long index_of_* standard name.
        '[ ntcw ]\n'
        '  standard_name = index_of_cloud_liquid_water_mixing_ratio_in_tracer_concentration_array\n'
        '  units = index\n'
        '  type = integer\n'
        '  protected = True\n'
        '  dimensions = ()\n'
    )

    def _scheme_var(self, local, std_name, intent='in'):
        from metadata.metadata_table import MetaVar
        ctx = _ctx()
        v = MetaVar(local, ctx)
        v.set_attr('standard_name', std_name, ctx)
        v.set_attr('units', 'index', ctx)
        v.set_attr('dimensions', '()', ctx)
        v.set_attr('type', 'integer', ctx)
        v.set_attr('intent', intent, ctx)
        return v

    def test_host_index_of_resolves_to_host_local_name(self):
        hd = build_flat_host_dict(_parse(self._HOST_SRC), [], [])
        suite_var = self._scheme_var(
            'ntcw',
            'index_of_cloud_liquid_water_mixing_ratio_in_tracer_concentration_array',
            intent='in',
        )
        arg = _resolve_one_arg(suite_var, 'run', hd, {}, 'gfs_mp_generic_pre', set())
        # Host metadata wins: source=host, short local name, NO leakage of
        # the long std_name into ccpp_host_constituents.
        self.assertEqual(arg.source, 'host')
        self.assertEqual(arg.call_expr, 'ntcw')
        self.assertIsNotNone(arg.host_entry)
        self.assertEqual(arg.host_entry.local_name, 'ntcw')

    def test_unclaimed_index_of_still_routes_to_constituents(self):
        """The framework auto-provisioning path is preserved for
        ``index_of_<X>`` names the host does NOT declare — required for
        capgen-owned constituent flows (cf. the advection e2e test)."""
        hd = build_flat_host_dict(_parse(self._HOST_SRC), [], [])
        suite_var = self._scheme_var(
            'idx_other', 'index_of_some_other_constituent_not_in_host',
            intent='in',
        )
        arg = _resolve_one_arg(suite_var, 'run', hd, {}, 'some_scheme', set())
        self.assertEqual(arg.source, 'constituent')
        self.assertEqual(arg.call_expr,
                         'index_of_some_other_constituent_not_in_host')


class TestDimDDTComponentResolution(unittest.TestCase):
    """When a dimension standard name maps to a DDT-component host
    entry (e.g. ``vertical_layer_dimension`` is ``physics%Model%levs``,
    a two-level DDT walk inside the SCM/UFS host), the resolver must:

    1. Emit the full ``access_path`` in subscript expressions
       (``1:physics%Model%levs``), NOT the bare leaf (``1:levs``).
    2. Walk back to the access-path *root* for the USE statement
       (``use scm_host_mod, only: physics``), NOT the leaf
       (``use scm_host_mod, only: levs`` — undefined symbol).
    3. Behave identically to today for plain module-level host vars
       (``access_path == local_name``).

    Historical: this was a long-standing pain point in the original
    capgen → SCM migration.  The pre-2026-05-13 capgen emitted
    ``use scm_type_defs, only: levs`` and similar bogus imports for
    every DDT-component dim, producing many ``Symbol referenced ...
    not found in module`` errors at compile time.
    """

    # Mimics SCM: ``physics`` is host-level (module ``scm_host_mod``),
    # of type ``physics_t`` (a DDT) which has component ``Model`` of
    # type ``gfs_control_t`` (a DDT) which has scalar ``levs`` and
    # ``ncols`` declared on it.
    _DDT_SRC = (
        '[ccpp-table-properties]\n'
        '  name = gfs_control_t\n'
        '  type = ddt\n'
        '[ccpp-arg-table]\n'
        '  name = gfs_control_t\n'
        '  type = ddt\n'
        '[levs]\n'
        '  standard_name = vertical_layer_dimension\n'
        '  units = count\n'
        '  dimensions = ()\n'
        '  type = integer\n'
        '[ncol]\n'
        '  standard_name = horizontal_dimension_total\n'
        '  units = count\n'
        '  dimensions = ()\n'
        '  type = integer\n'
        '\n'
        '[ccpp-table-properties]\n'
        '  name = physics_t\n'
        '  type = ddt\n'
        '[ccpp-arg-table]\n'
        '  name = physics_t\n'
        '  type = ddt\n'
        '[Model]\n'
        '  standard_name = gfs_control_instance\n'
        '  units = DDT\n'
        '  dimensions = ()\n'
        '  type = gfs_control_t\n'
    )

    _HOST_SRC = (
        '[ccpp-table-properties]\n'
        '  name = scm_host_mod\n'
        '  type = host\n'
        '[ccpp-arg-table]\n'
        '  name = scm_host_mod\n'
        '  type = host\n'
        # Plain (non-DDT) horizontal dim — for the mixed-dim test.
        '[ncols]\n'
        '  standard_name = horizontal_dimension\n'
        '  units = count\n'
        '  dimensions = ()\n'
        '  type = integer\n'
        # The DDT instance.  Resolver should produce
        # access_path == 'physics' for the instance, and
        # 'physics%Model%levs' for the leaf dim.
        '[physics]\n'
        '  standard_name = physics_state_instance\n'
        '  units = DDT\n'
        '  dimensions = ()\n'
        '  type = physics_t\n'
    )

    def _host_dict(self):
        return build_flat_host_dict(
            _parse(self._HOST_SRC), [], _parse(self._DDT_SRC),
        )

    # ---- Unit-level: host_dict entry shape (the prerequisite) -----------

    def test_host_dict_levs_has_ddt_walk_access_path(self):
        hd = self._host_dict()
        entry = hd.get('vertical_layer_dimension')
        self.assertIsNotNone(entry)
        self.assertEqual(entry.local_name, 'levs')
        self.assertEqual(entry.access_path, 'physics%Model%levs')
        self.assertEqual(entry.module_name, 'scm_host_mod')

    # ---- Unit-level: _resolve_single_bound (the primary fix site) -------

    def test_resolve_single_bound_returns_access_path_for_ddt_dim(self):
        from generator.suite_resolver import _resolve_single_bound
        hd = self._host_dict()
        used = set()
        result = _resolve_single_bound('vertical_layer_dimension', hd, used)
        self.assertEqual(result, 'physics%Model%levs')
        self.assertIn('vertical_layer_dimension', used)

    def test_resolve_single_bound_plain_var_unchanged(self):
        # ncols is a plain host var (access_path == local_name == 'ncols').
        from generator.suite_resolver import _resolve_single_bound
        hd = self._host_dict()
        used = set()
        result = _resolve_single_bound('horizontal_dimension', hd, used)
        self.assertEqual(result, 'ncols')

    # ---- Unit-level: _one_dim_part wrapping -----------------------------

    def test_one_dim_part_bare_form_uses_ddt_walk(self):
        from generator.suite_resolver import _one_dim_part
        hd = self._host_dict()
        # Bare std name normalises to ``ccpp_constant_one:<name>``.
        part, used = _one_dim_part(
            'vertical_layer_dimension', 'run', hd,
        )
        self.assertEqual(part, '1:physics%Model%levs')
        self.assertIn('vertical_layer_dimension', used)

    def test_one_dim_part_range_form_uses_ddt_walk(self):
        from generator.suite_resolver import _one_dim_part
        hd = self._host_dict()
        part, used = _one_dim_part(
            'ccpp_constant_one:vertical_layer_dimension', 'run', hd,
        )
        self.assertEqual(part, '1:physics%Model%levs')

    def test_one_dim_part_ddt_dim_as_lower_bound(self):
        # Stress: DDT-component appears as the LOWER bound of a range.
        from generator.suite_resolver import _one_dim_part
        hd = self._host_dict()
        part, used = _one_dim_part(
            'vertical_layer_dimension:horizontal_dimension_total', 'run', hd,
        )
        # Both bounds walked: physics%Model%levs : physics%Model%ncol
        self.assertEqual(part, 'physics%Model%levs:physics%Model%ncol')

    # ---- Unit-level: _build_call_subscript composition -----------------

    def test_build_call_subscript_two_ddt_dims(self):
        # Two DDT-component dims side by side: both must walk.
        from generator.suite_resolver import _build_call_subscript
        hd = self._host_dict()
        sub, used = _build_call_subscript(
            ['horizontal_dimension_total', 'vertical_layer_dimension'],
            'run', hd,
        )
        self.assertEqual(
            sub, '(1:physics%Model%ncol, 1:physics%Model%levs)',
        )

    def test_build_call_subscript_pure_ddt_dims(self):
        from generator.suite_resolver import _build_call_subscript
        hd = self._host_dict()
        sub, used = _build_call_subscript(
            ['vertical_layer_dimension', 'vertical_layer_dimension'],
            'run', hd,
        )
        self.assertEqual(sub, '(1:physics%Model%levs, 1:physics%Model%levs)')

    # ---- Sliced-subscript path: _build_merged_subscript ---------------

    def test_build_merged_subscript_ddt_index_token(self):
        # Mirrors host metadata like ``q(:,:,vertical_layer_dimension)``
        # but here we exercise the helper directly with a synthetic
        # local_subscript carrying a std-name token whose target lives
        # on a DDT.  The third token must be the full DDT walk, not
        # the bare leaf — bug pre-2026-05-13 produced ``levs`` instead
        # of ``physics%Model%levs`` and the generated cap then failed
        # to compile against the host module.
        from generator.suite_resolver import _build_merged_subscript
        hd = self._host_dict()
        # Use ``horizontal_dimension_total`` for the leading dims so the
        # fixture doesn't need loop bounds.
        merged, used = _build_merged_subscript(
            host_dims=['horizontal_dimension_total',
                       'horizontal_dimension_total'],
            local_subscript=[':', ':', 'vertical_layer_dimension'],
            phase='run', host_dict=hd, suite_vars={},
        )
        # Third subscript token = full DDT walk.
        self.assertTrue(
            merged.rstrip(')').endswith('physics%Model%levs'),
            'merged subscript should end with the DDT walk; got {!r}'
            .format(merged),
        )
        # Leaf should not leak.
        self.assertNotIn(', levs)', merged)

    # ---- Group-cap USE collection: _collect_dim_uses --------------------

    @staticmethod
    def _mock_arg(used_dim_std_names):
        from unittest.mock import MagicMock
        arg = MagicMock()
        arg.used_dim_std_names = set(used_dim_std_names)
        return arg

    def _mock_rg(self, arg):
        from unittest.mock import MagicMock
        from generator.suite_resolver import ResolvedCall
        resolved_group = MagicMock()
        resolved_group.phase_calls = {
            'run': [ResolvedCall(
                scheme_name='dummy', phase='run',
                args=[arg], scheme_module='dummy_mod',
            )],
        }
        return resolved_group

    def test_collect_dim_uses_walks_to_root_for_ddt_dim(self):
        from generator.suite_resolver import _collect_dim_uses
        hd = self._host_dict()
        arg = self._mock_arg({'vertical_layer_dimension'})
        resolved_group = self._mock_rg(arg)
        dim_uses = _collect_dim_uses(resolved_group, hd, suite_vars={})
        # USE clause must pull the ROOT (``physics``), not the leaf
        # (``levs``, which is not a module symbol).
        self.assertIn('scm_host_mod', dim_uses)
        self.assertIn('physics', dim_uses['scm_host_mod'])
        self.assertNotIn('levs', dim_uses['scm_host_mod'])
        self.assertNotIn('Model', dim_uses['scm_host_mod'])

    def test_collect_dim_uses_plain_var_unchanged(self):
        # Regression: behaviour identical for plain host vars.
        from generator.suite_resolver import _collect_dim_uses
        hd = self._host_dict()
        arg = self._mock_arg({'horizontal_dimension'})
        resolved_group = self._mock_rg(arg)
        dim_uses = _collect_dim_uses(resolved_group, hd, suite_vars={})
        self.assertEqual(dim_uses.get('scm_host_mod'), {'ncols'})

    def test_collect_dim_uses_two_ddt_dims_dedupe_root(self):
        # Both ``vertical_layer_dimension`` and
        # ``horizontal_dimension_total`` live on the same ``physics``
        # instance — USE clause emits ``physics`` ONCE, not twice.
        from generator.suite_resolver import _collect_dim_uses
        hd = self._host_dict()
        arg = self._mock_arg({'vertical_layer_dimension',
                              'horizontal_dimension_total'})
        resolved_group = self._mock_rg(arg)
        dim_uses = _collect_dim_uses(resolved_group, hd, suite_vars={})
        self.assertEqual(dim_uses.get('scm_host_mod'), {'physics'})

    # ---- End-to-end: resolve_suite + group cap output ------------------

    def test_end_to_end_ddt_dim_in_group_cap(self):
        """Scheme arg with a DDT-component dim should compile.

        Builds the full pipeline: parse scheme + host metadata, resolve
        the suite, and generate the group cap text.  Asserts:

        - The scheme call subscript contains ``1:physics%Model%levs``
          (NOT ``1:levs``).
        - The group cap's ``use scm_host_mod, only: ...`` clause
          contains ``physics`` (the DDT root) and NOT ``levs`` (the
          leaf, which would be an undefined symbol).
        """
        # Add control vars so the static API can build.
        control_src = (
            '[ccpp-table-properties]\n'
            '  name = control\n'
            '  type = control\n'
            '[ccpp-arg-table]\n'
            '  name = control\n'
            '  type = control\n'
            '[errmsg]\n'
            '  standard_name = ccpp_error_message\n'
            '  units = none\n'
            '  dimensions = ()\n'
            '  type = character | kind = len=512\n'
            '[errflg]\n'
            '  standard_name = ccpp_error_code\n'
            '  units = 1\n'
            '  dimensions = ()\n'
            '  type = integer\n'
            '[ilb]\n'
            '  standard_name = horizontal_loop_begin\n'
            '  units = count\n'
            '  dimensions = ()\n'
            '  type = integer\n'
            '[iub]\n'
            '  standard_name = horizontal_loop_end\n'
            '  units = count\n'
            '  dimensions = ()\n'
            '  type = integer\n'
        )
        scheme_src = (
            '[ccpp-table-properties]\n'
            '  name = ddt_dim_user\n'
            '  type = scheme\n'
            '[ccpp-arg-table]\n'
            '  name = ddt_dim_user_run\n'
            '  type = scheme\n'
            '[temp]\n'
            '  standard_name = air_temperature\n'
            '  units = K\n'
            '  dimensions = (horizontal_dimension, vertical_layer_dimension)\n'
            '  type = real | kind = kind_phys\n'
            '  intent = inout\n'
            '[errmsg]\n'
            '  standard_name = ccpp_error_message\n'
            '  units = none\n'
            '  dimensions = ()\n'
            '  type = character | kind = len=*\n'
            '  intent = out\n'
            '[errflg]\n'
            '  standard_name = ccpp_error_code\n'
            '  units = 1\n'
            '  dimensions = ()\n'
            '  type = integer\n'
            '  intent = out\n'
        )
        # Extend the host with horizontal_dimension + air_temperature
        # so the scheme's dims resolve.
        host_src = self._HOST_SRC + (
            '[gt0]\n'
            '  standard_name = air_temperature\n'
            '  units = K\n'
            '  dimensions = (horizontal_dimension, vertical_layer_dimension)\n'
            '  type = real | kind = kind_phys\n'
        )
        hd = build_flat_host_dict(
            _parse(host_src), _parse(control_src), _parse(self._DDT_SRC),
        )
        store = SchemeStore.build_from(_parse(scheme_src))

        # Build a tiny suite XML and resolve it.
        suite_xml = (
            '<?xml version="1.0" encoding="UTF-8"?>\n'
            '<suite name="ddt_dim_suite" version="1.0">\n'
            '  <group name="run_grp"><scheme>ddt_dim_user</scheme></group>\n'
            '</suite>\n'
        )
        with tempfile.TemporaryDirectory() as tmpdir:
            xml_path = os.path.join(tmpdir, 's.xml')
            with open(xml_path, 'w') as fh:
                fh.write(suite_xml)
            from generator.suite_xml import parse_suite_xml
            import logging
            logger = logging.getLogger('ddt_dim_e2e')
            suite = parse_suite_xml(xml_path, tmpdir, logger,
                                    skip_validation=True)
            suite_resolution = resolve_suite(suite, store, hd)

            # Inspect the resolved scheme arg's subscript.
            run_call = list(iter_phase_calls(suite_resolution.groups[0].phase_calls['run']))[0]
            temp_arg = [a for a in run_call.args
                        if a.scheme_local_name == 'temp'][0]
            self.assertIn('physics%Model%levs', temp_arg.subscript)
            # Leaf name must not appear bare anywhere in the call expr.
            self.assertNotIn(', 1:levs)', temp_arg.call_expr)

            # Now emit the group cap and inspect the USE clause.
            from generator.group_cap import _generate_group_cap
            group_lines = _generate_group_cap(
                suite_name=suite_resolution.suite_name,
                group_name=suite_resolution.groups[0].group_name,
                resolved_group=suite_resolution.groups[0], host_dict=hd,
            )
            group_text = '\n'.join(group_lines)
        # Pull the host-module USE line.  Must import ``physics``,
        # must NOT import the bare leaf ``levs``.
        import re as _re
        host_use_lines = [
            ln for ln in group_text.splitlines()
            if _re.search(r'use\s+scm_host_mod\b', ln)
        ]
        self.assertTrue(host_use_lines,
                        "group cap missing ``use scm_host_mod`` line")
        joined = ' '.join(host_use_lines)
        self.assertIn('physics', joined)
        # Word-boundary check so we don't false-match a substring like
        # ``physics`` containing ``levs`` etc.
        self.assertIsNone(_re.search(r'\blevs\b', joined),
                          "group cap leaked DDT-leaf ``levs`` into USE: "
                          + joined)
        self.assertIsNone(_re.search(r'\bModel\b', joined),
                          "group cap leaked DDT-mid-component ``Model`` "
                          "into USE: " + joined)


########################################################################
# Doctest loader
########################################################################

########################################################################
# validate_init_dimensions: a non-allocatable suite var dimensioned by a
# scheme-updated-after-register quantity must be allocatable.
########################################################################

class TestValidateInitDimensions(unittest.TestCase):
    """Reject a non-allocatable suite var whose dimension is written by a
    scheme in a phase after register (capgen can't size it at init).
    Regression for the rrtmgp pint_day OOM."""

    @staticmethod
    def _arg(std, intent):
        return types.SimpleNamespace(standard_name=std, intent=intent)

    @classmethod
    def _sr(cls, phase_writes, suite_vars):
        # phase_writes: {phase: [(std, intent), ...]}; suite_vars: [(std, alloc, dims)]
        calls = []
        pc = {}
        for phase, writes in phase_writes.items():
            pc[phase] = [ResolvedCall(scheme_name='s', phase=phase,
                                      args=[cls._arg(s, i) for s, i in writes])]
        grp = types.SimpleNamespace(phase_calls=pc)
        svs = {s: types.SimpleNamespace(standard_name=s, allocatable=a, dimensions=d)
               for s, a, d in suite_vars}
        return types.SimpleNamespace(groups=[grp], suite_vars=svs)

    def test_late_written_dim_nonalloc_raises(self):
        sr = self._sr(
            {'timestep_init': [('mydim', 'out')], 'run': [('myarr', 'out')]},
            [('mydim', False, []), ('myarr', False, ['mydim'])],
        )
        with self.assertRaisesRegex(CCPPError, "myarr.*mydim|allocatable"):
            validate_init_dimensions(sr)

    def test_register_written_dim_ok(self):
        sr = self._sr(
            {'register': [('mydim', 'out')], 'run': [('myarr', 'out')]},
            [('mydim', False, []), ('myarr', False, ['mydim'])],
        )
        validate_init_dimensions(sr)  # no raise

    def test_allocatable_var_skipped(self):
        sr = self._sr(
            {'timestep_init': [('mydim', 'out')], 'run': [('myarr', 'out')]},
            [('mydim', False, []), ('myarr', True, ['mydim'])],
        )
        validate_init_dimensions(sr)  # allocatable -> not capgen's to size

    def test_host_static_dim_ok(self):
        # 'hostdim' is never written by a scheme -> assumed available at init.
        sr = self._sr(
            {'run': [('myarr', 'out')]},
            [('myarr', False, ['hostdim'])],
        )
        validate_init_dimensions(sr)  # no raise

    def test_range_dimension_token_checked(self):
        sr = self._sr(
            {'timestep_init': [('mydim', 'inout')], 'run': [('myarr', 'out')]},
            [('mydim', False, []), ('myarr', False, ['ccpp_constant_one:mydim'])],
        )
        with self.assertRaises(CCPPError):
            validate_init_dimensions(sr)


def load_tests(loader, tests, ignore):
    import generator.suite_resolver as suite_resolution
    import generator.group_cap as gc
    tests.addTests(doctest.DocTestSuite(suite_resolution))
    tests.addTests(doctest.DocTestSuite(gc))
    return tests


if __name__ == '__main__':
    unittest.main(verbosity=2)
