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
    _substitute_instance_idx,
    _translate_active_expr,
    _root_symbol,
    _local_name_conflict,
    _resolve_one_arg,
    _dedup_scheme_names,
    resolve_suite,
    iter_phase_calls,
    SuiteVar,
    ResolvedArg,
    ResolvedCall,
    ResolvedGroup,
    ResolvedSubcycle,
    SuiteResolution,
)
from generator.group_cap import (
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
        sv = self._scheme_var('im', 'horizontal_loop_extent', 'in', 'count')
        arg = _resolve_one_arg(sv, 'run', hd, {}, 'my_scheme', set())
        self.assertEqual(arg.source, 'host')
        self.assertEqual(arg.transform_case, 1)
        self.assertEqual(arg.call_expr, 'im')
        self.assertFalse(arg.needs_transform)

    def test_case1_control_var(self):
        """Control variable → source='control', no USE module."""
        hd = self._host_dict()
        sv = self._scheme_var('thread_num', 'thread_number', 'in', '1')
        arg = _resolve_one_arg(sv, 'run', hd, {}, 'my_scheme', set())
        self.assertEqual(arg.source, 'control')
        self.assertIsNone(arg.module_name)

    def test_case1_2d_array_run(self):
        """2D array in run phase → subscript applied."""
        hd = self._host_dict()
        sv = self._scheme_var('temp', 'air_temperature', 'inout', 'K',
                              '(horizontal_loop_extent, vertical_layer_dimension)',
                              'real', 'kind_phys')
        arg = _resolve_one_arg(sv, 'run', hd, {}, 'my_scheme', set())
        # access_path = 'gt0', subscript = '(lb:ub, 1:nlev)'
        self.assertEqual(arg.call_expr, 'gt0(lb:ub, 1:nlev)')
        self.assertEqual(arg.transform_case, 1)

    def test_case2_suite_owned(self):
        """Case 2: not in host, first use intent(out) → creates SuiteVar."""
        hd = self._host_dict()
        sv = self._scheme_var('new_var', 'brand_new_standard_name', 'out', 'K',
                              '()', 'real', 'kind_phys')
        suite_vars: dict = {}
        arg = _resolve_one_arg(sv, 'run', hd, suite_vars, 'my_scheme', set())
        self.assertEqual(arg.source, 'suite')
        self.assertIn('brand_new_standard_name', suite_vars)
        self.assertIsNotNone(arg.suite_var)

    def test_case3_not_found_intent_in_raises(self):
        """Case 3: not in host, intent(in) → CCPPError."""
        hd = self._host_dict()
        sv = self._scheme_var('missing', 'totally_missing_stdname', 'in', 'K')
        with self.assertRaises(CCPPError) as cm:
            _resolve_one_arg(sv, 'run', hd, {}, 'bad_scheme', set())
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

        sv = self._scheme_var('p_hpa', 'air_pressure', 'in', 'hPa', '()', 'real', 'kind_phys')
        arg = _resolve_one_arg(sv, 'run', combined, {}, 'my_scheme', set())
        self.assertTrue(arg.needs_unit_transform)
        self.assertEqual(arg.transform_case, 3)
        self.assertIn('temp_name', arg.__dataclass_fields__)  # has temp_name field
        self.assertTrue(arg.temp_name)

    def test_no_transform_same_units(self):
        """Identical units → no transformation."""
        hd = self._host_dict()
        sv = self._scheme_var('im', 'horizontal_loop_extent', 'in', 'count')
        arg = _resolve_one_arg(sv, 'run', hd, {}, 'my_scheme', set())
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
        sv = self._scheme_var('v', 'some_value', 'in', 'abc_unit', '()', 'real', 'kind_phys')
        with self.assertRaises(CCPPError) as cm:
            _resolve_one_arg(sv, 'run', hd, {}, 'bad_scheme', set())
        self.assertIn('xyz_unit', str(cm.exception))

    def test_optional_sets_ptr_name(self):
        """Optional argument → ptr_name set."""
        hd = self._host_dict()
        sv = self._scheme_var('im', 'horizontal_loop_extent', 'in', 'count',
                              optional=True)
        arg = _resolve_one_arg(sv, 'run', hd, {}, 'my_scheme', set())
        self.assertTrue(arg.is_optional)
        self.assertTrue(arg.ptr_name)
        self.assertEqual(arg.transform_case, 2)


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
[ im ]
  standard_name = horizontal_loop_extent
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
        sv = MetaVar('temp', ctx)
        sv.set_attr('standard_name', 'air_temperature', ctx)
        sv.set_attr('units', scheme_units, ctx)
        sv.set_attr('dimensions',
                    '(horizontal_loop_extent, vertical_layer_dimension)', ctx)
        sv.set_attr('type', 'real', ctx)
        sv.set_attr('kind', 'kind_phys', ctx)
        sv.set_attr('intent', intent, ctx)
        if scheme_top_at_one:
            sv.set_attr('top_at_one', 'True', ctx)
        return hd, sv

    def test_no_flip_when_both_false(self):
        hd, sv = self._build_host_and_scheme(False, False)
        arg = _resolve_one_arg(sv, 'run', hd, {}, 'sch', set())
        self.assertFalse(arg.needs_vert_flip)
        self.assertEqual(arg.transform_case, 1)
        self.assertEqual(arg.call_expr, 'gt0(lb:ub, 1:nlev)')

    def test_no_flip_when_both_true(self):
        hd, sv = self._build_host_and_scheme(True, True)
        arg = _resolve_one_arg(sv, 'run', hd, {}, 'sch', set())
        self.assertFalse(arg.needs_vert_flip)
        self.assertEqual(arg.transform_case, 1)
        self.assertEqual(arg.call_expr, 'gt0(lb:ub, 1:nlev)')

    def test_flip_when_host_false_scheme_true(self):
        hd, sv = self._build_host_and_scheme(False, True)
        arg = _resolve_one_arg(sv, 'run', hd, {}, 'sch', set())
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
        hd, sv = self._build_host_and_scheme(True, False)
        arg = _resolve_one_arg(sv, 'run', hd, {}, 'sch', set())
        self.assertTrue(arg.needs_vert_flip)
        self.assertEqual(arg.call_expr, 'gt0(lb:ub, nlev:1:-1)')

    def test_flip_composes_with_unit_conversion(self):
        """Mismatched top_at_one AND a unit conversion → the unit-forward
        formula is applied to the flipped call_expr; the temp pattern is
        a single combined assignment."""
        hd, sv = self._build_host_and_scheme(False, True,
                                              host_units='Pa', scheme_units='hPa')
        arg = _resolve_one_arg(sv, 'run', hd, {}, 'sch', set())
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
        hd, sv = self._build_host_and_scheme(False, True, intent='in')
        arg = _resolve_one_arg(sv, 'run', hd, {}, 'sch', set())
        self.assertTrue(arg.needs_vert_flip)
        self.assertEqual(arg.unit_forward, 'gt0(lb:ub, nlev:1:-1)')
        self.assertEqual(arg.unit_backward, '')

    def test_intent_out_only_emits_backward(self):
        hd, sv = self._build_host_and_scheme(False, True, intent='out')
        arg = _resolve_one_arg(sv, 'run', hd, {}, 'sch', set())
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
        sv = MetaVar('s', ctx)
        sv.set_attr('standard_name', 'some_scalar', ctx)
        sv.set_attr('units', '1', ctx)
        sv.set_attr('dimensions', '()', ctx)
        sv.set_attr('type', 'real', ctx)
        sv.set_attr('kind', 'kind_phys', ctx)
        sv.set_attr('intent', 'in', ctx)
        # Scheme leaves top_at_one at default (False).
        arg = _resolve_one_arg(sv, 'run', hd, {}, 'sch', set())
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
        sv = self._scheme_var_char('msg', 'my_message', 'len=*')
        arg = _resolve_one_arg(sv, 'run', hd, {}, 'sch', set())
        self.assertFalse(arg.needs_kind_transform)
        self.assertEqual(arg.transform_case, 1)
        self.assertEqual(arg.temp_name, '')

    def test_len_match_compatible(self):
        """Same specific len=N in both host and scheme — no transform."""
        hd = self._host_with_char('len=512')
        sv = self._scheme_var_char('msg', 'my_message', 'len=512')
        arg = _resolve_one_arg(sv, 'run', hd, {}, 'sch', set())
        self.assertFalse(arg.needs_kind_transform)

    def test_len_star_in_host_no_error(self):
        """len=* in the host is also fine (assumed-length dummy everywhere)."""
        hd = self._host_with_char('len=*')
        sv = self._scheme_var_char('msg', 'my_message', 'len=*')
        arg = _resolve_one_arg(sv, 'run', hd, {}, 'sch', set())
        self.assertFalse(arg.needs_kind_transform)

    def test_mismatched_specific_lengths_raises(self):
        """Specific len=128 vs len=512 is a metadata error."""
        hd = self._host_with_char('len=512')
        sv = self._scheme_var_char('msg', 'my_message', 'len=128')
        with self.assertRaises(CCPPError) as cm:
            _resolve_one_arg(sv, 'run', hd, {}, 'bad_scheme', set())
        self.assertIn('len=512', str(cm.exception))
        self.assertIn('len=128', str(cm.exception))

    def test_len_star_host_specific_scheme_raises(self):
        """len=* in host but specific len=256 in scheme — error."""
        hd = self._host_with_char('len=*')
        sv = self._scheme_var_char('msg', 'my_message', 'len=256')
        with self.assertRaises(CCPPError) as cm:
            _resolve_one_arg(sv, 'run', hd, {}, 'bad_scheme', set())
        self.assertIn('len=256', str(cm.exception))


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
        sr, _ = self._resolve()
        self.assertEqual(sr.suite_name, 'test_simple')
        self.assertEqual(len(sr.groups), 1)
        self.assertEqual(sr.groups[0].group_name, 'physics')

    def test_run_phase_calls(self):
        sr, _ = self._resolve()
        rg = sr.groups[0]
        self.assertIn('run', rg.phase_calls)
        calls = rg.phase_calls['run']
        self.assertEqual(len(calls), 1)
        self.assertEqual(calls[0].scheme_name, 'temp_calc_adjust')

    def test_run_phase_args(self):
        sr, _ = self._resolve()
        calls = sr.groups[0].phase_calls['run']
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
        sr, _ = self._resolve()
        rg = sr.groups[0]
        self.assertIn('init', rg.phase_calls)
        calls = rg.phase_calls['init']
        self.assertEqual(calls[0].scheme_name, 'temp_calc_adjust')

    def test_init_phase_horizontal_subscript(self):
        """In init phase, scalar horizontal_dimension does not produce an lb:ub slice."""
        sr, _ = self._resolve()
        rg = sr.groups[0]
        # temp_calc_adjust_init doesn't have temp, but the general rule should hold
        # for any 2D variable in a non-run phase: no lb:ub slice in init args.
        # (Scalar horizontal_dimension args are synthesised as (ub - lb + 1),
        # which collapses to ncols in non-run phases but never contains
        # the substring 'lb:ub'.)
        calls = rg.phase_calls.get('init', [])
        for rc in calls:
            for arg in rc.args:
                self.assertNotIn('lb:ub', arg.call_expr)

    def test_no_suite_vars(self):
        """All variables in temp_calc_adjust are provided by the host."""
        sr, _ = self._resolve()
        self.assertEqual(sr.suite_vars, {})

    def test_used_modules(self):
        sr, _ = self._resolve()
        calls = sr.groups[0].phase_calls['run']
        mods = calls[0].used_modules
        # host_phys should appear (air_temperature, horizontal_dimension, etc.)
        self.assertIn('host_phys', mods)

    def test_control_args_no_module(self):
        sr, _ = self._resolve()
        calls = sr.groups[0].phase_calls['run']
        ctrl = [a for a in calls[0].args if a.source == 'control']
        for c in ctrl:
            self.assertIsNone(c.module_name)


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
        sr = self._resolve(suite_xml, self._SCHEME_META)
        self.assertIsNotNone(sr.suite_init_call)
        self.assertEqual(sr.suite_init_call.scheme_name, 'init_final_test')
        self.assertEqual(sr.suite_init_call.phase, 'init')
        self.assertIsNone(sr.suite_final_call)

    def test_final_call_attached(self):
        suite_xml = (
            '<?xml version="1.0" encoding="UTF-8"?>\n'
            '<suite name="s" version="2.0">\n'
            '  <group name="g"></group>\n'
            '  <final>init_final_test</final>\n'
            '</suite>\n'
        )
        sr = self._resolve(suite_xml, self._SCHEME_META)
        self.assertIsNone(sr.suite_init_call)
        self.assertIsNotNone(sr.suite_final_call)
        self.assertEqual(sr.suite_final_call.scheme_name, 'init_final_test')
        self.assertEqual(sr.suite_final_call.phase, 'final')

    def test_both_attached(self):
        suite_xml = (
            '<?xml version="1.0" encoding="UTF-8"?>\n'
            '<suite name="s" version="2.0">\n'
            '  <init>init_final_test</init>\n'
            '  <group name="g"></group>\n'
            '  <final>init_final_test</final>\n'
            '</suite>\n'
        )
        sr = self._resolve(suite_xml, self._SCHEME_META)
        self.assertIsNotNone(sr.suite_init_call)
        self.assertIsNotNone(sr.suite_final_call)

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
        sr = self._resolve()
        run_calls = list(iter_phase_calls(sr.groups[0].phase_calls['run']))
        self.assertEqual(len(run_calls), 2)
        self.assertEqual(
            [c.scheme_name for c in run_calls],
            ['temp_calc_adjust', 'temp_calc_adjust'],
        )

    def test_init_phase_dedupes(self):
        sr = self._resolve()
        init_calls = list(iter_phase_calls(sr.groups[0].phase_calls['init']))
        self.assertEqual(len(init_calls), 1)
        self.assertEqual(init_calls[0].scheme_name, 'temp_calc_adjust')

    def test_final_phase_dedupes(self):
        sr = self._resolve()
        final_calls = list(iter_phase_calls(sr.groups[0].phase_calls['final']))
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

    def test_horizontal_loop_extent_uses_chunk_bounds(self):
        # Same special case for the alternative dim std name.
        self.assertEqual(
            _dim_decl_local(['horizontal_loop_extent'], self.hd),
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
        rc = ResolvedCall(scheme_name='s', phase='run', args=args)
        rg = MagicMock()
        rg.phase_calls = {'run': [rc]}
        return rg

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
        sr = resolve_suite(suite, store, hd)
        rg = sr.groups[0]
        lines = _generate_group_cap('test_simple', 'physics', rg, hd)
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
        self.assertIn('public :: ccpp_test_simple_physics_run', text)

    def test_contains_block(self):
        lines = self._resolve_and_generate()
        self.assertIn('contains', lines)

    def test_run_subroutine(self):
        lines = self._resolve_and_generate()
        text = '\n'.join(lines)
        self.assertIn('subroutine ccpp_test_simple_physics_run', text)
        self.assertIn('end subroutine ccpp_test_simple_physics_run', text)

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
        self.assertIn('subroutine ccpp_test_simple_physics_init', text)
        self.assertIn('call temp_calc_adjust_init', text)

    def test_write_group_cap(self):
        """write_group_cap writes the file and returns its path."""
        hd = _load_full_host_dict()
        store = _load_scheme_store()
        suite = _parse_suite('suite_test_simple.xml')
        sr = resolve_suite(suite, store, hd)
        rg = sr.groups[0]
        with tempfile.TemporaryDirectory() as tmpdir:
            path = write_group_cap('test_simple', 'physics', rg, hd, tmpdir)
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
        sr = self._resolve_subcycle()
        rg = sr.groups[0]
        run_items = rg.phase_calls['run']
        self.assertEqual(len(run_items), 1)
        self.assertIsInstance(run_items[0], ResolvedSubcycle)

    def test_subcycle_loop_count(self):
        sr = self._resolve_subcycle()
        sub = sr.groups[0].phase_calls['run'][0]
        self.assertEqual(sub.loop, '3')

    def test_subcycle_contains_scheme(self):
        sr = self._resolve_subcycle()
        sub = sr.groups[0].phase_calls['run'][0]
        self.assertEqual(len(sub.calls), 1)
        self.assertEqual(sub.calls[0].scheme_name, 'temp_calc_adjust')

    def test_init_phase_is_flat(self):
        """Init phase flattens subcycles — no ResolvedSubcycle in init."""
        sr = self._resolve_subcycle()
        rg = sr.groups[0]
        for item in rg.phase_calls.get('init', []):
            self.assertNotIsInstance(item, ResolvedSubcycle)

    def test_iter_phase_calls_flattens(self):
        sr = self._resolve_subcycle()
        rg = sr.groups[0]
        all_calls = list(iter_phase_calls(rg.phase_calls['run']))
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
        sr = self._resolve_nested(suite_xml)
        run = sr.groups[0].phase_calls['run']
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
        sr = self._resolve_nested(suite_xml)
        run = sr.groups[0].phase_calls['run']
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
        sr = self._resolve_nested(suite_xml)
        run = sr.groups[0].phase_calls['run']
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
        sr     = resolve_suite(suite, store, hd)
        rg     = sr.groups[0]
        self.lines = _generate_group_cap('test_subcycle', 'physics', rg, hd)
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
        self.assertNotIn('do ccpp_loop_counter', self.text.split('subroutine ccpp_test_subcycle_physics_init')[1].split('end subroutine')[0])


########################################################################
# Tests: state machine in group cap
########################################################################

class TestStateMachineGroupCap(unittest.TestCase):

    def setUp(self):
        hd    = _load_full_host_dict()
        store = _load_scheme_store()
        suite = _parse_suite('suite_test_simple.xml')
        sr    = resolve_suite(suite, store, hd)
        rg    = sr.groups[0]
        self.lines = _generate_group_cap('test_simple', 'physics', rg, hd)
        self.text  = '\n'.join(self.lines)

    def test_state_constants_declared(self):
        self.assertIn('CCPP_GROUP_UNINITIALIZED = 0', self.text)
        self.assertIn('CCPP_GROUP_INITIALIZED   = 1', self.text)
        self.assertIn('CCPP_GROUP_IN_TIMESTEP   = 2', self.text)

    def test_state_array_declared(self):
        self.assertIn('integer, private, allocatable :: ccpp_group_state(:)', self.text)

    def test_state_alloc_public(self):
        self.assertIn('public :: ccpp_test_simple_physics_state_alloc', self.text)

    def test_state_dealloc_public(self):
        self.assertIn('public :: ccpp_test_simple_physics_state_dealloc', self.text)

    def test_init_idempotent_skip(self):
        # init returns silently when already INITIALIZED.
        self.assertIn(
            'if (ccpp_group_state(inst_num) == CCPP_GROUP_INITIALIZED) return',
            self.text,
        )

    def test_init_errors_on_invalid_state(self):
        # init must error if the state is anything other than UNINITIALIZED
        # or INITIALIZED (idempotent skip).
        init_sub = self.text.split('subroutine ccpp_test_simple_physics_init')[1]
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
        run_sub = self.text.split('subroutine ccpp_test_simple_physics_run')[1]
        run_sub = run_sub.split('end subroutine')[0]
        self.assertIn(
            'ccpp_group_state(inst_num) /= CCPP_GROUP_IN_TIMESTEP', run_sub
        )
        self.assertIn('errflg = 1', run_sub)

    def test_state_alloc_subroutine(self):
        # state_alloc always takes number_of_instances as explicit arg.
        self.assertIn(
            'subroutine ccpp_test_simple_physics_state_alloc(number_of_instances, errmsg, errflg)',
            self.text,
        )
        self.assertIn('allocate(ccpp_group_state(number_of_instances))', self.text)

    def test_ninstances_not_used_in_group_cap(self):
        # number_of_instances is no longer USEd by the group cap module;
        # it is passed as an explicit argument to state_alloc instead.
        preamble = self.text.split('contains')[0]
        self.assertNotIn('ninstances', preamble)

    def test_state_dealloc_subroutine(self):
        self.assertIn('subroutine ccpp_test_simple_physics_state_dealloc(errmsg, errflg)', self.text)
        self.assertIn('if (allocated(ccpp_group_state)) deallocate(ccpp_group_state)', self.text)

    def test_inst_num_in_init_args(self):
        # inst_num (the local name for instance_number) must be a dummy arg of init.
        init_sub = self.text.split('subroutine ccpp_test_simple_physics_init')[1]
        init_sub = init_sub.split('end subroutine')[0]
        self.assertIn('inst_num', init_sub)

    def test_inst_num_in_final_args(self):
        final_sub = self.text.split('subroutine ccpp_test_simple_physics_final')[1]
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
        sr    = resolve_suite(suite, store, hd)
        # Pass host_dict so number_of_instances flows through.
        lines = _generate_suite_cap('test_simple', sr, store, hd)
        self.text = '\n'.join(lines)

    def test_init_calls_state_alloc_with_ninstances(self):
        # host_full.meta has ninstances → number_of_instances.
        self.assertIn(
            'call ccpp_test_simple_physics_state_alloc(ninstances, errmsg, errflg)', self.text
        )

    def test_init_subroutine_has_ninstances_arg(self):
        init_sub = self.text.split('subroutine test_simple_init')[1].split('end subroutine')[0]
        self.assertIn('ninstances', init_sub)

    def test_final_calls_state_dealloc(self):
        self.assertIn(
            'call ccpp_test_simple_physics_state_dealloc(errmsg, errflg)', self.text
        )

    def test_state_alloc_imported_in_suite_cap(self):
        self.assertIn('ccpp_test_simple_physics_state_alloc', self.text.split('contains')[0])

    def test_state_dealloc_imported_in_suite_cap(self):
        self.assertIn('ccpp_test_simple_physics_state_dealloc', self.text.split('contains')[0])


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
        sr    = resolve_suite(suite, store, hd)
        lines = _generate_suite_cap('test_simple', sr, store, hd)
        self.text = '\n'.join(lines)

    def test_init_calls_state_alloc_with_literal_1(self):
        self.assertIn(
            'call ccpp_test_simple_physics_state_alloc(1, errmsg, errflg)', self.text
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
        self.sr     = resolve_suite(self.suite, self.store, self.hd)

    def test_dim_inter_promoted_to_suite_var(self):
        # The register-phase intent=out arg becomes a suite-owned variable.
        self.assertIn(
            'dimension_for_interstitial_variable', self.sr.suite_vars,
        )
        sv = self.sr.suite_vars['dimension_for_interstitial_variable']
        self.assertEqual(sv.type_, 'integer')
        self.assertEqual(sv.dimensions, [])
        self.assertEqual(sv.source_phase, 'register')

    def test_interstitial_var_promoted_to_suite_var(self):
        # The run-phase intent=out array also becomes a suite var, dimensioned
        # by the register-set scalar.
        self.assertIn(
            'output_only_interstitial_variable', self.sr.suite_vars,
        )
        sv = self.sr.suite_vars['output_only_interstitial_variable']
        self.assertEqual(sv.dimensions, ['dimension_for_interstitial_variable'])

    def test_register_phase_call_resolved(self):
        # Group's register phase has a ResolvedCall for the producer scheme.
        rg = self.sr.groups[0]
        register_calls = list(iter_phase_calls(rg.phase_calls.get('register', [])))
        self.assertEqual(len(register_calls), 1)
        self.assertEqual(register_calls[0].scheme_name, 'register_dim_producer')

    def test_run_phase_dim_resolves_via_suite_var(self):
        # The run-phase consumer call's interstitial_var arg's call_expr
        # must reference ccpp_suite_data(...)%dim_inter as the upper bound.
        rg = self.sr.groups[0]
        run_calls = list(iter_phase_calls(rg.phase_calls.get('run', [])))
        consumer = next(rc for rc in run_calls
                        if rc.scheme_name == 'register_dim_consumer')
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
        self.sr    = resolve_suite(self.suite, self.store, self.hd)
        self.text  = '\n'.join(
            _generate_suite_cap('reg_dim', self.sr, self.store, self.hd)
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
        'capgen-ng', 'src', 'ccpp_constituent_prop_mod.meta',
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
        self.sr    = resolve_suite(self.suite, self.store, self.hd)

    def test_constituent_register_calls_recorded(self):
        self.assertEqual(
            self.sr.constituent_register_calls,
            [('register_constituents', 'dyn_const')],
        )

    def test_constituent_arg_not_promoted_to_suite_var(self):
        # The constituent array is per-scheme transient — never a SuiteVar.
        self.assertNotIn(
            'dynamic_constituents_for_register_test', self.sr.suite_vars,
        )

    def test_constituent_arg_marked(self):
        rg = self.sr.groups[0]
        register_call = list(iter_phase_calls(rg.phase_calls['register']))[0]
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
        sr = resolve_suite(suite, store, hd)
        # The register-phase scheme is still recorded for the suite cap to
        # populate the per-suite dynamic-constituent buffer.
        self.assertEqual(
            sr.constituent_register_calls,
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
        self.sr    = resolve_suite(self.suite, self.store, self.hd)
        self.text  = '\n'.join(
            _generate_suite_cap('reg_consts', self.sr, self.store, self.hd)
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

    def test_two_pass_packs_into_buffer(self):
        register_body = self.text.split('subroutine reg_consts_register')[1].split(
            'end subroutine reg_consts_register'
        )[0]
        self.assertIn('First pass: count', register_body)
        self.assertIn('Second pass: copy into per-suite buffer', register_body)
        # The constituent-providing scheme is called twice (one per pass).
        self.assertEqual(
            register_body.count('call register_constituents_register'), 2,
        )

    def test_buffer_allocate(self):
        self.assertIn(
            'allocate(reg_consts_dynamic_constituents(num_consts))', self.text,
        )

    def test_buffer_populate_loop(self):
        self.assertIn(
            'reg_consts_dynamic_constituents(num_consts + i) = scheme_consts(i)',
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
        self.sr    = resolve_suite(self.suite, self.store, self.hd)
        run_calls  = list(iter_phase_calls(self.sr.groups[0].phase_calls['run']))
        self.run_args = {a.scheme_local_name: a for a in run_calls[0].args}

    def test_uses_constituents_flag_set(self):
        self.assertTrue(self.sr.uses_constituents)

    def test_constituent_index_names_enumerated(self):
        # Both the base read and the tendency write reference the same
        # base std name.
        self.assertEqual(
            self.sr.constituent_index_names,
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
        # in static_api tests; here we just confirm the source).
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
    the introspection routines in :mod:`generator.static_api` can list
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
        # resolver routes through capgen-ng's auto-provisioning path.
        from generator.suite_resolver import _resolve_constituent_arg
        hd = _load_full_host_dict()
        sv = self._scheme_var(
            'consts', 'ccpp_constituents',
            '(horizontal_dimension, vertical_layer_dimension, '
            'number_of_ccpp_constituents)',
            intent='in',
        )
        arg = _resolve_constituent_arg(
            sv, 'run', hd, {}, 'consts_user', 'mysuite',
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
        sv = self._scheme_var(
            'cldliq', 'cloud_liquid_water_mixing_ratio',
            '(horizontal_dimension, vertical_layer_dimension)',
            intent='in',
        )
        sv.set_attr('advected', 'True', _ctx())
        arg = _resolve_constituent_arg(
            sv, 'run', hd, {}, 'cldliq_user', 'mysuite',
        )
        self.assertIsNotNone(arg)
        self.assertEqual(arg.used_const_dim_std_names, set())


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
        '  dimensions = (horizontal_loop_extent, vertical_layer_dimension)\n'
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

    def test_tendency_intent_in_raises(self):
        # constituent flag + intent=in on a tendency_of_* std name → reject.
        with self.assertRaises(CCPPError) as ctx:
            self._resolve('tendency_of_air_temperature', 'in', 'constituent')
        self.assertIn('intent=out', str(ctx.exception))

    def test_tendency_intent_inout_raises(self):
        with self.assertRaises(CCPPError) as ctx:
            self._resolve('tendency_of_air_temperature', 'inout', 'constituent')
        self.assertIn('intent=out', str(ctx.exception))


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
        sv = self._scheme_var(
            'ntcw',
            'index_of_cloud_liquid_water_mixing_ratio_in_tracer_concentration_array',
            intent='in',
        )
        arg = _resolve_one_arg(sv, 'run', hd, {}, 'gfs_mp_generic_pre', set())
        # Host metadata wins: source=host, short local name, NO leakage of
        # the long std_name into ccpp_host_constituents.
        self.assertEqual(arg.source, 'host')
        self.assertEqual(arg.call_expr, 'ntcw')
        self.assertIsNotNone(arg.host_entry)
        self.assertEqual(arg.host_entry.local_name, 'ntcw')

    def test_unclaimed_index_of_still_routes_to_constituents(self):
        """The framework auto-provisioning path is preserved for
        ``index_of_<X>`` names the host does NOT declare — required for
        capgen-ng-owned constituent flows (cf. the advection e2e test)."""
        hd = build_flat_host_dict(_parse(self._HOST_SRC), [], [])
        sv = self._scheme_var(
            'idx_other', 'index_of_some_other_constituent_not_in_host',
            intent='in',
        )
        arg = _resolve_one_arg(sv, 'run', hd, {}, 'some_scheme', set())
        self.assertEqual(arg.source, 'constituent')
        self.assertEqual(arg.call_expr,
                         'index_of_some_other_constituent_not_in_host')


########################################################################
# Doctest loader
########################################################################

def load_tests(loader, tests, ignore):
    import generator.suite_resolver as sr
    import generator.group_cap as gc
    tests.addTests(doctest.DocTestSuite(sr))
    tests.addTests(doctest.DocTestSuite(gc))
    return tests


if __name__ == '__main__':
    unittest.main(verbosity=2)
