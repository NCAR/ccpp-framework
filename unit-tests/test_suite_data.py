"""Unit tests for generator.suite_data."""

import doctest
import os
import tempfile
import unittest

from metadata.parse_tools import CCPPError
from generator.suite_data import (
    _generate_suite_data,
    _collect_dim_uses,
    _dim_local_expr,
    write_suite_data,
)
from generator.suite_resolver import SuiteVar


def _make_sv(std_name, local='myvar', type_='real', kind='kind_phys',
             units='K', dims=None, scheme='sch', phase='run'):
    return SuiteVar(
        standard_name=std_name,
        local_name=local,
        type_=type_,
        kind=kind,
        units=units,
        dimensions=dims or [],
        source_scheme=scheme,
        source_phase=phase,
    )


class TestGenerateSuiteDataEmpty(unittest.TestCase):
    """Suite with no suite-owned variables."""

    def setUp(self):
        self.lines = _generate_suite_data('mysuite', {})
        self.text  = '\n'.join(self.lines)

    def test_module_header_comment(self):
        self.assertTrue(self.lines[0].startswith('!'))
        self.assertIn('mysuite', self.lines[0])

    def test_module_declaration(self):
        self.assertIn('module ccpp_mysuite_data', self.text)
        self.assertIn('end module ccpp_mysuite_data', self.text)

    def test_implicit_none_private(self):
        self.assertIn('implicit none', self.text)
        self.assertIn('private', self.text)

    def test_ddt_type_name(self):
        self.assertIn('type, public :: ccpp_mysuite_data_t', self.text)
        self.assertIn('end type ccpp_mysuite_data_t', self.text)

    def test_empty_ddt_comment(self):
        self.assertIn('no suite-owned variables', self.text)

    def test_module_instance(self):
        self.assertIn(
            'type(ccpp_mysuite_data_t), allocatable, target, public :: ccpp_suite_data(:)',
            self.text,
        )

    def test_no_trailing_newlines(self):
        for line in self.lines:
            self.assertNotIn('\n', line)


class TestGenerateSuiteDataWithVars(unittest.TestCase):
    """Suite with suite-owned variables."""

    def setUp(self):
        suite_vars = {
            'air_temp_adjusted': _make_sv(
                'air_temp_adjusted', 'temp_adj', 'real', 'kind_phys', 'K',
                dims=['horizontal_dimension', 'vertical_layer_dimension'],
            ),
            'humidity': _make_sv(
                'humidity', 'q', 'real', 'kind_phys', 'kg kg-1',
                dims=['horizontal_dimension'],
            ),
        }
        self.lines = _generate_suite_data('suite_x', suite_vars)
        self.text  = '\n'.join(self.lines)

    def test_fields_present(self):
        self.assertIn('temp_adj', self.text)
        self.assertIn('q', self.text)

    def test_allocatable_arrays(self):
        # Array fields should be allocatable.
        self.assertIn('allocatable', self.text)

    def test_real_kind(self):
        self.assertIn('real(kind=kind_phys)', self.text)

    def test_uses_ccpp_kinds(self):
        # Suite vars referencing ``kind_phys`` must USE it from ccpp_kinds.
        self.assertIn('use ccpp_kinds, only: kind_phys', self.text)

    def test_no_empty_comment(self):
        self.assertNotIn('no suite-owned variables', self.text)

    def test_fields_sorted(self):
        # Sorted by standard_name: air_temp_adjusted before humidity.
        idx_temp = self.text.index('temp_adj')
        idx_q    = self.text.index(' q')
        self.assertLess(idx_temp, idx_q)

    def test_components_have_no_target_attribute(self):
        """Fortran does NOT allow ``target`` as a derived-type component
        attribute; the TARGET attribute lives on the outer instance array
        instead.  See :func:`test_instance_array_is_target`."""
        # No component-level ``, target`` substring should appear on a
        # field declaration line.  We check the strict patterns we'd
        # emit if this regressed.
        self.assertNotIn('allocatable, target :: temp_adj', self.text)
        self.assertNotIn('allocatable, target :: q', self.text)

    def test_instance_array_is_target(self):
        """The module-level instance array carries TARGET so every
        ``ccpp_suite_data(i)%component(...)`` subobject is a valid
        pointer-assignment target (used by the group cap to pointer-assign
        optional-arg wrappers and transformation temporaries).
        """
        self.assertIn(
            'type(ccpp_suite_x_data_t), allocatable, target, public :: '
            'ccpp_suite_data(:)',
            self.text,
        )


class TestGenerateSuiteDataScalar(unittest.TestCase):
    """Suite var that is a scalar (no dimensions)."""

    def setUp(self):
        suite_var = _make_sv('flag_var', 'flag', 'logical', '', '1', dims=[])
        self.lines = _generate_suite_data('s', {'flag_var': suite_var})
        self.text  = '\n'.join(self.lines)

    def test_no_allocatable_for_scalar(self):
        # Scalar fields should NOT be allocatable; only the outer instance array is.
        type_body = self.text.split('type, public ::')[1].split('end type')[0]
        self.assertNotIn('allocatable', type_body)

    def test_logical_type(self):
        self.assertIn('logical', self.text)

    def test_no_ccpp_kinds_use_when_no_kinded_vars(self):
        # No real(kind=...) vars → no ``use ccpp_kinds`` should be emitted.
        self.assertNotIn('use ccpp_kinds', self.text)


class TestGenerateSuiteDataDDT(unittest.TestCase):
    """Suite-owned variable whose type is a DDT defined in a scheme module."""

    def setUp(self):
        self.suite_vars = {
            'volume_mixing_ratio_ddt': _make_sv(
                'volume_mixing_ratio_ddt', 'vmr', 'vmr_type', '', 'none',
                dims=[], scheme='make_ddt',
            ),
        }
        self.ddt_module_map = {'vmr_type': 'make_ddt'}

    def test_emits_use_for_ddt_module(self):
        lines = _generate_suite_data(
            'ddt_suite', self.suite_vars,
            ddt_module_map=self.ddt_module_map,
        )
        text = '\n'.join(lines)
        self.assertIn('use make_ddt, only: vmr_type', text)
        # Components carry the TARGET attribute so group caps can
        # pointer-assign into them (see suite_data.py docstring).
        self.assertIn('type(vmr_type) :: vmr', text)

    def test_use_appears_before_implicit_none(self):
        lines = _generate_suite_data(
            'ddt_suite', self.suite_vars,
            ddt_module_map=self.ddt_module_map,
        )
        idx_use = next(i for i, l in enumerate(lines) if 'use make_ddt' in l)
        idx_impl = next(i for i, l in enumerate(lines) if 'implicit none' in l)
        self.assertLess(idx_use, idx_impl)

    def test_handles_type_paren_form(self):
        suite_vars = {
            'wrapped_ddt': _make_sv(
                'wrapped_ddt', 'w', 'type(my_type)', '', 'none', dims=[],
            ),
        }
        lines = _generate_suite_data(
            'ddt_suite', suite_vars,
            ddt_module_map={'my_type': 'wrap_mod'},
        )
        text = '\n'.join(lines)
        self.assertIn('use wrap_mod, only: my_type', text)

    def test_missing_ddt_module_raises(self):
        with self.assertRaisesRegex(CCPPError, "vmr_type"):
            _generate_suite_data(
                'ddt_suite', self.suite_vars,
                ddt_module_map={},
            )

    def test_no_ddt_no_use(self):
        # When suite vars are all intrinsic, no DDT USE lines are emitted.
        suite_var = _make_sv('temp', 't', 'real', 'kind_phys', 'K', dims=[])
        lines = _generate_suite_data(
            'ds', {'temp': suite_var}, ddt_module_map=None,
        )
        text = '\n'.join(lines)
        self.assertNotIn('use make_ddt', text)
        self.assertIn('use ccpp_kinds, only: kind_phys', text)


class TestConstituentCountDim(unittest.TestCase):
    """Suite-owned var dimensioned by ``number_of_ccpp_constituents``.

    The framework owns the extent, so ``init_fields`` must allocate the field
    via the per-instance constituent object's ``num_layer_vars`` member and USE
    the object's module (``ccpp_host_constituents``).  Regression for the
    CAM-SIMA se_cslam allocate path (Fix B).
    """

    def test_dim_local_expr_resolves_to_constituent_count(self):
        self.assertEqual(
            _dim_local_expr('number_of_ccpp_constituents', {}, {}),
            'ccpp_model_constituents_obj(i)%num_layer_vars',
        )

    def test_collect_dim_uses_adds_constituent_module(self):
        sv = {'workspace': _make_sv(
            'workspace', 'work', dims=['number_of_ccpp_constituents'])}
        uses = _collect_dim_uses(sv, {})
        self.assertEqual(uses.get('ccpp_host_constituents'),
                         ['ccpp_model_constituents_obj'])

    def test_init_fields_allocates_with_constituent_count(self):
        sv = {'workspace': _make_sv(
            'workspace', 'work', dims=['number_of_ccpp_constituents'])}
        text = '\n'.join(_generate_suite_data('cdim', sv, host_dict={}))
        self.assertIn(
            'use ccpp_host_constituents, only: ccpp_model_constituents_obj',
            text)
        self.assertIn(
            'allocate(ccpp_suite_data(i)%work('
            'ccpp_model_constituents_obj(i)%num_layer_vars))',
            text)


class TestWriteSuiteData(unittest.TestCase):

    def test_writes_file(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            path = write_suite_data('s', {}, tmpdir)
            self.assertTrue(os.path.isfile(path))

    def test_filename(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            path = write_suite_data('test_simple', {}, tmpdir)
            self.assertEqual(os.path.basename(path), 'ccpp_test_simple_data.F90')

    def test_file_ends_with_newline(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            path = write_suite_data('s', {}, tmpdir)
            with open(path) as fh:
                self.assertTrue(fh.read().endswith('\n'))

    def test_creates_output_dir(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            subdir = os.path.join(tmpdir, 'newdir')
            write_suite_data('s', {}, subdir)
            self.assertTrue(os.path.isdir(subdir))

    def test_returns_absolute_path(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            path = write_suite_data('s', {}, tmpdir)
            self.assertTrue(os.path.isabs(path))


def load_tests(loader, tests, ignore):
    import generator.suite_data as sd
    tests.addTests(doctest.DocTestSuite(sd))
    return tests


if __name__ == '__main__':
    unittest.main(verbosity=2)
