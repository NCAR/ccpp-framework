"""Tests for the ccpp_datafile query CLI.

Covers each of the 17 CLI flags end-to-end by:
  1. building a real datatable.xml via the writer in generator.datatable,
  2. invoking datatable_report / datatable_pretty_print on it,
  3. asserting the textual output.
"""

import doctest
import os
import shutil
import sys
import tempfile
import unittest

_TESTS_DIR  = os.path.dirname(os.path.abspath(__file__))
_CAPGEN_DIR = os.path.join(os.path.dirname(_TESTS_DIR), 'capgen')
for _p in (_CAPGEN_DIR, _TESTS_DIR):
    if _p not in sys.path:
        sys.path.insert(0, _p)

import ccpp_datafile as cdf
from ccpp_datafile import (
    DatatableReport,
    datatable_pretty_print,
    datatable_report,
)
from generator.datatable import write_datatable

from test_suite_resolver import (
    _load_full_host_dict,
    _load_scheme_store,
    _parse_suite,
)
from generator.suite_resolver import resolve_suite


def _build_datatable(tmpdir,
                    host_file_paths=None, utility_paths=None,
                    suite_file_paths=None, scheme_file_paths=None,
                    dependency_paths=None,
                    suite_meta_paths=None, expanded_sdf_paths=None,
                    protect_first_host_var=False):
    """Build a real datatable.xml in *tmpdir* and return its path."""
    hd    = _load_full_host_dict()
    if protect_first_host_var:
        first = next(iter(hd))
        hd[first].protected = True
    store = _load_scheme_store()
    suite = _parse_suite('suite_test_simple.xml')
    suite_resolution    = resolve_suite(suite, store, hd)
    return write_datatable(
        [suite_resolution],
        store,
        utility_paths or ['/out/ccpp_kinds.F90'],
        suite_file_paths or ['/out/ccpp_test_simple_cap.F90',
                             '/out/ccpp_test_simple_physics_cap.F90'],
        tmpdir,
        host_file_paths=host_file_paths or ['/out/test_host_ccpp_cap.F90'],
        scheme_file_paths=scheme_file_paths,
        dependency_paths=dependency_paths or [],
        suite_meta_paths=suite_meta_paths,
        expanded_sdf_paths=expanded_sdf_paths,
        host_dict=hd,
    )


class _DTBase(unittest.TestCase):
    """Shared fixture: build one datatable.xml per test class."""

    @classmethod
    def setUpClass(cls):
        cls._tmpdir = tempfile.mkdtemp()
        cls._datatable = _build_datatable(cls._tmpdir)

    @classmethod
    def tearDownClass(cls):
        shutil.rmtree(cls._tmpdir)


class TestDatatableReportFileActions(_DTBase):

    def test_host_files(self):
        out = datatable_report(self._datatable,
                               DatatableReport('host_files'), ',')
        self.assertEqual(out, '/out/test_host_ccpp_cap.F90')

    def test_suite_files(self):
        out = datatable_report(self._datatable,
                               DatatableReport('suite_files'), ',')
        items = out.split(',')
        self.assertIn('/out/ccpp_test_simple_cap.F90', items)
        self.assertIn('/out/ccpp_test_simple_physics_cap.F90', items)

    def test_utility_files(self):
        out = datatable_report(self._datatable,
                               DatatableReport('utility_files'), ',')
        self.assertEqual(out, '/out/ccpp_kinds.F90')

    def test_capgen_files_returns_all(self):
        out = datatable_report(self._datatable,
                               DatatableReport('capgen_files'), ',')
        items = out.split(',')
        self.assertIn('/out/ccpp_kinds.F90', items)
        self.assertIn('/out/test_host_ccpp_cap.F90', items)
        self.assertIn('/out/ccpp_test_simple_cap.F90', items)

    def test_separator_honored(self):
        out = datatable_report(self._datatable,
                               DatatableReport('suite_files'), ';')
        self.assertIn(';', out)
        self.assertNotIn(',', out)


class TestDatatableReportInspectionFiles(unittest.TestCase):
    """--inspection-files returns meta + expanded SDF paths and excludes them
    from --capgen-files."""

    def setUp(self):
        self._tmpdir = tempfile.mkdtemp()
        self._datatable = _build_datatable(
            self._tmpdir,
            suite_meta_paths=['/out/ccpp_test_simple_data.meta'],
            expanded_sdf_paths=['/out/ccpp_test_simple_expanded.xml'],
        )

    def tearDown(self):
        shutil.rmtree(self._tmpdir)

    def test_inspection_files_returns_meta_and_expanded(self):
        out = datatable_report(self._datatable,
                               DatatableReport('inspection_files'), ',')
        items = out.split(',')
        self.assertIn('/out/ccpp_test_simple_data.meta', items)
        self.assertIn('/out/ccpp_test_simple_expanded.xml', items)

    def test_inspection_files_excluded_from_capgen_files(self):
        out = datatable_report(self._datatable,
                               DatatableReport('capgen_files'), ',')
        items = out.split(',')
        self.assertNotIn('/out/ccpp_test_simple_data.meta', items)
        self.assertNotIn('/out/ccpp_test_simple_expanded.xml', items)

    def test_inspection_files_empty_when_none_given(self):
        # Build a datatable with no inspection paths; the section is still
        # present, and --inspection-files returns an empty string.
        with tempfile.TemporaryDirectory() as d:
            path = _build_datatable(d)
            out = datatable_report(path,
                                   DatatableReport('inspection_files'), ',')
            self.assertEqual(out, '')


class TestDatatableReportSchemeActions(_DTBase):

    def test_process_list_empty(self):
        # capgen does not emit <scheme process="..."> attrs.
        out = datatable_report(self._datatable,
                               DatatableReport('process_list'), ',')
        self.assertEqual(out, '')

    def test_module_list_includes_scheme_modules(self):
        out = datatable_report(self._datatable,
                               DatatableReport('module_list'), ',')
        modules = out.split(',')
        self.assertIn('temp_calc_adjust', modules)

    def test_dependencies_empty_when_none(self):
        out = datatable_report(self._datatable,
                               DatatableReport('dependencies'), ',')
        self.assertEqual(out, '')


class TestDatatableReportSchemeFiles(unittest.TestCase):
    """--scheme-files returns the used-scheme Fortran source paths from
    <scheme_files>; the section is always present (possibly empty) so the
    query never raises on a vanilla datatable."""

    def setUp(self):
        self._tmpdir = tempfile.mkdtemp()

    def tearDown(self):
        shutil.rmtree(self._tmpdir)

    def test_scheme_files_returns_listed_paths(self):
        path = _build_datatable(
            self._tmpdir,
            scheme_file_paths=['/phys/scheme_b.F90', '/phys/scheme_a.F90'],
        )
        out = datatable_report(path, DatatableReport('scheme_files'), ',')
        items = out.split(',')
        # Writer preserves caller order; do not assume sort.
        self.assertIn('/phys/scheme_b.F90', items)
        self.assertIn('/phys/scheme_a.F90', items)

    def test_scheme_files_empty_when_none_given(self):
        path = _build_datatable(self._tmpdir)
        out = datatable_report(path, DatatableReport('scheme_files'), ',')
        self.assertEqual(out, '')


class TestDatatableReportDependenciesPopulated(unittest.TestCase):
    """--dependencies returns the sorted, dedup'd dependency list."""

    def setUp(self):
        self._tmpdir = tempfile.mkdtemp()
        self._datatable = _build_datatable(
            self._tmpdir,
            dependency_paths=['/dep/b.F90', '/dep/a.F90', '/dep/a.F90'],
        )

    def tearDown(self):
        shutil.rmtree(self._tmpdir)

    def test_dependencies_sorted_dedup(self):
        out = datatable_report(self._datatable,
                               DatatableReport('dependencies'), ',')
        self.assertEqual(out, '/dep/a.F90,/dep/b.F90')


class TestDatatableReportSuiteList(_DTBase):

    def test_suite_list(self):
        out = datatable_report(self._datatable,
                               DatatableReport('suite_list'), ',')
        self.assertEqual(out, 'test_simple')


class TestDatatableReportVariableActions(_DTBase):

    def test_required_variables_includes_call_list_vars(self):
        out = datatable_report(
            self._datatable,
            DatatableReport('required_variables', 'test_simple'), ',')
        names = out.split(',')
        self.assertIn('air_temperature', names)

    def test_input_variables_excludes_out_only(self):
        # An intent=out var must not appear in --input-variables.
        out = datatable_report(
            self._datatable,
            DatatableReport('input_variables', 'test_simple'), ',')
        names = set(out.split(','))
        # ccpp_error_code is intent=out → not in input list.
        self.assertNotIn('ccpp_error_code', names)

    def test_output_variables_excludes_in_only(self):
        out = datatable_report(
            self._datatable,
            DatatableReport('output_variables', 'test_simple'), ',')
        names = set(out.split(','))
        self.assertIn('ccpp_error_code', names)

    def test_host_variables_returns_host_names(self):
        out = datatable_report(self._datatable,
                               DatatableReport('host_variables'), ',')
        names = set(out.split(','))
        self.assertIn('air_temperature', names)

    def test_unknown_suite_returns_empty(self):
        out = datatable_report(
            self._datatable,
            DatatableReport('required_variables', 'no_such_suite'), ',')
        self.assertEqual(out, '')


class TestExcludeProtected(unittest.TestCase):

    def setUp(self):
        self._tmpdir = tempfile.mkdtemp()
        self._datatable = _build_datatable(
            self._tmpdir, protect_first_host_var=True,
        )
        hd = _load_full_host_dict()
        self._protected_name = next(iter(hd))

    def tearDown(self):
        shutil.rmtree(self._tmpdir)

    def test_protected_excluded_from_host_when_flag_on(self):
        out = datatable_report(
            self._datatable,
            DatatableReport('host_variables'), ',', exclude_protected=True)
        names = set(out.split(','))
        self.assertNotIn(self._protected_name, names)

    def test_protected_included_when_flag_off(self):
        out = datatable_report(
            self._datatable,
            DatatableReport('host_variables'), ',', exclude_protected=False)
        names = set(out.split(','))
        self.assertIn(self._protected_name, names)


class TestShowAction(_DTBase):

    def test_show_returns_string(self):
        out = datatable_pretty_print(self._datatable, indent=2, line_wrap=-1)
        self.assertIsInstance(out, str)
        self.assertIn('<ccpp_datatable', out)
        self.assertIn('<capgen_files', out)
        self.assertIn('<schemes', out)
        self.assertIn('<api', out)

    def test_show_indent_changes_output(self):
        out2 = datatable_pretty_print(self._datatable, indent=2, line_wrap=-1)
        # _INDENT_STR is module-level; reset between runs.
        cdf._INDENT_STR = '    '
        out4 = datatable_pretty_print(self._datatable, indent=4, line_wrap=-1)
        cdf._INDENT_STR = '  '
        self.assertNotEqual(out2, out4)


class TestReportValidation(unittest.TestCase):

    def test_invalid_action_raises(self):
        with self.assertRaises(ValueError):
            DatatableReport('not_a_real_action')

    def test_datatable_report_requires_action(self):
        with self.assertRaises(ValueError):
            datatable_report('does-not-matter', None, ',')

    def test_datatable_report_requires_sep(self):
        with tempfile.TemporaryDirectory() as d:
            path = _build_datatable(d)
            with self.assertRaises(ValueError):
                datatable_report(path,
                                 DatatableReport('suite_list'), '')


class TestMainCLI(unittest.TestCase):
    """Smoke test the CLI entry point via argv parsing."""

    def setUp(self):
        self._tmpdir = tempfile.mkdtemp()
        self._datatable = _build_datatable(self._tmpdir)

    def tearDown(self):
        shutil.rmtree(self._tmpdir)

    def test_main_suite_list(self):
        import io
        import contextlib
        buf = io.StringIO()
        with contextlib.redirect_stdout(buf):
            resolved_call = cdf.main([self._datatable, '--suite-list'])
        self.assertEqual(resolved_call, 0)
        self.assertEqual(buf.getvalue().strip(), 'test_simple')

    def test_main_mutually_exclusive(self):
        # argparse should reject two actions at once.
        with self.assertRaises(SystemExit):
            cdf.main([self._datatable, '--suite-list', '--module-list'])


def load_tests(loader, tests, ignore):
    tests.addTests(doctest.DocTestSuite(cdf))
    return tests


if __name__ == '__main__':
    unittest.main(verbosity=2)
