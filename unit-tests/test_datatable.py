"""Unit tests for generator.datatable."""

import doctest
import os
import tempfile
import unittest
import xml.etree.ElementTree as ET

from generator.suite_resolver import resolve_suite
import generator.datatable as dt_mod
from generator.datatable import write_datatable
from test_suite_resolver import (
    _load_full_host_dict,
    _load_scheme_store,
    _parse_suite,
)


def _resolve(suite_xml='suite_test_simple.xml'):
    hd    = _load_full_host_dict()
    store = _load_scheme_store()
    suite = _parse_suite(suite_xml)
    return resolve_suite(suite, store, hd), store


def _write(tmpdir, suite_xml='suite_test_simple.xml', utility_paths=None,
           suite_file_paths=None, host_file_paths=None, host_dict=None, suite_meta_paths=None,
           expanded_sdf_paths=None):
    suite_resolution, store = _resolve(suite_xml)
    return (
        write_datatable(
            [suite_resolution],
            store,
            utility_paths or ['/out/ccpp_kinds.F90', '/out/test_host_ccpp_cap.F90'],
            suite_file_paths or ['/out/ccpp_test_simple_cap.F90'],
            tmpdir,
            host_file_paths=host_file_paths,
            suite_meta_paths=suite_meta_paths,
            expanded_sdf_paths=expanded_sdf_paths,
            host_dict=host_dict,
        ),
        suite_resolution,
        store,
    )


class TestWriteDatatableFile(unittest.TestCase):

    def test_creates_file(self):
        with tempfile.TemporaryDirectory() as d:
            path, _, _ = _write(d)
            self.assertTrue(os.path.isfile(path))
            self.assertEqual(os.path.basename(path), 'datatable.xml')

    def test_creates_output_dir(self):
        with tempfile.TemporaryDirectory() as d:
            subdir = os.path.join(d, 'ccpp')
            _write(subdir)
            self.assertTrue(os.path.isdir(subdir))

    def test_file_ends_with_newline(self):
        with tempfile.TemporaryDirectory() as d:
            path, _, _ = _write(d)
            with open(path) as fh:
                content = fh.read()
            self.assertTrue(content.endswith('\n'))

    def test_xml_declaration(self):
        with tempfile.TemporaryDirectory() as d:
            path, _, _ = _write(d)
            with open(path) as fh:
                first_line = fh.readline()
            self.assertIn('<?xml', first_line)

    def test_root_element_and_version(self):
        with tempfile.TemporaryDirectory() as d:
            path, _, _ = _write(d)
            tree = ET.parse(path)
            root = tree.getroot()
            self.assertEqual(root.tag, 'ccpp_datatable')
            self.assertEqual(root.get('version'), '1.0')


class TestCcppFilesSection(unittest.TestCase):

    def setUp(self):
        self._tmpdir = tempfile.mkdtemp()
        utils = ['/out/ccpp_kinds.F90', '/out/test_host_ccpp_cap.F90']
        sfiles = ['/out/ccpp_test_simple_cap.F90', '/out/ccpp_test_simple_data.F90']
        path, _, _ = _write(self._tmpdir, utility_paths=utils, suite_file_paths=sfiles)
        self._root = ET.parse(path).getroot()

    def tearDown(self):
        import shutil
        shutil.rmtree(self._tmpdir)

    def _capgen_files(self):
        return self._root.find('capgen_files')

    def test_capgen_files_element_present(self):
        self.assertIsNotNone(self._capgen_files())

    def test_utilities_subsection(self):
        utils = self._capgen_files().find('utilities')
        self.assertIsNotNone(utils)
        files = [f.text for f in utils.findall('file')]
        self.assertIn('/out/ccpp_kinds.F90', files)
        self.assertIn('/out/test_host_ccpp_cap.F90', files)

    def test_suite_files_subsection(self):
        sfiles_elem = self._capgen_files().find('suite_files')
        self.assertIsNotNone(sfiles_elem)
        files = [f.text for f in sfiles_elem.findall('file')]
        self.assertIn('/out/ccpp_test_simple_cap.F90', files)
        self.assertIn('/out/ccpp_test_simple_data.F90', files)

    def test_host_files_section_present(self):
        capgen_files = self._capgen_files()
        host_files = capgen_files.find('host_files')
        self.assertIsNotNone(host_files)

    def test_host_files_empty_when_not_given(self):
        capgen_files = self._capgen_files()
        host_files = capgen_files.find('host_files')
        self.assertEqual(len(list(host_files)), 0)

    def test_no_suite_meta_in_capgen_files(self):
        """suite_meta_files used to live under <capgen_files>; it must not now."""
        self.assertIsNone(self._capgen_files().find('suite_meta_files'))


class TestInspectionFilesSection(unittest.TestCase):
    """<inspection_files> collects non-Fortran artifacts (meta + expanded SDF)."""

    def setUp(self):
        self._tmpdir = tempfile.mkdtemp()
        path, _, _ = _write(
            self._tmpdir,
            suite_meta_paths=['/out/ccpp_test_simple_data.meta'],
            expanded_sdf_paths=['/out/ccpp_test_simple_expanded.xml'],
        )
        self._root = ET.parse(path).getroot()

    def tearDown(self):
        import shutil
        shutil.rmtree(self._tmpdir)

    def _inspection(self):
        return self._root.find('inspection_files')

    def test_inspection_files_element_present(self):
        self.assertIsNotNone(self._inspection())

    def test_suite_meta_files_subsection(self):
        meta = self._inspection().find('suite_meta_files')
        self.assertIsNotNone(meta)
        files = [f.text for f in meta.findall('file')]
        self.assertEqual(files, ['/out/ccpp_test_simple_data.meta'])

    def test_expanded_sdf_files_subsection(self):
        exp = self._inspection().find('expanded_sdf_files')
        self.assertIsNotNone(exp)
        files = [f.text for f in exp.findall('file')]
        self.assertEqual(files, ['/out/ccpp_test_simple_expanded.xml'])

    def test_empty_subsections_when_no_paths(self):
        """Section + both subsections are always written, even when empty."""
        with tempfile.TemporaryDirectory() as d:
            path, _, _ = _write(d)  # no meta / expanded paths
            root = ET.parse(path).getroot()
            insp = root.find('inspection_files')
            self.assertIsNotNone(insp)
            self.assertIsNotNone(insp.find('suite_meta_files'))
            self.assertIsNotNone(insp.find('expanded_sdf_files'))
            self.assertEqual(len(insp.find('suite_meta_files').findall('file')), 0)
            self.assertEqual(len(insp.find('expanded_sdf_files').findall('file')), 0)


class TestSchemesSection(unittest.TestCase):

    def setUp(self):
        self._tmpdir = tempfile.mkdtemp()
        path, _, _ = _write(self._tmpdir)
        self._root = ET.parse(path).getroot()

    def tearDown(self):
        import shutil
        shutil.rmtree(self._tmpdir)

    def _schemes(self):
        return self._root.find('schemes')

    def test_schemes_element_present(self):
        self.assertIsNotNone(self._schemes())

    def test_scheme_name_present(self):
        names = [s.get('name') for s in self._schemes().findall('scheme')]
        self.assertIn('temp_calc_adjust', names)

    def test_no_duplicate_scheme_elements(self):
        names = [s.get('name') for s in self._schemes().findall('scheme')]
        self.assertEqual(len(names), len(set(names)))

    def test_run_phase_element(self):
        scheme = next(
            s for s in self._schemes().findall('scheme')
            if s.get('name') == 'temp_calc_adjust'
        )
        phase_tags = [child.tag for child in scheme]
        self.assertIn('run', phase_tags)

    def test_run_phase_attributes(self):
        scheme = next(
            s for s in self._schemes().findall('scheme')
            if s.get('name') == 'temp_calc_adjust'
        )
        run_elem = scheme.find('run')
        self.assertIsNotNone(run_elem)
        self.assertEqual(run_elem.get('name'), 'temp_calc_adjust')
        self.assertEqual(run_elem.get('subroutine_name'), 'temp_calc_adjust_run')
        self.assertEqual(run_elem.get('module'), 'temp_calc_adjust')

    def test_call_list_present(self):
        scheme = next(
            s for s in self._schemes().findall('scheme')
            if s.get('name') == 'temp_calc_adjust'
        )
        run_elem = scheme.find('run')
        call_list = run_elem.find('call_list')
        self.assertIsNotNone(call_list)

    def test_call_list_has_vars(self):
        scheme = next(
            s for s in self._schemes().findall('scheme')
            if s.get('name') == 'temp_calc_adjust'
        )
        run_elem = scheme.find('run')
        call_list = run_elem.find('call_list')
        vars_ = call_list.findall('var')
        self.assertGreater(len(vars_), 0)

    def test_var_has_required_attributes(self):
        scheme = next(
            s for s in self._schemes().findall('scheme')
            if s.get('name') == 'temp_calc_adjust'
        )
        run_elem = scheme.find('run')
        call_list = run_elem.find('call_list')
        for v in call_list.findall('var'):
            self.assertIsNotNone(v.get('name'), "var missing 'name'")
            self.assertIsNotNone(v.get('intent'), "var missing 'intent'")
            self.assertIsNotNone(v.get('local_name'), "var missing 'local_name'")

    def test_error_vars_present_in_call_list(self):
        scheme = next(
            s for s in self._schemes().findall('scheme')
            if s.get('name') == 'temp_calc_adjust'
        )
        run_elem = scheme.find('run')
        call_list = run_elem.find('call_list')
        std_names = [v.get('name') for v in call_list.findall('var')]
        self.assertIn('ccpp_error_message', std_names)
        self.assertIn('ccpp_error_code', std_names)


class TestApiSection(unittest.TestCase):

    def setUp(self):
        self._tmpdir = tempfile.mkdtemp()
        path, _, _ = _write(self._tmpdir)
        self._root = ET.parse(path).getroot()

    def tearDown(self):
        import shutil
        shutil.rmtree(self._tmpdir)

    def test_api_element_present(self):
        self.assertIsNotNone(self._root.find('api'))

    def test_suites_element_present(self):
        api = self._root.find('api')
        self.assertIsNotNone(api.find('suites'))

    def test_suite_name(self):
        suites = self._root.find('api').find('suites')
        names = [s.get('name') for s in suites.findall('suite')]
        self.assertIn('test_simple', names)

    def test_group_name(self):
        suites = self._root.find('api').find('suites')
        suite = next(s for s in suites.findall('suite') if s.get('name') == 'test_simple')
        groups = suite.findall('group')
        self.assertGreater(len(groups), 0)
        group_names = [g.get('name') for g in groups]
        self.assertIn('physics', group_names)

    def test_scheme_listed_in_group(self):
        suites = self._root.find('api').find('suites')
        suite = next(s for s in suites.findall('suite') if s.get('name') == 'test_simple')
        group = next(g for g in suite.findall('group') if g.get('name') == 'physics')
        scheme_names = [s.text for s in group.findall('scheme')]
        self.assertIn('temp_calc_adjust', scheme_names)

    def test_no_duplicate_schemes_in_group(self):
        suites = self._root.find('api').find('suites')
        suite = next(s for s in suites.findall('suite') if s.get('name') == 'test_simple')
        group = next(g for g in suite.findall('group') if g.get('name') == 'physics')
        names = [s.text for s in group.findall('scheme')]
        self.assertEqual(len(names), len(set(names)))


class TestDependenciesSection(unittest.TestCase):

    def setUp(self):
        self._tmpdir = tempfile.mkdtemp()
        path, _, _ = _write(self._tmpdir)
        self._root = ET.parse(path).getroot()

    def tearDown(self):
        import shutil
        shutil.rmtree(self._tmpdir)

    def test_dependencies_element_present(self):
        self.assertIsNotNone(self._root.find('dependencies'))

    def test_dependencies_empty_by_default(self):
        deps = self._root.find('dependencies')
        self.assertEqual(len(list(deps)), 0)


class TestDependenciesPopulated(unittest.TestCase):
    """write_datatable writes <dependency> children when paths are given."""

    def setUp(self):
        self._tmpdir = tempfile.mkdtemp()
        suite_resolution, store = _resolve()
        self._path = write_datatable(
            [suite_resolution],
            store,
            [],
            [],
            self._tmpdir,
            dependency_paths=['/path/to/a.F90', '/path/to/b.F90', '/path/to/a.F90'],
        )
        self._root = ET.parse(self._path).getroot()

    def tearDown(self):
        import shutil
        shutil.rmtree(self._tmpdir)

    def test_dependency_elements_present(self):
        deps = self._root.find('dependencies')
        texts = [d.text for d in deps.findall('dependency')]
        self.assertIn('/path/to/a.F90', texts)
        self.assertIn('/path/to/b.F90', texts)

    def test_dependencies_deduplicated(self):
        deps = self._root.find('dependencies')
        texts = [d.text for d in deps.findall('dependency')]
        self.assertEqual(texts.count('/path/to/a.F90'), 1)

    def test_dependencies_sorted(self):
        deps = self._root.find('dependencies')
        texts = [d.text for d in deps.findall('dependency')]
        self.assertEqual(texts, sorted(texts))


class TestSubcycleDatatable(unittest.TestCase):
    """Schemes inside subcycles appear once in the datatable."""

    def setUp(self):
        self._tmpdir = tempfile.mkdtemp()
        path, _, _ = _write(self._tmpdir, suite_xml='suite_test_subcycle.xml')
        self._root = ET.parse(path).getroot()

    def tearDown(self):
        import shutil
        shutil.rmtree(self._tmpdir)

    def test_scheme_deduped_in_schemes(self):
        names = [s.get('name') for s in self._root.find('schemes').findall('scheme')]
        self.assertEqual(names.count('temp_calc_adjust'), 1)

    def test_scheme_deduped_in_api_group(self):
        suites = self._root.find('api').find('suites')
        suite = suites.findall('suite')[0]
        group = suite.findall('group')[0]
        names = [s.text for s in group.findall('scheme')]
        self.assertEqual(names.count('temp_calc_adjust'), 1)


class TestDiagnosticNameEmission(unittest.TestCase):
    """diagnostic_name / diagnostic_name_fixed must reach <var> attributes."""

    def _find_var(self, root, scheme, phase, std_name):
        for s in root.find('schemes').findall('scheme'):
            if s.get('name') != scheme:
                continue
            ph = s.find(phase)
            if ph is None:
                continue
            for v in ph.find('call_list').findall('var'):
                if v.get('name') == std_name:
                    return v
        return None

    def test_explicit_diagnostic_name_emitted(self):
        with tempfile.TemporaryDirectory() as d:
            path, _, store = _write(d)
            # Find a scheme variable, set an explicit diagnostic_name on its
            # MetaVar, then re-write the datatable.
            mvars = store.variables_for('temp_calc_adjust', 'run')
            target = next(mv for mv in mvars
                          if mv.standard_name == 'air_temperature')
            target._diagnostic_name = 'temperature'
            suite_resolution, _ = _resolve()
            path = write_datatable(
                [suite_resolution], store,
                ['/out/ccpp_kinds.F90'],
                ['/out/ccpp_test_simple_cap.F90'],
                d,
            )
            root = ET.parse(path).getroot()
            v = self._find_var(root, 'temp_calc_adjust', 'run',
                               'air_temperature')
            self.assertIsNotNone(v)
            self.assertEqual(v.get('diagnostic_name'), 'temperature')

    def test_diagnostic_name_fixed_emitted(self):
        with tempfile.TemporaryDirectory() as d:
            suite_resolution, store = _resolve()
            mvars = store.variables_for('temp_calc_adjust', 'run')
            target = next(mv for mv in mvars
                          if mv.standard_name == 'air_temperature')
            target.diagnostic_name_fixed = 'Q'
            path = write_datatable(
                [suite_resolution], store,
                ['/out/ccpp_kinds.F90'],
                ['/out/ccpp_test_simple_cap.F90'],
                d,
            )
            root = ET.parse(path).getroot()
            v = self._find_var(root, 'temp_calc_adjust', 'run',
                               'air_temperature')
            self.assertEqual(v.get('diagnostic_name_fixed'), 'Q')
            # diagnostic_name attr is suppressed (since _fixed wins).
            self.assertIsNone(v.get('diagnostic_name'))

    def test_default_diagnostic_name_is_local_name(self):
        """No explicit attrs → diagnostic_name attribute equals local_name."""
        with tempfile.TemporaryDirectory() as d:
            path, _, _ = _write(d)
            root = ET.parse(path).getroot()
            v = self._find_var(root, 'temp_calc_adjust', 'run',
                               'air_temperature')
            self.assertIsNotNone(v)
            self.assertEqual(v.get('diagnostic_name'), v.get('local_name'))


class TestHostFilesPopulated(unittest.TestCase):
    """host_file_paths arg populates <host_files>."""

    def setUp(self):
        self._tmpdir = tempfile.mkdtemp()
        self._path, _, _ = _write(
            self._tmpdir,
            host_file_paths=['/out/test_host_ccpp_cap.F90'],
        )
        self._root = ET.parse(self._path).getroot()

    def tearDown(self):
        import shutil
        shutil.rmtree(self._tmpdir)

    def test_host_files_contains_host_cap(self):
        host_files = self._root.find('capgen_files').find('host_files')
        names = [f.text for f in host_files.findall('file')]
        self.assertEqual(names, ['/out/test_host_ccpp_cap.F90'])


class TestVarDictionariesSection(unittest.TestCase):
    """write_datatable emits <var_dictionaries> when host_dict is given."""

    def setUp(self):
        self._tmpdir = tempfile.mkdtemp()
        self._hd = _load_full_host_dict()
        self._path, self._sr, _ = _write(
            self._tmpdir,
            host_dict=self._hd,
        )
        self._root = ET.parse(self._path).getroot()

    def tearDown(self):
        import shutil
        shutil.rmtree(self._tmpdir)

    def _vd(self):
        return self._root.find('var_dictionaries')

    def test_section_emitted_when_host_dict_provided(self):
        self.assertIsNotNone(self._vd())

    def test_no_section_when_host_dict_absent(self):
        with tempfile.TemporaryDirectory() as d:
            path, _, _ = _write(d)  # host_dict=None
            root = ET.parse(path).getroot()
            self.assertIsNone(root.find('var_dictionaries'))

    def test_host_dictionary_present(self):
        host_d = next(
            (vd for vd in self._vd().findall('var_dictionary')
             if vd.get('type') == 'host'),
            None,
        )
        self.assertIsNotNone(host_d)
        self.assertEqual(host_d.get('name'), 'host')

    def test_host_dictionary_has_vars(self):
        host_d = next(vd for vd in self._vd().findall('var_dictionary')
                      if vd.get('type') == 'host')
        names = {v.get('name') for v in host_d.find('variables').findall('var')}
        self.assertIn('air_temperature', names)

    def test_api_dict_parent_is_host(self):
        api_d = next(vd for vd in self._vd().findall('var_dictionary')
                     if vd.get('type') == 'api')
        # The 'host' string is a fixed internal label written by the
        # generator; ccpp_datafile.py uses it only for the api->host walk.
        self.assertEqual(api_d.get('parent'), 'host')

    def test_suite_dict_parent_is_api(self):
        suite_d = next(vd for vd in self._vd().findall('var_dictionary')
                       if vd.get('type') == 'suite')
        self.assertEqual(suite_d.get('parent'), 'ccpp_api')
        self.assertEqual(suite_d.get('name'), 'test_simple')

    def test_group_dict_parent_is_suite(self):
        group_d = next(vd for vd in self._vd().findall('var_dictionary')
                       if vd.get('type') == 'group')
        self.assertEqual(group_d.get('parent'), 'test_simple')

    def test_group_call_list_parent_is_group(self):
        call_d = next(vd for vd in self._vd().findall('var_dictionary')
                      if vd.get('type') == 'group_call_list')
        gname = call_d.get('name').replace('_call_list', '')
        self.assertEqual(call_d.get('parent'), gname)

    def test_group_call_list_has_vars(self):
        call_d = next(vd for vd in self._vd().findall('var_dictionary')
                      if vd.get('type') == 'group_call_list')
        vars_ = call_d.find('variables').findall('var')
        self.assertGreater(len(vars_), 0)
        for v in vars_:
            self.assertIsNotNone(v.get('name'))
            # intent absent for the rare control-only entry is OK
            # but every var must have a name


class TestVarDictionariesProtectedAttr(unittest.TestCase):
    """Protected host vars carry protected='True'; others omit the attr."""

    def setUp(self):
        self._tmpdir = tempfile.mkdtemp()
        hd = _load_full_host_dict()
        # Flip one entry to protected for the test.
        self._protected_std = next(iter(hd))
        hd[self._protected_std].protected = True
        self._hd = hd
        path, _, _ = _write(self._tmpdir, host_dict=hd)
        self._root = ET.parse(path).getroot()

    def tearDown(self):
        import shutil
        shutil.rmtree(self._tmpdir)

    def test_protected_attr_emitted_when_true(self):
        host_d = next(vd for vd in self._root.find('var_dictionaries')
                      .findall('var_dictionary')
                      if vd.get('type') == 'host')
        v = next(v for v in host_d.find('variables').findall('var')
                 if v.get('name') == self._protected_std)
        self.assertEqual(v.get('protected'), 'True')

    def test_protected_attr_absent_when_false(self):
        host_d = next(vd for vd in self._root.find('var_dictionaries')
                      .findall('var_dictionary')
                      if vd.get('type') == 'host')
        non_prot = next(v for v in host_d.find('variables').findall('var')
                        if v.get('name') != self._protected_std)
        self.assertIsNone(non_prot.get('protected'))


def load_tests(loader, tests, ignore):
    tests.addTests(doctest.DocTestSuite(dt_mod))
    return tests


if __name__ == '__main__':
    unittest.main(verbosity=2)
