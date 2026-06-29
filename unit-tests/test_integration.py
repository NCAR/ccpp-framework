"""End-to-end integration tests for capgen.capgen().

These tests invoke the full pipeline — metadata loading, variable resolution,
and file generation — and verify that all expected output files are produced
with correct content.  No Fortran compiler is involved; correctness is checked
at the text/XML level.
"""

import os
import tempfile
import time
import unittest
import xml.etree.ElementTree as ET

from ccpp_capgen import capgen

_TESTS_DIR   = os.path.dirname(__file__)
_SAMPLES_DIR = os.path.join(_TESTS_DIR, 'sample_files')
_SUITE_DIR   = os.path.join(_TESTS_DIR, 'sample_suite_files')


def _sf(name):
    return os.path.join(_SAMPLES_DIR, name)


def _suite_file(name):
    return os.path.join(_SUITE_DIR, name)


# ---------------------------------------------------------------------------
# Helper: run capgen and return output directory + file map
# ---------------------------------------------------------------------------

def _run_simple(tmpdir, suite_xml='suite_test_simple.xml', kind_types=None):
    """Run capgen with the simple test suite and return the output dir."""
    capgen(
        host_name='test_host',
        host_files=[_sf('host_full.meta'), _sf('control_full.meta')],
        scheme_files=[_sf('scheme_multipart.meta')],
        suite_files=[_suite_file(suite_xml)],
        output_root=tmpdir,
        kind_types=kind_types or {},
    )
    return tmpdir


def _run_subcycle(tmpdir):
    return _run_simple(tmpdir, suite_xml='suite_test_subcycle.xml')


# ---------------------------------------------------------------------------
# Test: output files exist
# ---------------------------------------------------------------------------

class TestOutputFilesExist(unittest.TestCase):

    def setUp(self):
        self._tmpdir = tempfile.mkdtemp()
        _run_simple(self._tmpdir)

    def tearDown(self):
        import shutil
        shutil.rmtree(self._tmpdir)

    def _path(self, name):
        return os.path.join(self._tmpdir, name)

    def test_host_cap_exists(self):
        self.assertTrue(os.path.isfile(self._path('test_host_ccpp_cap.F90')))

    def test_suite_cap_exists(self):
        self.assertTrue(os.path.isfile(self._path('ccpp_test_simple_cap.F90')))

    def test_group_cap_exists(self):
        self.assertTrue(os.path.isfile(self._path('ccpp_test_simple_physics_cap.F90')))

    def test_suite_data_exists(self):
        self.assertTrue(os.path.isfile(self._path('ccpp_test_simple_data.F90')))

    def test_datatable_exists(self):
        self.assertTrue(os.path.isfile(self._path('datatable.xml')))

    def test_ccpp_kinds_always_generated(self):
        # ccpp_kinds.F90 is always written, even when no --kind-type is given.
        self.assertTrue(os.path.isfile(self._path('ccpp_kinds.F90')))

    def test_ccpp_kinds_default_kind_phys(self):
        """With no --kind-type, kind_phys defaults to REAL64 from iso_fortran_env."""
        with open(self._path('ccpp_kinds.F90')) as fh:
            text = fh.read()
        self.assertIn('use iso_fortran_env, only: REAL64', text)
        self.assertIn('kind_phys = REAL64', text)

    def test_ccpp_kinds_with_explicit_kind_types(self):
        with tempfile.TemporaryDirectory() as d:
            _run_simple(d, kind_types={
                'kind_phys': ('iso_fortran_env', 'REAL64'),
            })
            self.assertTrue(os.path.isfile(os.path.join(d, 'ccpp_kinds.F90')))

    def test_ccpp_kinds_default_injected_when_other_kinds_given(self):
        """kind_phys default is injected even when other --kind-type args are present."""
        with tempfile.TemporaryDirectory() as d:
            _run_simple(d, kind_types={
                'kind_dyn': ('iso_fortran_env', 'REAL32'),
            })
            with open(os.path.join(d, 'ccpp_kinds.F90')) as fh:
                text = fh.read()
            self.assertIn('kind_dyn',  text)
            self.assertIn('kind_phys', text)


# ---------------------------------------------------------------------------
# Test: kind_spec declared in metadata is folded into ccpp_kinds.F90
# ---------------------------------------------------------------------------

def _inject_kind_spec_lines(src_meta, dest_meta, lines):
    """Copy *src_meta* to *dest_meta*, inserting *lines* into the first
    ``[ccpp-table-properties]`` block immediately after its header."""
    with open(src_meta) as fh:
        text = fh.read()
    marker = '[ccpp-table-properties]'
    idx    = text.find(marker)
    if idx < 0:
        raise RuntimeError("no ccpp-table-properties marker in " + src_meta)
    nl     = text.find('\n', idx)
    insertion = ''.join('  ' + line + '\n' for line in lines)
    new_text  = text[:nl + 1] + insertion + text[nl + 1:]
    with open(dest_meta, 'w') as fh:
        fh.write(new_text)


class TestMetadataKindSpec(unittest.TestCase):
    """Integration tests for kind_spec declared in [ccpp-table-properties]."""

    def _scheme_with_kind_spec(self, dest_dir, lines):
        """Write a copy of scheme_multipart.meta with extra ``kind_spec`` lines."""
        dest = os.path.join(dest_dir, 'scheme_multipart.meta')
        _inject_kind_spec_lines(_sf('scheme_multipart.meta'), dest, lines)
        return dest

    def test_metadata_kind_spec_added_to_ccpp_kinds(self):
        """A ``kind_spec`` declared in scheme metadata appears in ccpp_kinds.F90."""
        with tempfile.TemporaryDirectory() as d:
            scheme_meta = self._scheme_with_kind_spec(
                d, ['kind_spec = temp_kinds:kind_temp=>temp_r8'],
            )
            capgen(
                host_name='test_host',
                host_files=[_sf('host_full.meta'), _sf('control_full.meta')],
                scheme_files=[scheme_meta],
                suite_files=[_suite_file('suite_test_simple.xml')],
                output_root=d,
                kind_types={},
            )
            with open(os.path.join(d, 'ccpp_kinds.F90')) as fh:
                text = fh.read()
            # Use lines may be column-aligned when multiple modules are
            # present; match each token rather than the full line.
            self.assertRegex(text, r'use\s+temp_kinds\b[^\n]*only:\s*temp_r8')
            self.assertRegex(text, r'kind_temp\s*=\s*temp_r8')
            # kind_phys default still injected.
            self.assertIn('kind_phys',                  text)
            self.assertIn('use iso_fortran_env',        text)

    def test_metadata_kind_spec_shorthand_added_to_ccpp_kinds(self):
        """Shorthand ``module:spec`` republishes spec under its own name."""
        with tempfile.TemporaryDirectory() as d:
            scheme_meta = self._scheme_with_kind_spec(
                d, ['kind_spec = host_kinds:kind_r4'],
            )
            capgen(
                host_name='test_host',
                host_files=[_sf('host_full.meta'), _sf('control_full.meta')],
                scheme_files=[scheme_meta],
                suite_files=[_suite_file('suite_test_simple.xml')],
                output_root=d,
                kind_types={},
            )
            with open(os.path.join(d, 'ccpp_kinds.F90')) as fh:
                text = fh.read()
            self.assertRegex(text, r'use\s+host_kinds\b[^\n]*only:\s*kind_r4')
            self.assertRegex(text, r'kind_r4\s*=\s*kind_r4')

    def test_cli_and_metadata_identical_kind_spec_no_conflict(self):
        """CLI --kind-type and metadata kind_spec for the same kind are accepted when identical."""
        with tempfile.TemporaryDirectory() as d:
            scheme_meta = self._scheme_with_kind_spec(
                d, ['kind_spec = temp_kinds:kind_temp=>temp_r8'],
            )
            capgen(
                host_name='test_host',
                host_files=[_sf('host_full.meta'), _sf('control_full.meta')],
                scheme_files=[scheme_meta],
                suite_files=[_suite_file('suite_test_simple.xml')],
                output_root=d,
                kind_types={'kind_temp': ('temp_kinds', 'temp_r8')},
            )
            with open(os.path.join(d, 'ccpp_kinds.F90')) as fh:
                text = fh.read()
            # The kind appears exactly once in ccpp_kinds.F90.
            self.assertEqual(text.count('kind_temp ='), 1)

    def test_cli_and_metadata_conflicting_kind_spec_raises(self):
        """CLI and metadata declaring the same kind with different specs is a hard error."""
        with tempfile.TemporaryDirectory() as d:
            scheme_meta = self._scheme_with_kind_spec(
                d, ['kind_spec = temp_kinds:kind_temp=>temp_r8'],
            )
            from metadata.parse_tools import CCPPError
            with self.assertRaises(CCPPError) as cm:
                capgen(
                    host_name='test_host',
                    host_files=[_sf('host_full.meta'), _sf('control_full.meta')],
                    scheme_files=[scheme_meta],
                    suite_files=[_suite_file('suite_test_simple.xml')],
                    output_root=d,
                    kind_types={'kind_temp': ('other_kinds', 'r8')},
                )
            self.assertIn('kind_temp', str(cm.exception))


# ---------------------------------------------------------------------------
# Test: host_constituents module + framework file inclusion
# ---------------------------------------------------------------------------

def _run_with_constituents(tmpdir, suite_xml='suite_consume_constituent.xml'):
    """Run capgen with a constituent-using fixture and return tmpdir."""
    capgen(
        host_name='test_host',
        host_files=[_sf('host_with_constituents.meta'),
                    _sf('control_full.meta')],
        scheme_files=[_sf('scheme_consume_constituent.meta')],
        suite_files=[_suite_file(suite_xml)],
        output_root=tmpdir,
        kind_types={},
    )
    return tmpdir


class TestHostConstituentsEmittedEndToEnd(unittest.TestCase):
    """Full pipeline emits ccpp_host_constituents.F90 and lists every
    framework F90 dependency in datatable.xml's <utilities>."""

    def setUp(self):
        self._tmpdir = tempfile.mkdtemp()
        _run_with_constituents(self._tmpdir)

    def tearDown(self):
        import shutil
        shutil.rmtree(self._tmpdir)

    def test_host_constituents_file_exists(self):
        self.assertTrue(os.path.isfile(
            os.path.join(self._tmpdir, 'ccpp_host_constituents.F90')
        ))

    def _utility_paths(self):
        tree = ET.parse(os.path.join(self._tmpdir, 'datatable.xml'))
        return [e.text for e in tree.getroot().findall(
            './capgen_files/utilities/file'
        )]

    def test_datatable_lists_host_constituents(self):
        utils = self._utility_paths()
        names = [os.path.basename(p) for p in utils]
        self.assertIn('ccpp_host_constituents.F90', names)

    def test_datatable_lists_framework_constituent_module(self):
        utils = self._utility_paths()
        names = [os.path.basename(p) for p in utils]
        self.assertIn('ccpp_constituent_prop_mod.F90', names)

    def test_datatable_lists_framework_dependencies(self):
        utils = self._utility_paths()
        names = [os.path.basename(p) for p in utils]
        # Transitive deps of ccpp_constituent_prop_mod.
        self.assertIn('ccpp_hashable.F90',   names)
        self.assertIn('ccpp_hash_table.F90', names)
        # Used by cam-sima schemes (ccpp_constituent_index).
        self.assertIn('ccpp_scheme_utils.F90', names)

    def test_framework_paths_absolute_and_existing(self):
        utils = self._utility_paths()
        framework_names = {
            'ccpp_constituent_prop_mod.F90',
            'ccpp_hashable.F90',
            'ccpp_hash_table.F90',
            'ccpp_scheme_utils.F90',
        }
        for path in utils:
            if os.path.basename(path) in framework_names:
                self.assertTrue(os.path.isabs(path),
                                'framework path not absolute: ' + path)
                self.assertTrue(os.path.isfile(path),
                                'framework file missing: ' + path)

    def test_framework_paths_resolve_under_capgen_src(self):
        """Every framework F90 listed must resolve under capgen/src/.
        Capgen ships self-contained — no parent-dir fallback.  If any
        framework file lands outside capgen/src/ this test fails so
        downstream consumers (vendoring just capgen/) don't silently
        miss a required dependency."""
        from ccpp_capgen import _FRAMEWORK_SRC_DIR
        framework_names = {
            'ccpp_constituent_prop_mod.F90',
            'ccpp_hashable.F90',
            'ccpp_hash_table.F90',
            'ccpp_scheme_utils.F90',
        }
        utils = self._utility_paths()
        canonical = os.path.abspath(_FRAMEWORK_SRC_DIR)
        for path in utils:
            if os.path.basename(path) in framework_names:
                self.assertEqual(
                    os.path.abspath(os.path.dirname(path)), canonical,
                    'framework F90 outside capgen/src/: ' + path,
                )


class TestResolveFrameworkF90FilesMissingRaises(unittest.TestCase):
    """``_resolve_framework_f90_files`` raises CCPPError listing the
    missing file(s) when a required framework F90 is not present under
    capgen/src/.  Catches deployment errors immediately instead of
    leaving the host build to fail with an opaque "Cannot open module
    file" message at compile time."""

    def test_missing_file_raises_with_actionable_message(self):
        import ccpp_capgen
        from metadata.parse_tools import CCPPError
        # Append a never-vendored sentinel to the framework-F90 list,
        # then restore on teardown via a try/finally so we don't leak
        # state into other tests.
        original = list(ccpp_capgen._FRAMEWORK_F90_FILES)
        ccpp_capgen._FRAMEWORK_F90_FILES.append('definitely_missing.F90')
        try:
            with self.assertRaises(CCPPError) as cm:
                ccpp_capgen._resolve_framework_f90_files()
        finally:
            ccpp_capgen._FRAMEWORK_F90_FILES[:] = original
        msg = str(cm.exception)
        # Names the missing file, the search dir, and what to do.
        self.assertIn('definitely_missing.F90',     msg)
        self.assertIn(ccpp_capgen._FRAMEWORK_SRC_DIR, msg)
        self.assertIn('Vendor',                     msg)


class TestNoHostConstituentsWhenAbsent(unittest.TestCase):
    """The host-constituents module is NOT emitted (and the framework F90
    files are NOT listed) when no suite touches constituent state."""

    def setUp(self):
        self._tmpdir = tempfile.mkdtemp()
        _run_simple(self._tmpdir)

    def tearDown(self):
        import shutil
        shutil.rmtree(self._tmpdir)

    def test_host_constituents_file_absent(self):
        self.assertFalse(os.path.isfile(
            os.path.join(self._tmpdir, 'ccpp_host_constituents.F90')
        ))

    def test_no_framework_dependencies_in_utilities(self):
        tree = ET.parse(os.path.join(self._tmpdir, 'datatable.xml'))
        utils = [e.text for e in tree.getroot().findall(
            './capgen_files/utilities/file'
        )]
        names = [os.path.basename(p) for p in utils]
        # Only ccpp_kinds.F90 — no constituent framework files.
        self.assertEqual(names, ['ccpp_kinds.F90'])


# ---------------------------------------------------------------------------
# Test: static API content
# ---------------------------------------------------------------------------

class TestHostCapContent(unittest.TestCase):

    def setUp(self):
        self._tmpdir = tempfile.mkdtemp()
        _run_simple(self._tmpdir)
        with open(os.path.join(self._tmpdir, 'test_host_ccpp_cap.F90')) as fh:
            self.text = fh.read()

    def tearDown(self):
        import shutil
        shutil.rmtree(self._tmpdir)

    def test_module_declaration(self):
        self.assertIn('module test_host_ccpp_cap', self.text)

    def test_ccpp_register_always_present(self):
        # ccpp_register is mandatory in the new design and always emitted,
        # even when no scheme has a register phase (state transition only).
        self.assertIn('subroutine ccpp_register', self.text)

    def test_ccpp_init_present(self):
        self.assertIn('subroutine ccpp_init', self.text)

    def test_ccpp_final_present(self):
        self.assertIn('subroutine ccpp_final', self.text)

    def test_ccpp_physics_run_present(self):
        self.assertIn('subroutine ccpp_physics_run', self.text)

    def test_dispatches_to_test_simple(self):
        self.assertIn("case('test_simple')", self.text)

    def test_uses_suite_cap_module(self):
        self.assertIn('use ccpp_test_simple_cap', self.text)


# ---------------------------------------------------------------------------
# Test: suite cap content
# ---------------------------------------------------------------------------

class TestSuiteCapContent(unittest.TestCase):

    def setUp(self):
        self._tmpdir = tempfile.mkdtemp()
        _run_simple(self._tmpdir)
        with open(os.path.join(self._tmpdir, 'ccpp_test_simple_cap.F90')) as fh:
            self.text = fh.read()

    def tearDown(self):
        import shutil
        shutil.rmtree(self._tmpdir)

    def test_module_declaration(self):
        self.assertIn('module ccpp_test_simple_cap', self.text)

    def test_register_subroutine_always_present(self):
        # <suite>_register is mandatory in the new design — emitted even when
        # no scheme has a register phase (state-transition skeleton only).
        self.assertIn('subroutine test_simple_register', self.text)

    def test_init_subroutine(self):
        self.assertIn('subroutine test_simple_init', self.text)

    def test_final_subroutine(self):
        self.assertIn('subroutine test_simple_final', self.text)

    def test_physics_run_subroutine(self):
        self.assertIn('subroutine test_simple_physics_run', self.text)

    def test_dispatches_to_group(self):
        self.assertIn("case('physics')", self.text)

    def test_state_alloc_called(self):
        self.assertIn('state_alloc', self.text)

    def test_state_dealloc_called(self):
        self.assertIn('state_dealloc', self.text)


# ---------------------------------------------------------------------------
# Test: group cap content
# ---------------------------------------------------------------------------

class TestGroupCapContent(unittest.TestCase):

    def setUp(self):
        self._tmpdir = tempfile.mkdtemp()
        _run_simple(self._tmpdir)
        with open(os.path.join(self._tmpdir, 'ccpp_test_simple_physics_cap.F90')) as fh:
            self.text = fh.read()

    def tearDown(self):
        import shutil
        shutil.rmtree(self._tmpdir)

    def test_module_declaration(self):
        self.assertIn('module ccpp_test_simple_physics_cap', self.text)

    def test_state_machine_params(self):
        self.assertIn('CCPP_GROUP_UNINITIALIZED', self.text)
        self.assertIn('CCPP_GROUP_INITIALIZED', self.text)

    def test_scheme_call_present(self):
        self.assertIn('call temp_calc_adjust_run', self.text)

    def test_init_guard(self):
        self.assertIn('CCPP_GROUP_INITIALIZED', self.text)
        self.assertIn('ccpp_group_state', self.text)

    def test_state_alloc_subroutine(self):
        self.assertIn('subroutine physics_state_alloc', self.text)

    def test_state_dealloc_subroutine(self):
        self.assertIn('subroutine physics_state_dealloc', self.text)

    def test_ends_with_newline(self):
        with open(os.path.join(self._tmpdir, 'ccpp_test_simple_physics_cap.F90')) as fh:
            raw = fh.read()
        self.assertTrue(raw.endswith('\n'))


# ---------------------------------------------------------------------------
# Test: datatable.xml content
# ---------------------------------------------------------------------------

class TestDatatableContent(unittest.TestCase):

    def setUp(self):
        self._tmpdir = tempfile.mkdtemp()
        _run_simple(self._tmpdir)
        self._root = ET.parse(os.path.join(self._tmpdir, 'datatable.xml')).getroot()

    def tearDown(self):
        import shutil
        shutil.rmtree(self._tmpdir)

    def test_root_version(self):
        self.assertEqual(self._root.get('version'), '1.0')

    def test_suite_file_in_capgen_files(self):
        suite_files = self._root.find('capgen_files').find('suite_files')
        names = [os.path.basename(f.text) for f in suite_files.findall('file')]
        self.assertIn('ccpp_test_simple_cap.F90', names)
        self.assertIn('ccpp_test_simple_physics_cap.F90', names)

    def test_host_cap_in_host_files(self):
        host_files = self._root.find('capgen_files').find('host_files')
        self.assertIsNotNone(host_files)
        names = [os.path.basename(f.text) for f in host_files.findall('file')]
        self.assertIn('test_host_ccpp_cap.F90', names)

    def test_ccpp_kinds_in_utilities(self):
        # ccpp_kinds.F90 is always generated and must be discoverable by
        # CMake via <utilities> (matches the original ccpp_capgen behavior).
        utils = self._root.find('capgen_files').find('utilities')
        names = [os.path.basename(f.text) for f in utils.findall('file')]
        self.assertIn('ccpp_kinds.F90', names)

    def test_ccpp_kinds_utilities_path_is_absolute_and_exists(self):
        """The path stored in <utilities> must be absolute and resolve to a real file."""
        utils = self._root.find('capgen_files').find('utilities')
        kinds_paths = [
            f.text for f in utils.findall('file')
            if os.path.basename(f.text) == 'ccpp_kinds.F90'
        ]
        self.assertEqual(len(kinds_paths), 1)
        self.assertTrue(os.path.isabs(kinds_paths[0]))
        self.assertTrue(os.path.isfile(kinds_paths[0]))

    def test_suite_meta_in_inspection_files(self):
        inspection = self._root.find('inspection_files')
        self.assertIsNotNone(inspection)
        meta_files = inspection.find('suite_meta_files')
        self.assertIsNotNone(meta_files)
        names = [os.path.basename(f.text) for f in meta_files.findall('file')]
        self.assertIn('ccpp_test_simple_data.meta', names)

    def test_suite_meta_not_in_capgen_files(self):
        capgen_files = self._root.find('capgen_files')
        self.assertIsNone(capgen_files.find('suite_meta_files'))

    def test_expanded_sdf_in_inspection_files(self):
        inspection = self._root.find('inspection_files')
        self.assertIsNotNone(inspection)
        exp_files = inspection.find('expanded_sdf_files')
        self.assertIsNotNone(exp_files)
        names = [os.path.basename(f.text) for f in exp_files.findall('file')]
        self.assertIn('ccpp_test_simple_expanded.xml', names)
        # The path stored must resolve to a real file on disk.
        for f in exp_files.findall('file'):
            self.assertTrue(os.path.isabs(f.text))
            self.assertTrue(os.path.isfile(f.text))

    def test_scheme_in_schemes(self):
        names = [s.get('name') for s in self._root.find('schemes').findall('scheme')]
        self.assertIn('temp_calc_adjust', names)

    def test_suite_in_api(self):
        suites = self._root.find('api').find('suites')
        names = [s.get('name') for s in suites.findall('suite')]
        self.assertIn('test_simple', names)

    def test_group_in_api_suite(self):
        suites = self._root.find('api').find('suites')
        suite = next(s for s in suites.findall('suite') if s.get('name') == 'test_simple')
        group_names = [g.get('name') for g in suite.findall('group')]
        self.assertIn('physics', group_names)


# ---------------------------------------------------------------------------
# Test: subcycle suite
# ---------------------------------------------------------------------------

class TestSubcycleIntegration(unittest.TestCase):

    def setUp(self):
        self._tmpdir = tempfile.mkdtemp()
        _run_subcycle(self._tmpdir)

    def tearDown(self):
        import shutil
        shutil.rmtree(self._tmpdir)

    def test_group_cap_exists(self):
        self.assertTrue(
            os.path.isfile(
                os.path.join(self._tmpdir, 'ccpp_test_subcycle_physics_cap.F90')
            )
        )

    def test_do_loop_in_group_cap(self):
        with open(
            os.path.join(self._tmpdir, 'ccpp_test_subcycle_physics_cap.F90')
        ) as fh:
            text = fh.read()
        # ``suite_test_subcycle.xml`` declares ``<subcycle loop="3">`` —
        # the integer literal must flow through verbatim into the
        # generated do-loop bound (no host-dict lookup, no symbolic
        # translation).  Compare against the literal value, not just the
        # presence of any ``do ccpp_loop_counter = 1,``.
        self.assertIn('do ccpp_loop_counter = 1, 3', text)
        self.assertIn('end do', text)

    def test_datatable_for_subcycle_suite(self):
        root = ET.parse(os.path.join(self._tmpdir, 'datatable.xml')).getroot()
        suites = root.find('api').find('suites')
        names = [s.get('name') for s in suites.findall('suite')]
        self.assertIn('test_subcycle', names)


# ---------------------------------------------------------------------------
# Test: multiple suites in one capgen run
# ---------------------------------------------------------------------------

class TestMultipleSuites(unittest.TestCase):

    def setUp(self):
        self._tmpdir = tempfile.mkdtemp()
        capgen(
            host_name='test_host',
            host_files=[_sf('host_full.meta'), _sf('control_full.meta')],
            scheme_files=[_sf('scheme_multipart.meta')],
            suite_files=[
                _suite_file('suite_test_simple.xml'),
                _suite_file('suite_test_subcycle.xml'),
            ],
            output_root=self._tmpdir,
            kind_types={},
        )

    def tearDown(self):
        import shutil
        shutil.rmtree(self._tmpdir)

    def test_both_suite_caps_exist(self):
        self.assertTrue(
            os.path.isfile(os.path.join(self._tmpdir, 'ccpp_test_simple_cap.F90'))
        )
        self.assertTrue(
            os.path.isfile(os.path.join(self._tmpdir, 'ccpp_test_subcycle_cap.F90'))
        )

    def test_host_cap_dispatches_both(self):
        with open(os.path.join(self._tmpdir, 'test_host_ccpp_cap.F90')) as fh:
            text = fh.read()
        self.assertIn("case('test_simple')", text)
        self.assertIn("case('test_subcycle')", text)

    def test_datatable_has_both_suites(self):
        root = ET.parse(os.path.join(self._tmpdir, 'datatable.xml')).getroot()
        suites = root.find('api').find('suites')
        names = [s.get('name') for s in suites.findall('suite')]
        self.assertIn('test_simple', names)
        self.assertIn('test_subcycle', names)

    def test_scheme_deduplicated_in_datatable(self):
        root = ET.parse(os.path.join(self._tmpdir, 'datatable.xml')).getroot()
        scheme_names = [
            s.get('name') for s in root.find('schemes').findall('scheme')
        ]
        self.assertEqual(scheme_names.count('temp_calc_adjust'), 1)


# ---------------------------------------------------------------------------
# Test: multi-instance — number_of_instances flows end-to-end
# ---------------------------------------------------------------------------

class TestMultiInstanceIntegration(unittest.TestCase):
    """host_full.meta provides ninstances → generated code is multi-instance aware."""

    def setUp(self):
        self._tmpdir = tempfile.mkdtemp()
        _run_simple(self._tmpdir)

    def tearDown(self):
        import shutil
        shutil.rmtree(self._tmpdir)

    def test_ccpp_init_minimal_signature(self):
        # Lifecycle signature for a multi-instance host carries the
        # paired (inst_num, ninstances) control vars.
        with open(os.path.join(self._tmpdir, 'test_host_ccpp_cap.F90')) as fh:
            text = fh.read()
        self.assertIn(
            'subroutine ccpp_init(suite_name, errflg, errmsg, inst_num, ninstances)',
            text,
        )

    def test_suite_init_minimal_signature(self):
        with open(os.path.join(self._tmpdir, 'ccpp_test_simple_cap.F90')) as fh:
            text = fh.read()
        self.assertIn(
            'subroutine test_simple_init(inst_num, ninstances, errmsg, errflg)',
            text,
        )

    def test_register_passes_ninstances_to_suite_state_alloc(self):
        # <suite>_register USEs ninstances from the host module and passes it
        # to the suite_state allocator (idempotent first-call alloc).
        with open(os.path.join(self._tmpdir, 'ccpp_test_simple_cap.F90')) as fh:
            text = fh.read()
        self.assertIn(
            'call test_simple_suite_state_alloc(ninstances, errmsg, errflg)',
            text,
        )

    def test_suite_init_passes_ninstances_to_group_state_alloc(self):
        with open(os.path.join(self._tmpdir, 'ccpp_test_simple_cap.F90')) as fh:
            text = fh.read()
        self.assertIn(
            'call physics_state_alloc(ninstances, errmsg, errflg)',
            text,
        )

    def test_state_alloc_takes_number_of_instances_arg(self):
        with open(os.path.join(self._tmpdir, 'ccpp_test_simple_physics_cap.F90')) as fh:
            text = fh.read()
        self.assertIn(
            'subroutine physics_state_alloc(number_of_instances, errmsg, errflg)',
            text,
        )

    def test_state_alloc_uses_number_of_instances(self):
        with open(os.path.join(self._tmpdir, 'ccpp_test_simple_physics_cap.F90')) as fh:
            text = fh.read()
        self.assertIn('allocate(ccpp_group_state(number_of_instances))', text)

    def test_group_state_alloc_is_idempotent(self):
        """The generated group_state_alloc must short-circuit when the
        array is already allocated — otherwise the second instance's
        <suite>_init call crashes on a double-allocate.  Mirrors the
        ``<suite>_suite_state_alloc`` idempotency contract.
        """
        with open(os.path.join(self._tmpdir, 'ccpp_test_simple_physics_cap.F90')) as fh:
            text = fh.read()
        alloc_body = text.split('state_alloc(number_of_instances')[1]
        alloc_body = alloc_body.split('end subroutine')[0]
        self.assertIn('if (allocated(ccpp_group_state)) return', alloc_body)
        # And the guard must precede the allocate, not follow it.
        guard_pos = alloc_body.find('if (allocated(ccpp_group_state))')
        alloc_pos = alloc_body.find('allocate(ccpp_group_state(')
        self.assertGreater(alloc_pos, guard_pos,
            "Idempotency guard must precede the allocate statement")

    def test_state_guard_uses_inst_num(self):
        with open(os.path.join(self._tmpdir, 'ccpp_test_simple_physics_cap.F90')) as fh:
            text = fh.read()
        self.assertIn('ccpp_group_state(inst_num)', text)

    def test_group_init_has_inst_num_arg(self):
        with open(os.path.join(self._tmpdir, 'ccpp_test_simple_physics_cap.F90')) as fh:
            text = fh.read()
        init_sub = text.split('subroutine physics_init')[1]
        init_sub = init_sub.split('end subroutine')[0]
        self.assertIn('inst_num', init_sub)

    def test_suite_cap_dispatches_inst_num_to_group_init(self):
        with open(os.path.join(self._tmpdir, 'ccpp_test_simple_cap.F90')) as fh:
            text = fh.read()
        physics_init = text.split('subroutine test_simple_physics_init')[1]
        physics_init = physics_init.split('end subroutine')[0]
        self.assertIn('inst_num', physics_init)


class TestSingleInstanceIntegration(unittest.TestCase):
    """host_no_instance.meta + control_no_instance.meta omit the instance
    pair → generated static API drops instance_number from public
    signatures, suite cap uses literal '1' for state-array indexing,
    state arrays are allocated with the literal '1' as bound.
    """

    def setUp(self):
        self._tmpdir = tempfile.mkdtemp()
        capgen(
            host_name='test_host',
            host_files=[
                _sf('host_no_instance.meta'),
                _sf('control_no_instance.meta'),
            ],
            scheme_files=[_sf('scheme_multipart.meta')],
            suite_files=[_suite_file('suite_test_simple.xml')],
            output_root=self._tmpdir,
            kind_types={},
        )

    def tearDown(self):
        import shutil
        shutil.rmtree(self._tmpdir)

    def test_host_cap_ccpp_init_omits_inst_num(self):
        with open(os.path.join(self._tmpdir, 'test_host_ccpp_cap.F90')) as fh:
            text = fh.read()
        # ``suite_name_var`` is the local name declared by
        # ``control_no_instance.meta`` for the suite_name control var.
        self.assertIn(
            'subroutine ccpp_init(suite_name_var, errflg, errmsg)',
            text,
        )
        # And NOT the multi-instance shape.
        self.assertNotIn('inst_num', text)

    def test_host_cap_ccpp_register_omits_inst_num(self):
        with open(os.path.join(self._tmpdir, 'test_host_ccpp_cap.F90')) as fh:
            text = fh.read()
        self.assertIn(
            'subroutine ccpp_register(suite_name_var, errflg, errmsg)',
            text,
        )

    def test_host_cap_ccpp_final_omits_inst_num(self):
        with open(os.path.join(self._tmpdir, 'test_host_ccpp_cap.F90')) as fh:
            text = fh.read()
        self.assertIn(
            'subroutine ccpp_final(suite_name_var, errflg, errmsg)',
            text,
        )

    def test_suite_init_omits_inst_num(self):
        with open(os.path.join(self._tmpdir, 'ccpp_test_simple_cap.F90')) as fh:
            text = fh.read()
        self.assertIn('subroutine test_simple_init(errmsg, errflg)', text)

    def test_register_passes_literal_one_to_state_alloc(self):
        with open(os.path.join(self._tmpdir, 'ccpp_test_simple_cap.F90')) as fh:
            text = fh.read()
        self.assertIn(
            'call test_simple_suite_state_alloc(1, errmsg, errflg)', text,
        )

    def test_suite_state_indexing_uses_literal_one(self):
        with open(os.path.join(self._tmpdir, 'ccpp_test_simple_cap.F90')) as fh:
            text = fh.read()
        self.assertIn('ccpp_suite_state(1)', text)

    def test_group_state_alloc_passes_literal_one(self):
        with open(os.path.join(self._tmpdir, 'ccpp_test_simple_cap.F90')) as fh:
            text = fh.read()
        self.assertIn(
            'call physics_state_alloc(1, errmsg, errflg)',
            text,
        )

    def test_group_cap_init_omits_inst_num(self):
        with open(os.path.join(self._tmpdir, 'ccpp_test_simple_physics_cap.F90')) as fh:
            text = fh.read()
        init_sub = text.split('subroutine physics_init')[1]
        init_sub = init_sub.split('end subroutine')[0]
        self.assertNotIn('inst_num', init_sub)


class TestInstancePairingErrors(unittest.TestCase):
    """End-to-end: ``capgen`` rejects hosts that declare exactly one of the
    instance_number / number_of_instances pair."""

    def setUp(self):
        self._tmpdir = tempfile.mkdtemp()

    def tearDown(self):
        import shutil
        shutil.rmtree(self._tmpdir)

    def test_instance_alone_raises(self):
        from metadata.parse_tools import CCPPError
        with self.assertRaises(CCPPError) as ctx:
            capgen(
                host_name='test_host',
                host_files=[
                    _sf('host_no_instance.meta'),
                    _sf('control_inst_only.meta'),  # has instance_number only
                ],
                scheme_files=[_sf('scheme_multipart.meta')],
                suite_files=[_suite_file('suite_test_simple.xml')],
                output_root=self._tmpdir,
                kind_types={},
            )
        msg = str(ctx.exception)
        self.assertIn('instance_number', msg)
        self.assertIn('number_of_instances', msg)
        self.assertIn('paired', msg.lower())

    def test_ninstances_alone_raises(self):
        from metadata.parse_tools import CCPPError
        with self.assertRaises(CCPPError) as ctx:
            capgen(
                host_name='test_host',
                host_files=[
                    _sf('host_no_instance.meta'),
                    _sf('control_ninst_only.meta'), # has ninstances only
                ],
                scheme_files=[_sf('scheme_multipart.meta')],
                suite_files=[_suite_file('suite_test_simple.xml')],
                output_root=self._tmpdir,
                kind_types={},
            )
        msg = str(ctx.exception)
        self.assertIn('instance_number', msg)
        self.assertIn('number_of_instances', msg)
        self.assertIn('paired', msg.lower())


# ---------------------------------------------------------------------------
# Helper runners for ported test_prebuild test cases
# ---------------------------------------------------------------------------

def _run_opt_arg(tmpdir):
    capgen(
        host_name='test_host',
        host_files=[_sf('host_opt_arg.meta'), _sf('control_opt_arg.meta')],
        scheme_files=[_sf('scheme_opt_arg.meta')],
        suite_files=[_suite_file('suite_opt_arg.xml')],
        output_root=tmpdir,
        kind_types={},
    )
    return tmpdir


def _run_unit_conv(tmpdir):
    capgen(
        host_name='test_host',
        host_files=[_sf('host_unit_conv.meta'), _sf('control_unit_conv.meta')],
        scheme_files=[
            _sf('scheme_unit_conv_1.meta'),
            _sf('scheme_unit_conv_2.meta'),
        ],
        suite_files=[_suite_file('suite_unit_conv.xml')],
        output_root=tmpdir,
        kind_types={},
    )
    return tmpdir


def _run_chunked_data(tmpdir):
    capgen(
        host_name='test_host',
        host_files=[
            _sf('host_chunked_data.meta'),
            _sf('ddt_chunked_data.meta'),
            _sf('control_chunked_data.meta'),
        ],
        scheme_files=[_sf('scheme_chunked_data.meta')],
        suite_files=[_suite_file('suite_chunked_data.xml')],
        output_root=tmpdir,
        kind_types={},
    )
    return tmpdir


# ---------------------------------------------------------------------------
# Test: suite types module (optional variable pointer wrappers)
# ---------------------------------------------------------------------------

class TestSuiteTypesModule(unittest.TestCase):
    """Suite types module is generated when optional args are present."""

    def setUp(self):
        self._tmpdir = tempfile.mkdtemp()
        _run_opt_arg(self._tmpdir)

    def tearDown(self):
        import shutil
        shutil.rmtree(self._tmpdir)

    def _path(self, name):
        return os.path.join(self._tmpdir, name)

    def test_types_module_exists(self):
        self.assertTrue(os.path.isfile(self._path('ccpp_opt_arg_types.F90')))

    def test_types_module_declaration(self):
        with open(self._path('ccpp_opt_arg_types.F90')) as fh:
            text = fh.read()
        self.assertIn('module ccpp_opt_arg_types', text)
        self.assertIn('end module ccpp_opt_arg_types', text)

    def test_integer_ptr_type_declared(self):
        with open(self._path('ccpp_opt_arg_types.F90')) as fh:
            text = fh.read()
        self.assertIn('type :: integer_rank1_ptr_type', text)
        self.assertIn('integer, pointer :: ptr(:) => null()', text)

    def test_real_kind_phys_ptr_type_declared(self):
        with open(self._path('ccpp_opt_arg_types.F90')) as fh:
            text = fh.read()
        self.assertIn('type :: real_kind_phys_rank1_ptr_type', text)
        self.assertIn('real(kind=kind_phys), pointer :: ptr(:) => null()', text)

    def test_types_module_public_declarations(self):
        with open(self._path('ccpp_opt_arg_types.F90')) as fh:
            text = fh.read()
        self.assertIn('public :: integer_rank1_ptr_type', text)
        self.assertIn('public :: real_kind_phys_rank1_ptr_type', text)

    def test_types_module_uses_ccpp_kinds(self):
        with open(self._path('ccpp_opt_arg_types.F90')) as fh:
            text = fh.read()
        # The real_kind_phys_rank1_ptr_type uses kind_phys → must USE it.
        self.assertIn('use ccpp_kinds, only: kind_phys', text)

    def test_no_types_module_for_simple_suite(self):
        with tempfile.TemporaryDirectory() as d:
            _run_simple(d)
            self.assertFalse(os.path.isfile(
                os.path.join(d, 'ccpp_test_simple_types.F90')
            ))


# ---------------------------------------------------------------------------
# Test: optional variable handling (ported from test_prebuild/test_opt_arg)
# ---------------------------------------------------------------------------

class TestOptArgIntegration(unittest.TestCase):
    """End-to-end test with Case 2 (optional, no transform) and Case 4
    (optional + unit conversion km→m)."""

    def setUp(self):
        self._tmpdir = tempfile.mkdtemp()
        _run_opt_arg(self._tmpdir)
        with open(
            os.path.join(self._tmpdir, 'ccpp_opt_arg_opt_arg_group_cap.F90')
        ) as fh:
            self.text = fh.read()

    def tearDown(self):
        import shutil
        shutil.rmtree(self._tmpdir)

    def test_group_cap_exists(self):
        self.assertTrue(
            os.path.isfile(
                os.path.join(self._tmpdir, 'ccpp_opt_arg_opt_arg_group_cap.F90')
            )
        )

    def test_uses_types_module(self):
        self.assertIn('use ccpp_opt_arg_types', self.text)

    def test_group_cap_uses_ccpp_kinds(self):
        # Group cap declares ``real(kind=kind_phys)`` transformation locals
        # → must USE kind_phys from ccpp_kinds.
        self.assertIn('use ccpp_kinds, only: kind_phys', self.text)

    def test_case2_ptr_type_declared(self):
        self.assertIn('type(integer_rank1_ptr_type) :: opt_var_p', self.text)

    def test_case4_ptr_and_temp_declared(self):
        self.assertIn('type(real_kind_phys_rank1_ptr_type) :: opt_var_2_p', self.text)
        self.assertIn('opt_var_2_l', self.text)

    def test_case4_temp_has_target_attr(self):
        # The temp is the RHS of ``ptr%ptr => temp``, so it must be a TARGET.
        # Match the declaration line: ``real(kind=kind_phys), dimension(nx), target  :: opt_var_2_l``
        self.assertRegex(
            self.text,
            r'real\(kind=kind_phys\)[^\n]*,\s*target[^\n]*::\s*opt_var_2_l\b',
        )

    def test_case2_pre_call_active_guard(self):
        self.assertIn('if (flag_for_opt_arg) then', self.text)

    def test_case2_ptr_assignment(self):
        self.assertIn('opt_var_p%ptr => opt_arg', self.text)

    def test_case2_nullify_on_else(self):
        self.assertIn('nullify(opt_var_p%ptr)', self.text)

    def test_case4_forward_unit_conversion(self):
        # km → m: multiply by 1.0E+3
        self.assertIn('opt_var_2_l = 1.0E+3_kind_phys*opt_arg_2', self.text)

    def test_case4_ptr_to_temp(self):
        self.assertIn('opt_var_2_p%ptr => opt_var_2_l', self.text)

    def test_case4_backward_unit_conversion(self):
        # m → km: multiply by 1.0E-3
        self.assertIn('opt_arg_2', self.text)
        self.assertIn('1.0E-3_kind_phys*opt_var_2_l', self.text)

    def test_optional_arg_passed_as_ptr(self):
        self.assertIn('opt_var=opt_var_p%ptr', self.text)
        self.assertIn('opt_var_2=opt_var_2_p%ptr', self.text)

    def test_active_condition_from_host(self):
        # Active condition inherited from host metadata, not scheme metadata.
        self.assertIn('flag_for_opt_arg', self.text)

    def test_types_in_datatable(self):
        root = ET.parse(os.path.join(self._tmpdir, 'datatable.xml')).getroot()
        suite_files = root.find('capgen_files').find('suite_files')
        names = [os.path.basename(f.text) for f in suite_files.findall('file')]
        self.assertIn('ccpp_opt_arg_types.F90', names)


# ---------------------------------------------------------------------------
# Test: unit conversion (ported from test_prebuild/test_unit_conv, simplified)
# ---------------------------------------------------------------------------

class TestSubcycleStdnameLoopBound(unittest.TestCase):
    """End-to-end: a subcycle with ``loop="<std_name>"`` must resolve to
    the host's local Fortran name in the generated ``do`` loop, plus
    emit the needed USE / dummy-arg threading."""

    def setUp(self):
        self._tmpdir = tempfile.mkdtemp()
        capgen(
            host_name='test_host',
            host_files=[
                _sf('host_full.meta'),
                _sf('control_full.meta'),
                _sf('host_subcycle_stdname.meta'),
            ],
            scheme_files=[_sf('scheme_multipart.meta')],
            suite_files=[_suite_file('suite_subcycle_stdname.xml')],
            output_root=self._tmpdir,
            kind_types={},
        )
        with open(os.path.join(
            self._tmpdir,
            'ccpp_subcycle_stdname_suite_physics_cap.F90',
        )) as fh:
            self.text = fh.read()

    def tearDown(self):
        import shutil
        shutil.rmtree(self._tmpdir)

    def test_do_loop_uses_local_name(self):
        """The do-loop bound is the host's local Fortran name (n_sub),
        not the CCPP standard name (num_subcycles_for_test)."""
        self.assertIn('do ccpp_loop_counter = 1, n_sub', self.text)
        # And NOT the raw std name.
        self.assertNotIn('do ccpp_loop_counter = 1, num_subcycles_for_test',
                         self.text)

    def test_use_statement_imports_loop_local(self):
        """The host module exporting n_sub is USE'd so the symbol is
        in scope inside the generated subroutine."""
        # n_sub comes from host_phys_subcycle_helper module.
        self.assertIn('use host_phys_subcycle_helper', self.text)
        self.assertIn('n_sub', self.text)


class TestHostTableDependenciesInDatatable(unittest.TestCase):
    """A host table's ``dependencies =`` declarations must surface in
    datatable.xml's <dependencies> section.  The generator originally
    only walked scheme_tables for deps, silently dropping host-table
    declarations (real CCPP-physics hosts like SCM's GFS_typedefs.meta
    declare many host-side file dependencies)."""

    def setUp(self):
        self._tmpdir = tempfile.mkdtemp()
        capgen(
            host_name='test_host',
            host_files=[
                _sf('host_with_dependencies.meta'),
                _sf('control_full.meta'),
            ],
            scheme_files=[_sf('scheme_multipart.meta')],
            suite_files=[_suite_file('suite_test_simple.xml')],
            output_root=self._tmpdir,
            kind_types={},
        )
        import xml.etree.ElementTree as ET
        tree = ET.parse(os.path.join(self._tmpdir, 'datatable.xml'))
        self._deps = [
            d.text for d in tree.getroot()
                .find('dependencies').findall('dependency')
        ]

    def tearDown(self):
        import shutil
        shutil.rmtree(self._tmpdir)

    def test_host_dependencies_listed(self):
        """Every dep declared on the host table appears in <dependencies>
        with the dependencies_path prefix resolved."""
        names = [os.path.basename(p) for p in self._deps]
        self.assertIn('some_mp_params.F90',  names)
        self.assertIn('some_rad_param.f',    names)
        self.assertIn('some_chem.F90',       names)

    def test_dependencies_path_applied_to_host_deps(self):
        """The host's dependencies_path is resolved against each entry."""
        joined = '\n'.join(self._deps)
        self.assertIn('/tmp/fake_phys/mp/some_mp_params.F90',         joined)
        self.assertIn('/tmp/fake_phys/radiation/some_rad_param.f',    joined)
        self.assertIn('/tmp/fake_phys/chemistry/some_chem.F90',       joined)


class TestUnusedSchemeDependenciesFiltered(unittest.TestCase):
    """Scheme metadata files supplied on the CLI but not referenced by
    any loaded suite must not contribute to datatable.xml's
    <dependencies>.  Host build systems often pass the full physics
    metadata catalog and rely on capgen to narrow the compile set.
    """

    _USED_DEP   = '/tmp/used_phys/used_dep.F90'
    _UNUSED_DEP = '/tmp/unused_phys/unused_dep.F90'

    def setUp(self):
        self._tmpdir = tempfile.mkdtemp()
        used = os.path.join(self._tmpdir, 'used_scheme.meta')
        unused = os.path.join(self._tmpdir, 'unused_scheme.meta')
        # The "used" scheme is the existing scheme_multipart fixture
        # with a single ``dependencies =`` line spliced into its outer
        # [ccpp-table-properties] block.
        with open(_sf('scheme_multipart.meta')) as src:
            body = src.read()
        used_body = body.replace(
            '  type = scheme\n',
            '  type = scheme\n  dependencies = {}\n'.format(self._USED_DEP),
            1,
        )
        with open(used, 'w') as fh:
            fh.write(used_body)
        # The "unused" scheme has its own dependency.  The SDF below
        # references temp_calc_adjust only, so this file's deps must be
        # filtered out of datatable.xml.
        with open(unused, 'w') as fh:
            fh.write(
                "[ccpp-table-properties]\n"
                "  name = scheme_never_used\n"
                "  type = scheme\n"
                "  dependencies = {}\n"
                "\n"
                "[ccpp-arg-table]\n"
                "  name = scheme_never_used_run\n"
                "  type = scheme\n"
                "[ errmsg ]\n"
                "  standard_name = ccpp_error_message\n"
                "  units = none\n"
                "  dimensions = ()\n"
                "  type = character\n"
                "  kind = len=512\n"
                "  intent = out\n"
                "[ errflg ]\n"
                "  standard_name = ccpp_error_code\n"
                "  units = 1\n"
                "  dimensions = ()\n"
                "  type = integer\n"
                "  intent = out\n".format(self._UNUSED_DEP)
            )
        # Drop a placeholder .F90 next to the used scheme's .meta so the
        # source-path resolver picks it up rather than warning.  The
        # filename matches the .meta basename per the convention.
        with open(
            os.path.join(self._tmpdir, 'used_scheme.F90'), 'w'
        ) as fh:
            fh.write('! placeholder for source-path resolution\n')
        capgen(
            host_name='test_host',
            host_files=[_sf('host_full.meta'), _sf('control_full.meta')],
            scheme_files=[used, unused],
            suite_files=[_suite_file('suite_test_simple.xml')],
            output_root=self._tmpdir,
            kind_types={},
        )
        tree = ET.parse(os.path.join(self._tmpdir, 'datatable.xml'))
        root = tree.getroot()
        self._deps = [
            d.text for d in root.find('dependencies').findall('dependency')
        ]
        self._scheme_files = [
            f.text for f in root.find('scheme_files').findall('file')
        ]

    def tearDown(self):
        import shutil
        shutil.rmtree(self._tmpdir)

    def test_used_scheme_dependency_present(self):
        self.assertIn(self._USED_DEP, self._deps)

    def test_unused_scheme_dependency_absent(self):
        self.assertNotIn(self._UNUSED_DEP, self._deps)

    def test_used_scheme_source_listed(self):
        """<scheme_files> contains the resolved .F90 path for the used
        scheme (same-base-name convention against the .meta file)."""
        names = [os.path.basename(p) for p in self._scheme_files]
        self.assertIn('used_scheme.F90', names)

    def test_unused_scheme_source_absent(self):
        """A scheme metadata file passed on the CLI but not called by any
        suite must not contribute its .F90 to <scheme_files>."""
        names = [os.path.basename(p) for p in self._scheme_files]
        self.assertNotIn('unused_scheme.F90', names)
        self.assertNotIn('unused_scheme.f90', names)


class TestRegenerationIsNoopWhenContentUnchanged(unittest.TestCase):
    """Running capgen twice with the same inputs must leave every
    generated file's mtime untouched on the second invocation.  This is
    what ccpp-prebuild and original ccpp-capgen did so CMake / Make /
    Ninja do NOT rebuild dependents on a no-op regeneration.  The
    generator stages each file via a sibling temp under the output root
    and only replaces the target when content actually differs.
    """

    def setUp(self):
        self._tmpdir = tempfile.mkdtemp()
        _run_simple(self._tmpdir)
        self._first_run_files = sorted(
            f for f in os.listdir(self._tmpdir) if not f.startswith('.')
        )
        # Capture mtimes after the first run, then back-date everything
        # by an hour so any rewrite is detectable as a bumped mtime.
        self._old_mtimes = {}
        old = time.time() - 3600
        for name in self._first_run_files:
            path = os.path.join(self._tmpdir, name)
            os.utime(path, (old, old))
            self._old_mtimes[name] = os.path.getmtime(path)
        # Second run with identical inputs.
        _run_simple(self._tmpdir)

    def tearDown(self):
        import shutil
        shutil.rmtree(self._tmpdir)

    def test_no_generated_file_was_rewritten(self):
        unchanged = []
        rewritten = []
        for name, old in self._old_mtimes.items():
            path = os.path.join(self._tmpdir, name)
            new = os.path.getmtime(path)
            if new == old:
                unchanged.append(name)
            else:
                rewritten.append((name, old, new))
        self.assertEqual(
            rewritten, [],
            "files rewritten on no-op regeneration: {}".format(rewritten),
        )
        # Sanity check: we did see at least one file on disk to compare.
        self.assertTrue(unchanged)

    def test_no_temp_artifacts_remain(self):
        """The staging temp files (``.capgen_tmp_*``) must be cleaned up
        after each run; none may survive into the next build step."""
        leftovers = [
            f for f in os.listdir(self._tmpdir)
            if f.startswith('.capgen_tmp_')
        ]
        self.assertEqual(leftovers, [])


class TestRegenerationRewritesWhenContentChanges(unittest.TestCase):
    """Negative of the above: when the inputs change between runs, the
    affected generated files MUST be rewritten (bumped mtime).
    """

    def setUp(self):
        self._tmpdir = tempfile.mkdtemp()
        _run_simple(self._tmpdir)
        old = time.time() - 3600
        for name in os.listdir(self._tmpdir):
            if name.startswith('.'):
                continue
            path = os.path.join(self._tmpdir, name)
            os.utime(path, (old, old))
        # Second run picks a different SDF — exercises every cap file.
        _run_simple(self._tmpdir, suite_xml='suite_test_subcycle.xml')

    def tearDown(self):
        import shutil
        shutil.rmtree(self._tmpdir)

    def test_at_least_one_cap_was_rewritten(self):
        """The subcycle suite differs from the simple suite — some cap
        files MUST have a fresh mtime."""
        now = time.time()
        any_recent = False
        for name in os.listdir(self._tmpdir):
            if name.startswith('.'):
                continue
            path = os.path.join(self._tmpdir, name)
            if os.path.getmtime(path) > now - 60:
                any_recent = True
                break
        self.assertTrue(
            any_recent,
            "no cap file was rewritten when the suite changed",
        )


class TestDdtDependenciesInSchemeMetaPreserved(unittest.TestCase):
    """A scheme metadata file may carry a ``type = ddt`` block alongside
    its ``type = scheme`` blocks (real-world pattern: a scheme that
    constructs a DDT instance declares the DDT type in the same .meta).
    The DDT block's ``dependencies = …`` must reach datatable.xml even
    though the table name ('vmr_type', not the scheme name) won't match
    the used-schemes set.  Regression for the bug that broke the
    end-to-end-tests/capgen test where ddt2.F90 went missing because
    the DDT's deps were filtered out alongside actual scheme deps."""

    _DDT_DEP = '/tmp/ddt_phys/inner_ddt.F90'

    def setUp(self):
        self._tmpdir = tempfile.mkdtemp()
        meta = os.path.join(self._tmpdir, 'mixed_scheme.meta')
        # Splice a ``type = ddt`` table in front of the existing
        # scheme_multipart body so the .meta carries both kinds.
        with open(_sf('scheme_multipart.meta')) as src:
            body = src.read()
        ddt_block = (
            "[ccpp-table-properties]\n"
            "  name = inner_ddt_type\n"
            "  type = ddt\n"
            "  dependencies = {dep}\n"
            "[ccpp-arg-table]\n"
            "  name = inner_ddt_type\n"
            "  type = ddt\n"
            "[ pad ]\n"
            "  standard_name = inner_ddt_padding\n"
            "  units = count\n"
            "  dimensions = ()\n"
            "  type = integer\n"
            "\n"
        ).format(dep=self._DDT_DEP)
        with open(meta, 'w') as fh:
            fh.write(ddt_block)
            fh.write(body)
        capgen(
            host_name='test_host',
            host_files=[_sf('host_full.meta'), _sf('control_full.meta')],
            scheme_files=[meta],
            suite_files=[_suite_file('suite_test_simple.xml')],
            output_root=self._tmpdir,
            kind_types={},
        )
        tree = ET.parse(os.path.join(self._tmpdir, 'datatable.xml'))
        self._deps = [
            d.text for d in tree.getroot()
                .find('dependencies').findall('dependency')
        ]

    def tearDown(self):
        import shutil
        shutil.rmtree(self._tmpdir)

    def test_ddt_dependency_preserved(self):
        self.assertIn(self._DDT_DEP, self._deps)


class TestSuiteInitFinalEmission(unittest.TestCase):
    """End-to-end: an SDF with ``<init>`` and ``<final>`` at the suite
    level produces calls to the named scheme's init/final phases inside
    ``<suite>_init`` and ``<suite>_final``, with USE statements for the
    scheme module and the standard error-flag check after the call."""

    def setUp(self):
        self._tmpdir = tempfile.mkdtemp()
        capgen(
            host_name='test_host',
            host_files=[_sf('host_full.meta'), _sf('control_full.meta')],
            scheme_files=[
                _sf('scheme_multipart.meta'),
                _sf('scheme_suite_init_final.meta'),
            ],
            suite_files=[_suite_file('suite_with_init_final.xml')],
            output_root=self._tmpdir,
            kind_types={},
        )
        with open(os.path.join(
            self._tmpdir, 'ccpp_with_init_final_suite_cap.F90',
        )) as fh:
            self.text = fh.read()

    def tearDown(self):
        import shutil
        shutil.rmtree(self._tmpdir)

    def test_suite_init_calls_init_scheme(self):
        sub = self.text.split('subroutine with_init_final_suite_init')[1]
        sub = sub.split('end subroutine')[0]
        self.assertIn('call suite_init_final_scheme_init', sub)

    def test_suite_final_calls_final_scheme(self):
        sub = self.text.split('subroutine with_init_final_suite_final')[1]
        sub = sub.split('end subroutine')[0]
        self.assertIn('call suite_init_final_scheme_final', sub)

    def test_init_call_uses_scheme_module(self):
        """The scheme module is USE'd inside ``<suite>_init`` so the
        init subroutine is in scope."""
        sub = self.text.split('subroutine with_init_final_suite_init')[1]
        sub = sub.split('end subroutine')[0]
        self.assertIn(
            'use suite_init_final_scheme, only:', sub,
        )
        self.assertIn('suite_init_final_scheme_init', sub)

    def test_final_call_uses_scheme_module(self):
        sub = self.text.split('subroutine with_init_final_suite_final')[1]
        sub = sub.split('end subroutine')[0]
        self.assertIn(
            'use suite_init_final_scheme, only:', sub,
        )
        self.assertIn('suite_init_final_scheme_final', sub)

    def test_init_call_precedes_state_transition(self):
        """The init scheme is called BEFORE the FRAMEWORK_INITIALIZED
        state transition — failures during the suite-init scheme stop
        the state transition from firing."""
        sub = self.text.split('subroutine with_init_final_suite_init')[1]
        sub = sub.split('end subroutine')[0]
        call_pos = sub.index('call suite_init_final_scheme_init')
        state_pos = sub.index('CCPP_SUITE_FRAMEWORK_INITIALIZED')
        # There may be multiple state references (early-return guard);
        # the final assignment is what matters.
        state_set = sub.rindex(
            'ccpp_suite_state({}) = CCPP_SUITE_FRAMEWORK_INITIALIZED'.format(
                'inst_num' if 'inst_num' in sub else '1'
            )
        )
        self.assertLess(call_pos, state_set)

    def test_final_call_precedes_unregister_transition(self):
        sub = self.text.split('subroutine with_init_final_suite_final')[1]
        sub = sub.split('end subroutine')[0]
        call_pos = sub.index('call suite_init_final_scheme_final')
        state_set = sub.index(
            'ccpp_suite_state({}) = CCPP_SUITE_UNREGISTERED'.format(
                'inst_num' if 'inst_num' in sub else '1'
            )
        )
        self.assertLess(call_pos, state_set)

    def test_suite_init_final_scheme_listed_in_datatable(self):
        """The suite-level <init>/<final> scheme is genuinely 'used',
        so it must appear in datatable.xml's <schemes> section alongside
        any group-phase schemes.  Without this, downstream consumers
        (e.g. CMake glue iterating <schemes>) miss the file."""
        tree = ET.parse(os.path.join(self._tmpdir, 'datatable.xml'))
        names = {
            s.get('name')
            for s in tree.getroot().find('schemes').findall('scheme')
        }
        self.assertIn('suite_init_final_scheme', names)
        self.assertIn('temp_calc_adjust', names)


class TestNestedSubcycleEmission(unittest.TestCase):
    """End-to-end: a nested ``<subcycle>`` in the SDF must produce
    nested ``do`` loops in the generated cap.  Without this, schemes
    inside the inner loops run fewer times than the SDF specified,
    silently producing wrong numerical answers."""

    def setUp(self):
        self._tmpdir = tempfile.mkdtemp()
        capgen(
            host_name='test_host',
            host_files=[_sf('host_full.meta'), _sf('control_full.meta')],
            scheme_files=[_sf('scheme_multipart.meta')],
            suite_files=[_suite_file('suite_nested_subcycle.xml')],
            output_root=self._tmpdir,
            kind_types={},
        )
        with open(os.path.join(
            self._tmpdir,
            'ccpp_nested_subcycle_suite_physics_cap.F90',
        )) as fh:
            self.text = fh.read()

    def tearDown(self):
        import shutil
        shutil.rmtree(self._tmpdir)

    def test_outer_loop_present(self):
        """The outer loop uses ``ccpp_loop_counter`` (preserves the
        existing single-level convention)."""
        self.assertIn('do ccpp_loop_counter = 1, 3', self.text)

    def test_inner_loop_present(self):
        """The inner loop uses ``ccpp_loop_counter_2`` so the two loop
        vars are distinct in the same scope (Fortran requires it)."""
        self.assertIn('do ccpp_loop_counter_2 = 1, 2', self.text)

    def test_two_end_do_statements(self):
        """Each nesting level closes with an ``end do``."""
        # Find the run-phase body to scope the check.
        run = self.text.split('subroutine physics_run')[1]
        run = run.split('end subroutine')[0]
        self.assertEqual(run.count('end do'), 2)

    def test_inner_loop_inside_outer(self):
        """The inner ``do`` line appears AFTER the outer ``do`` line
        and BEFORE the first ``end do``."""
        run = self.text.split('subroutine physics_run')[1]
        run = run.split('end subroutine')[0]
        outer = run.index('do ccpp_loop_counter = 1, 3')
        inner = run.index('do ccpp_loop_counter_2 = 1, 2')
        first_end = run.index('end do')
        self.assertLess(outer, inner)
        self.assertLess(inner, first_end)

    def test_scheme_call_inside_inner_loop(self):
        """The scheme call is nested inside the inner loop — not in
        between the loops."""
        run = self.text.split('subroutine physics_run')[1]
        run = run.split('end subroutine')[0]
        inner = run.index('do ccpp_loop_counter_2 = 1, 2')
        call = run.index('call temp_calc_adjust_run')
        # First end do is the inner loop's close.
        first_end = run.index('end do')
        self.assertLess(inner, call)
        self.assertLess(call, first_end)

    def test_two_counter_declarations(self):
        """Each loop variable has its own integer declaration."""
        self.assertIn('integer :: ccpp_loop_counter\n',   self.text)
        self.assertIn('integer :: ccpp_loop_counter_2\n', self.text)


class TestSubcycleStdnameLoopBoundDdt(unittest.TestCase):
    """End-to-end: a subcycle ``loop="<std_name>"`` that resolves to a
    DDT-component must emit the access path (``<inst>%<field>``) as the
    loop bound and USE the DDT instance's parent module by the *root*
    of that access path — not the bare component name.
    """

    def setUp(self):
        self._tmpdir = tempfile.mkdtemp()
        capgen(
            host_name='test_host',
            host_files=[
                _sf('host_full.meta'),
                _sf('control_full.meta'),
                _sf('host_subcycle_stdname_ddt.meta'),
                _sf('ddt_subcycle_stdname.meta'),
            ],
            scheme_files=[_sf('scheme_multipart.meta')],
            suite_files=[_suite_file('suite_subcycle_stdname_ddt.xml')],
            output_root=self._tmpdir,
            kind_types={},
        )
        with open(os.path.join(
            self._tmpdir,
            'ccpp_subcycle_stdname_ddt_suite_physics_cap.F90',
        )) as fh:
            self.text = fh.read()

    def tearDown(self):
        import shutil
        shutil.rmtree(self._tmpdir)

    def test_do_loop_uses_access_path(self):
        """The do-loop bound is the access path of the DDT component,
        not the bare component name."""
        self.assertIn(
            'do ccpp_loop_counter = 1, phys_state%n_sub', self.text,
        )
        # And NOT the bare component or the standard name.
        self.assertNotIn('do ccpp_loop_counter = 1, n_sub\n', self.text)
        self.assertNotIn(
            'do ccpp_loop_counter = 1, num_subcycles_for_test', self.text,
        )

    def test_use_imports_ddt_instance_root(self):
        """The USE statement targets the DDT *instance* (phys_state),
        not the bare component (n_sub)."""
        # Some line starting with `use test_host_with_ddt_mod, only: ...`
        # must list phys_state — n_sub is not a free module symbol.
        self.assertIn('phys_state', self.text)
        # The bare component must not be on the ``only:`` clause.
        import re
        m = re.search(
            r'use\s+test_host_with_ddt_mod\s*,\s*only:\s*([^\n]+)',
            self.text,
        )
        self.assertIsNotNone(m)
        only_clause = m.group(1)
        self.assertIn('phys_state', only_clause)
        # Word-boundary check: ``n_sub`` may legitimately appear inside
        # ``phys_state%n_sub`` further down in the file, but it must not
        # be a free symbol on this USE line.
        symbols = [s.strip() for s in only_clause.split(',')]
        self.assertNotIn('n_sub', symbols)


class TestModuleNameOverrideIntegration(unittest.TestCase):
    """End-to-end: a scheme whose ``[ccpp-table-properties]`` declares an
    explicit ``module_name`` must cause the generated cap to emit
    ``use <module_name>`` rather than the table's ``name``."""

    def setUp(self):
        self._tmpdir = tempfile.mkdtemp()
        capgen(
            host_name='test_host',
            host_files=[_sf('host_full.meta'), _sf('control_full.meta')],
            scheme_files=[_sf('scheme_module_name_override.meta')],
            suite_files=[_suite_file('suite_module_name_override.xml')],
            output_root=self._tmpdir,
            kind_types={},
        )
        with open(os.path.join(
            self._tmpdir,
            'ccpp_module_name_override_suite_physics_cap.F90',
        )) as fh:
            self.text = fh.read()

    def tearDown(self):
        import shutil
        shutil.rmtree(self._tmpdir)

    def test_group_cap_uses_module_name_not_scheme_name(self):
        """The USE statement targets the Fortran module name."""
        self.assertIn('use mod_alt_name, only:', self.text)

    def test_group_cap_does_not_use_scheme_name_directly(self):
        """The scheme-name token (``scheme_alt_name``) must not appear as a
        module on a ``use`` line — it would resolve to a non-existent
        module file at compile time."""
        # Word-boundary check: ``scheme_alt_name_run`` (the subroutine) is
        # legitimately mentioned in the ``only:`` clause.
        import re
        bad = re.search(r'use\s+scheme_alt_name\s*,', self.text)
        self.assertIsNone(bad,
            "Cap should not emit 'use scheme_alt_name, ...' when the "
            "metadata declares module_name = mod_alt_name")

    def test_only_clause_carries_phase_subroutine(self):
        """The ``only:`` clause exposes ``<scheme_name>_<phase>`` symbols
        from the renamed module — Fortran subroutine names stay tied to
        the scheme name, not the module name."""
        self.assertIn('scheme_alt_name_run', self.text)


class TestVerticalFlipIntegration(unittest.TestCase):
    """End-to-end: host declares air_temperature with default
    top_at_one = False; scheme declares top_at_one = True.  The
    generated group cap must emit a temp + flipped subscript on the
    host-side access expression and pass the temp to the scheme.
    """

    def setUp(self):
        self._tmpdir = tempfile.mkdtemp()
        capgen(
            host_name='test_host',
            host_files=[_sf('host_full.meta'), _sf('control_full.meta')],
            scheme_files=[_sf('scheme_top_at_one.meta')],
            suite_files=[_suite_file('suite_top_at_one.xml')],
            output_root=self._tmpdir,
            kind_types={},
        )
        with open(
            os.path.join(
                self._tmpdir,
                'ccpp_top_at_one_suite_physics_cap.F90',
            )
        ) as fh:
            self.text = fh.read()

    def tearDown(self):
        import shutil
        shutil.rmtree(self._tmpdir)

    def test_group_cap_exists(self):
        self.assertTrue(
            os.path.isfile(
                os.path.join(
                    self._tmpdir,
                    'ccpp_top_at_one_suite_physics_cap.F90',
                )
            )
        )

    def test_temp_local_declared(self):
        # Transform pipeline kicks in: temp local named ``temp_l`` with
        # scheme dimensions (lb:ub on horizontal, nlev on vertical — the
        # vertical bound is single-extent shorthand for 1:nlev).
        self.assertIn('real(kind=kind_phys), dimension(lb:ub, nlev)  :: temp_l',
                      self.text)

    def test_pre_call_forward_uses_flipped_subscript(self):
        # Host metadata declares air_temperature as ``gt0`` with default
        # top_at_one = False; scheme wants top_at_one = True → reverse
        # stride on the vertical axis of the host-side expression.
        self.assertIn('temp_l = gt0(lb:ub, nlev:1:-1)', self.text)

    def test_post_call_backward_writes_into_flipped_lhs(self):
        # inout, so backward also fires; writes temp back into host with
        # the flipped subscript as LHS.
        self.assertIn('gt0(lb:ub, nlev:1:-1) = temp_l', self.text)

    def test_call_site_passes_temp_not_host(self):
        # The scheme is called with the temp local, not the host expression.
        call_block = self.text.split('call top_at_one_scheme_run')[1]
        call_block = call_block.split(')')[0]
        self.assertIn('temp=temp_l', call_block)

    def test_comment_mentions_vertical_flip(self):
        self.assertIn('vertical flip', self.text)


class TestUnitConvIntegration(unittest.TestCase):
    """Two schemes in one group: scheme_1 needs m (Case 1/2),
    scheme_2 needs km (Case 3/4 — m→km forward, km→m backward)."""

    def setUp(self):
        self._tmpdir = tempfile.mkdtemp()
        _run_unit_conv(self._tmpdir)
        with open(
            os.path.join(
                self._tmpdir, 'ccpp_unit_conv_unit_conv_group_cap.F90'
            )
        ) as fh:
            self.text = fh.read()

    def tearDown(self):
        import shutil
        shutil.rmtree(self._tmpdir)

    def test_group_cap_exists(self):
        self.assertTrue(
            os.path.isfile(
                os.path.join(
                    self._tmpdir, 'ccpp_unit_conv_unit_conv_group_cap.F90'
                )
            )
        )

    def test_case1_direct_call_scheme_1(self):
        # Scheme 1 wants m, host has m — direct reference, no temp.
        self.assertIn('call unit_conv_scheme_1_run', self.text)
        # data_array should be passed directly (no _l suffix for scheme_1 data_array)
        self.assertIn('data_array=data_array(lb:ub)', self.text)

    def test_case3_temp_forward_for_scheme_2(self):
        # Scheme 2 wants km: m→km conversion.
        self.assertIn('1.0E-3_kind_phys*data_array(lb:ub)', self.text)

    def test_case3_backward_for_scheme_2(self):
        # km→m backward conversion.
        self.assertIn('1.0E+3_kind_phys*', self.text)

    def test_case2_optional_ptr_for_scheme_1(self):
        # Scheme 1 optional data_array_opt: same units, pointer-only.
        self.assertIn('data_array_opt_p', self.text)

    def test_case4_optional_plus_transform_for_scheme_2(self):
        # Scheme 2 optional data_array_opt: pointer + km transform.
        self.assertIn('data_array_opt_2_p', self.text)

    def test_types_module_for_unit_conv(self):
        self.assertTrue(
            os.path.isfile(
                os.path.join(self._tmpdir, 'ccpp_unit_conv_types.F90')
            )
        )


# ---------------------------------------------------------------------------
# Test: chunked data (ported from test_prebuild/test_chunked_data)
# ---------------------------------------------------------------------------

class TestChunkedDataIntegration(unittest.TestCase):
    """Suite with a DDT-based host variable accessed by a scheme."""

    def setUp(self):
        self._tmpdir = tempfile.mkdtemp()
        _run_chunked_data(self._tmpdir)

    def tearDown(self):
        import shutil
        shutil.rmtree(self._tmpdir)

    def test_group_cap_exists(self):
        self.assertTrue(
            os.path.isfile(
                os.path.join(
                    self._tmpdir,
                    'ccpp_chunked_data_chunked_data_group_cap.F90',
                )
            )
        )

    def test_scheme_run_calls_present(self):
        with open(
            os.path.join(
                self._tmpdir,
                'ccpp_chunked_data_chunked_data_group_cap.F90',
            )
        ) as fh:
            text = fh.read()
        self.assertIn('call chunked_data_scheme_run', text)

    def test_ddt_access_in_group_cap(self):
        with open(
            os.path.join(
                self._tmpdir,
                'ccpp_chunked_data_chunked_data_group_cap.F90',
            )
        ) as fh:
            text = fh.read()
        # DDT field access should appear in the call expression.
        self.assertIn('chunked_data_instance%array_data', text)

    def test_no_types_module_for_chunked_data(self):
        self.assertFalse(
            os.path.isfile(
                os.path.join(self._tmpdir, 'ccpp_chunked_data_types.F90')
            )
        )

    def test_scheme_module_imported(self):
        with open(
            os.path.join(
                self._tmpdir,
                'ccpp_chunked_data_chunked_data_group_cap.F90',
            )
        ) as fh:
            text = fh.read()
        # Group cap must import the actual scheme module so that
        # `call chunked_data_scheme_<phase>(...)` resolves.
        self.assertIn('use chunked_data_scheme, only:', text)
        for phase in ('init', 'timestep_init', 'run',
                      'timestep_final', 'final'):
            self.assertIn('chunked_data_scheme_{}'.format(phase), text)

    def test_phase_subroutine_order_canonical(self):
        with open(
            os.path.join(
                self._tmpdir,
                'ccpp_chunked_data_chunked_data_group_cap.F90',
            )
        ) as fh:
            text = fh.read()
        # Phase subroutines and public declarations must appear in canonical
        # order: init, timestep_init, run, timestep_final, final, then
        # state_alloc, state_dealloc.
        canonical = [
            '_init', '_timestep_init', '_run', '_timestep_final', '_final',
            '_state_alloc', '_state_dealloc',
        ]
        positions = [
            text.index('public :: chunked_data_group{}'.format(s))
            for s in canonical
        ]
        self.assertEqual(positions, sorted(positions))

    def test_datatable_has_chunked_data_suite(self):
        root = ET.parse(os.path.join(self._tmpdir, 'datatable.xml')).getroot()
        suites = root.find('api').find('suites')
        names = [s.get('name') for s in suites.findall('suite')]
        self.assertIn('chunked_data', names)


# ---------------------------------------------------------------------------
# Test: interstitial suite-owned data — allocation and multi-instance
# ---------------------------------------------------------------------------

def _run_interstitial(tmpdir):
    from ccpp_capgen import capgen
    capgen(
        host_name='test_host',
        host_files=[_sf('host_full.meta'), _sf('control_full.meta')],
        scheme_files=[
            _sf('scheme_interstitial_producer.meta'),
            _sf('scheme_interstitial_consumer.meta'),
        ],
        suite_files=[_suite_file('suite_interstitial.xml')],
        output_root=tmpdir,
        kind_types={},
    )


class TestInterstitialSuiteData(unittest.TestCase):
    """Suite-owned interstitial variable: deferred-shape DDT, allocatable
    instance array, suite_state_alloc wired into suite init."""

    def setUp(self):
        self._tmpdir = tempfile.mkdtemp()
        _run_interstitial(self._tmpdir)

    def tearDown(self):
        import shutil
        shutil.rmtree(self._tmpdir)

    def _data(self):
        return open(os.path.join(self._tmpdir, 'ccpp_interstitial_data.F90')).read()

    def _suite_cap(self):
        return open(os.path.join(self._tmpdir, 'ccpp_interstitial_cap.F90')).read()

    def _group_cap(self):
        return open(os.path.join(self._tmpdir,
                                 'ccpp_interstitial_diag_group_cap.F90')).read()

    def test_suite_data_field_uses_deferred_shape(self):
        """Allocatable field must use (:,:) not dimension(std_name,...)."""
        text = self._data()
        self.assertIn('(:,:)', text)
        self.assertNotIn('dimension(horizontal_dimension', text)

    def test_suite_data_has_allocatable_instance_array(self):
        text = self._data()
        self.assertIn('type(ccpp_interstitial_data_t), allocatable', text)
        self.assertIn('ccpp_suite_data(:)', text)

    def test_suite_data_alloc_subroutine_exists(self):
        self.assertIn('suite_data_alloc', self._data())

    def test_suite_data_init_fields_uses_host_dims(self):
        # init_fields now owns the inner allocations; it needs the host dims.
        text = self._data()
        init_fields = text.split('subroutine suite_data_init_fields')[1].split('end subroutine')[0]
        self.assertIn('use host_phys', init_fields)
        self.assertIn('ncols', init_fields)
        self.assertIn('nlev', init_fields)

    def test_suite_data_alloc_allocates_outer_array(self):
        self.assertIn('allocate(ccpp_suite_data(number_of_instances))', self._data())

    def test_suite_data_init_fields_allocates_field_per_instance(self):
        # Inner allocations moved out of suite_data_alloc into init_fields so
        # suite-owned dims (e.g. set during _register) can be picked up after
        # the register phase has run.
        text = self._data()
        init_fields = text.split('subroutine suite_data_init_fields')[1].split('end subroutine')[0]
        self.assertIn('allocate(ccpp_suite_data(i)%diag_out(ncols, nlev))', init_fields)

    def test_suite_data_dealloc_subroutine_exists(self):
        self.assertIn('suite_data_dealloc', self._data())

    def test_suite_data_final_fields_subroutine_exists(self):
        self.assertIn('suite_data_final_fields', self._data())

    def test_suite_cap_has_suite_state_alloc(self):
        self.assertIn('interstitial_suite_state_alloc', self._suite_cap())

    def test_suite_cap_register_calls_suite_state_alloc(self):
        # State + DDT-array allocation has moved from <suite>_init into
        # <suite>_register so register can be the first lifecycle entry point.
        text = self._suite_cap()
        register_body = text.split('subroutine interstitial_register')[1].split('end subroutine')[0]
        self.assertIn('interstitial_suite_state_alloc', register_body)

    def test_suite_state_alloc_calls_suite_data_alloc(self):
        text = self._suite_cap()
        alloc_body = text.split('subroutine interstitial_suite_state_alloc')[1].split('end subroutine')[0]
        self.assertIn('suite_data_alloc', alloc_body)

    def test_suite_init_calls_init_fields(self):
        # <suite>_init triggers per-instance inner allocations.
        text = self._suite_cap()
        init_body = text.split('subroutine interstitial_init')[1].split('end subroutine')[0]
        self.assertIn('suite_data_init_fields', init_body)

    def test_suite_state_alloc_allocates_state_array(self):
        self.assertIn('allocate(ccpp_suite_state(number_of_instances))', self._suite_cap())

    def test_suite_cap_final_calls_suite_state_dealloc(self):
        text = self._suite_cap()
        final_body = text.split('subroutine interstitial_final')[1].split('end subroutine')[0]
        self.assertIn('interstitial_suite_state_dealloc', final_body)

    def test_group_cap_uses_suite_data_module(self):
        self.assertIn('use ccpp_interstitial_data', self._group_cap())

    def test_group_cap_accesses_suite_data_with_instance(self):
        """Suite var access must index ccpp_suite_data by instance."""
        text = self._group_cap()
        self.assertIn('ccpp_suite_data(', text)

    def test_group_run_has_inst_num_dummy_arg(self):
        """Gap 3: instance_number must be a dummy arg when suite vars are referenced."""
        text = self._group_cap()
        run_sub = text.split('subroutine diag_group_run')[1]
        run_sub = run_sub.split('end subroutine')[0]
        self.assertIn('inst_num', run_sub)
        self.assertIn('integer, intent(in)', run_sub)

    def test_suite_cap_passes_inst_num_to_group_run(self):
        """Suite cap physics_run dispatch must pass inst_num to group run."""
        text = self._suite_cap()
        physics_run = text.split('subroutine interstitial_physics_run')[1]
        physics_run = physics_run.split('end subroutine')[0]
        self.assertIn('inst_num', physics_run)
        self.assertIn('call diag_group_run', physics_run)

    def test_host_cap_physics_run_has_inst_num(self):
        """Static API ccpp_physics_run must include inst_num when groups need it."""
        with open(os.path.join(self._tmpdir, 'test_host_ccpp_cap.F90')) as fh:
            text = fh.read()
        physics_run = text.split('subroutine ccpp_physics_run')[1]
        physics_run = physics_run.split('end subroutine')[0]
        self.assertIn('inst_num', physics_run)

    def test_suite_meta_file_exists(self):
        """Polish 2: ccpp_<suite>_data.meta must be generated."""
        self.assertTrue(
            os.path.isfile(os.path.join(self._tmpdir, 'ccpp_interstitial_data.meta'))
        )

    def test_suite_meta_contains_suite_var(self):
        """Suite meta file must list suite-owned variable."""
        with open(os.path.join(self._tmpdir, 'ccpp_interstitial_data.meta')) as fh:
            text = fh.read()
        self.assertIn('diagnostic_interstitial_field', text)
        self.assertIn('type = suite', text)
        self.assertIn('standard_name = diagnostic_interstitial_field', text)


# ---------------------------------------------------------------------------
# Test: Gap 1 — active expression variables in USE statements
# ---------------------------------------------------------------------------

class TestActiveVarInUseStatements(unittest.TestCase):
    """Gap 1: Variables referenced in active= expressions must appear in USE."""

    def setUp(self):
        self._tmpdir = tempfile.mkdtemp()
        from test_integration import _run_unit_conv
        _run_unit_conv(self._tmpdir)
        with open(
            os.path.join(self._tmpdir, 'ccpp_unit_conv_unit_conv_group_cap.F90')
        ) as fh:
            self.text = fh.read()

    def tearDown(self):
        import shutil
        shutil.rmtree(self._tmpdir)

    def test_active_flag_in_use_statement(self):
        """flag_for_opt_array is referenced in active= but was not a direct arg."""
        self.assertIn('flag_for_opt_array', self.text.split('contains')[0])

    def test_active_flag_is_use_d_not_declared(self):
        """The active flag must appear in a use statement, not a dummy declaration."""
        use_section = self.text.split('implicit none')[0]
        self.assertIn('flag_for_opt_array', use_section)


# ---------------------------------------------------------------------------
# Test: Gap 2 — temp variable declarations use local names not standard names
# ---------------------------------------------------------------------------

class TestTempDeclLocalNames(unittest.TestCase):
    """Gap 2: Temp locals for unit conversion must be declared with local names."""

    def setUp(self):
        self._tmpdir = tempfile.mkdtemp()
        from test_integration import _run_unit_conv
        _run_unit_conv(self._tmpdir)
        with open(
            os.path.join(self._tmpdir, 'ccpp_unit_conv_unit_conv_group_cap.F90')
        ) as fh:
            self.text = fh.read()

    def tearDown(self):
        import shutil
        shutil.rmtree(self._tmpdir)

    def test_temp_uses_chunk_bounds(self):
        """Temp array for a horizontal_dimension scheme arg must be sized
        with the chunk loop bounds (host's local names for
        horizontal_loop_begin / horizontal_loop_end), NOT the full-extent
        name.  Using ``dimension(ncols)`` over-sizes the temp and produces
        a Fortran shape mismatch on the unit-conversion assignment like
        ``data_array_l = factor * data_array(lb:ub)``.

        The unit_conv fixture's control table uses ``lb`` / ``ub`` as the
        local names for horizontal_loop_begin / horizontal_loop_end.
        """
        self.assertIn('dimension(lb:ub)', self.text)
        # And NOT the full-extent name.
        self.assertNotIn('dimension(ncols)', self.text)

    def test_temp_does_not_use_standard_name(self):
        """Standard name must not appear in a dimension() declaration."""
        self.assertNotIn('dimension(horizontal_dimension)', self.text)
        self.assertNotIn('dimension(horizontal_loop_extent)', self.text)
        self.assertNotIn('dimension(horizontal_loop_begin', self.text)


class TestTempDeclLocalNamesOptArg(unittest.TestCase):
    """Gap 2: Temp decl uses nx (local) not size_of_std_arg (standard name)."""

    def setUp(self):
        self._tmpdir = tempfile.mkdtemp()
        from test_integration import _run_opt_arg
        _run_opt_arg(self._tmpdir)
        with open(
            os.path.join(self._tmpdir, 'ccpp_opt_arg_opt_arg_group_cap.F90')
        ) as fh:
            self.text = fh.read()

    def tearDown(self):
        import shutil
        shutil.rmtree(self._tmpdir)

    def test_temp_uses_local_dim_name(self):
        self.assertIn('dimension(nx)', self.text)

    def test_temp_does_not_use_standard_name(self):
        self.assertNotIn('dimension(size_of_std_arg)', self.text)


# ---------------------------------------------------------------------------
# Test: Polish 1 — timestep_init/timestep_final phase state guards
# ---------------------------------------------------------------------------

class TestTimestepStateGuards(unittest.TestCase):
    """Polish 1: timestep_init must guard on IN_TIMESTEP; timestep_final resets."""

    def setUp(self):
        self._tmpdir = tempfile.mkdtemp()
        from test_integration import _run_opt_arg
        _run_opt_arg(self._tmpdir)
        with open(
            os.path.join(self._tmpdir, 'ccpp_opt_arg_opt_arg_group_cap.F90')
        ) as fh:
            self.text = fh.read()

    def tearDown(self):
        import shutil
        shutil.rmtree(self._tmpdir)

    def test_timestep_init_entry_guard(self):
        """timestep_init subroutine must have IN_TIMESTEP entry guard."""
        ts_init = self.text.split(
            'subroutine opt_arg_group_timestep_init'
        )[1].split('end subroutine')[0]
        self.assertIn('CCPP_GROUP_IN_TIMESTEP', ts_init)
        self.assertIn('return', ts_init)

    def test_timestep_init_sets_in_timestep(self):
        """timestep_init must set state to IN_TIMESTEP after scheme calls."""
        ts_init = self.text.split(
            'subroutine opt_arg_group_timestep_init'
        )[1].split('end subroutine')[0]
        self.assertIn('ccpp_group_state', ts_init)
        self.assertIn('CCPP_GROUP_IN_TIMESTEP', ts_init)

    def test_timestep_final_resets_to_initialized(self):
        """timestep_final must reset state to INITIALIZED."""
        ts_final = self.text.split(
            'subroutine opt_arg_group_timestep_final'
        )[1].split('end subroutine')[0]
        self.assertIn('CCPP_GROUP_INITIALIZED', ts_final)
        self.assertIn('ccpp_group_state', ts_final)


# ---------------------------------------------------------------------------
# Test: Polish 2 — ccpp_<suite>_data.meta output file
# ---------------------------------------------------------------------------

class TestSuiteMetaOutput(unittest.TestCase):
    """Polish 2: ccpp_<suite>_data.meta must be written for every suite."""

    def setUp(self):
        self._tmpdir = tempfile.mkdtemp()
        _run_simple(self._tmpdir)

    def tearDown(self):
        import shutil
        shutil.rmtree(self._tmpdir)

    def test_meta_file_created(self):
        self.assertTrue(
            os.path.isfile(os.path.join(self._tmpdir, 'ccpp_test_simple_data.meta'))
        )

    def test_meta_header_comment(self):
        with open(os.path.join(self._tmpdir, 'ccpp_test_simple_data.meta')) as fh:
            text = fh.read()
        self.assertTrue(text.startswith('!'))
        self.assertIn('ccpp_test_simple', text.split('\n')[0])

    def test_meta_table_properties(self):
        with open(os.path.join(self._tmpdir, 'ccpp_test_simple_data.meta')) as fh:
            text = fh.read()
        self.assertIn('[ccpp-table-properties]', text)
        self.assertIn('name = ccpp_test_simple_data', text)
        self.assertIn('type = suite', text)

    def test_meta_empty_suite_has_no_var_entries(self):
        """simple suite has no suite-owned vars so no [ var ] blocks."""
        with open(os.path.join(self._tmpdir, 'ccpp_test_simple_data.meta')) as fh:
            text = fh.read()
        self.assertNotIn('standard_name =', text)

    def test_meta_with_suite_vars_lists_them(self):
        """Interstitial suite's meta must list the interstitial variable."""
        with tempfile.TemporaryDirectory() as d:
            capgen(
                host_name='test_host',
                host_files=[_sf('host_full.meta'), _sf('control_full.meta')],
                scheme_files=[
                    _sf('scheme_interstitial_producer.meta'),
                    _sf('scheme_interstitial_consumer.meta'),
                ],
                suite_files=[_suite_file('suite_interstitial.xml')],
                output_root=d,
                kind_types={},
            )
            with open(os.path.join(d, 'ccpp_interstitial_data.meta')) as fh:
                text = fh.read()
        self.assertIn('standard_name = diagnostic_interstitial_field', text)
        self.assertIn('units = K', text)
        self.assertIn('type = real', text)
        self.assertIn('kind = kind_phys', text)


if __name__ == '__main__':
    unittest.main(verbosity=2)
