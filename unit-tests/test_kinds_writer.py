"""Unit tests for generator.kinds_writer."""

import doctest
import os
import tempfile
import unittest

from metadata.parse_tools import CCPPError
from generator.kinds_writer import _generate_ccpp_kinds, write_ccpp_kinds

_ISO = 'iso_fortran_env'


class TestGenerateCcppKinds(unittest.TestCase):

    def test_single_kind(self):
        lines = _generate_ccpp_kinds({'kind_phys': (_ISO, 'REAL64')})
        self.assertIn('module ccpp_kinds', lines)
        self.assertIn('end module ccpp_kinds', lines)
        self.assertTrue(any('kind_phys' in l and 'REAL64' in l for l in lines))
        self.assertTrue(any('parameter' in l and 'public' in l for l in lines))

    def test_iso_use_single(self):
        lines = _generate_ccpp_kinds({'kind_phys': (_ISO, 'REAL64')})
        iso_line = next(l for l in lines if 'iso_fortran_env' in l)
        self.assertIn('REAL64', iso_line)
        self.assertNotIn('&', iso_line)

    def test_iso_use_shared_spec_deduped(self):
        """Two kinds sharing the same spec → spec listed once."""
        lines = _generate_ccpp_kinds({
            'kind_a': (_ISO, 'REAL64'),
            'kind_b': (_ISO, 'REAL64'),
        })
        iso_lines = [l for l in lines if 'iso_fortran_env' in l]
        iso_text  = ' '.join(iso_lines)
        self.assertEqual(iso_text.count('REAL64'), 1)

    def test_iso_use_multiple_distinct_specs(self):
        """Two distinct specs → both appear in the same use line."""
        lines = _generate_ccpp_kinds({
            'kind_phys': (_ISO, 'REAL64'),
            'kind_dyn':  (_ISO, 'REAL32'),
        })
        iso_line = next(l for l in lines if 'iso_fortran_env' in l)
        self.assertIn('REAL32', iso_line)
        self.assertIn('REAL64', iso_line)

    def test_host_module(self):
        """Host-supplied module emits use of that module with renamed param."""
        lines = _generate_ccpp_kinds({
            'kind_phys': ('my_host_kinds', 'kind_r8'),
        })
        text = '\n'.join(lines)
        self.assertIn('use my_host_kinds, only: kind_r8', text)
        self.assertIn('kind_phys = kind_r8', text)
        self.assertNotIn('iso_fortran_env', text)

    def test_mixed_modules(self):
        """Mixed modules → one use line per module, sorted alphabetically."""
        lines = _generate_ccpp_kinds({
            'kind_phys': ('my_host_kinds', 'kind_r8'),
            'kind_iso':  (_ISO, 'REAL64'),
        })
        use_lines = [l for l in lines if l.lstrip().startswith('use ')]
        self.assertEqual(len(use_lines), 2)
        # Sorted alphabetically: iso_fortran_env first, then my_host_kinds.
        self.assertIn('iso_fortran_env', use_lines[0])
        self.assertIn('my_host_kinds',   use_lines[1])

    def test_multiple_kinds_sorted(self):
        lines = _generate_ccpp_kinds({
            'kind_z': (_ISO, 'REAL32'),
            'kind_a': (_ISO, 'REAL64'),
        })
        param_lines = [l for l in lines if 'parameter' in l and 'public' in l]
        self.assertEqual(len(param_lines), 2)
        idx_a = next(i for i, l in enumerate(lines) if 'kind_a' in l and 'parameter' in l)
        idx_z = next(i for i, l in enumerate(lines) if 'kind_z' in l and 'parameter' in l)
        self.assertLess(idx_a, idx_z)

    def test_aligned_equals(self):
        """Declarations should be column-aligned on '='."""
        lines = _generate_ccpp_kinds({
            'kind_phys': (_ISO, 'REAL64'),
            'k':         (_ISO, 'REAL32'),
        })
        param_lines = [l for l in lines if 'parameter' in l and 'public' in l]
        eq_positions = [l.index('=') for l in param_lines]
        self.assertEqual(len(set(eq_positions)), 1, "All '=' should be at the same column")

    def test_implicit_none_private(self):
        lines = _generate_ccpp_kinds({'kind_phys': (_ISO, 'REAL64')})
        self.assertIn('  implicit none', lines)
        self.assertIn('  private', lines)

    def test_empty_raises(self):
        with self.assertRaises(CCPPError):
            _generate_ccpp_kinds({})

    def test_header_comment(self):
        lines = _generate_ccpp_kinds({'kind_phys': (_ISO, 'REAL64')})
        self.assertTrue(lines[0].startswith('!'))
        self.assertIn('ccpp_kinds', lines[0])

    def test_no_trailing_newlines_in_lines(self):
        lines = _generate_ccpp_kinds({'kind_phys': (_ISO, 'REAL64')})
        for line in lines:
            self.assertNotIn('\n', line)

    def test_integer_parameter(self):
        lines = _generate_ccpp_kinds({'kind_phys': (_ISO, 'REAL64')})
        param_line = next(l for l in lines if 'kind_phys' in l and 'parameter' in l)
        self.assertIn('integer', param_line)
        self.assertIn('parameter', param_line)
        self.assertIn('public', param_line)


class TestWriteCcppKinds(unittest.TestCase):

    def test_writes_file(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            path = write_ccpp_kinds({'kind_phys': (_ISO, 'REAL64')}, tmpdir)
            self.assertTrue(os.path.isfile(path))
            self.assertEqual(os.path.basename(path), 'ccpp_kinds.F90')

    def test_file_content(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            path = write_ccpp_kinds({'kind_phys': (_ISO, 'REAL64')}, tmpdir)
            with open(path) as fh:
                content = fh.read()
            self.assertIn('module ccpp_kinds', content)
            self.assertIn('kind_phys', content)
            self.assertIn('REAL64', content)
            self.assertTrue(content.endswith('\n'))

    def test_creates_output_dir(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            subdir = os.path.join(tmpdir, 'new_subdir')
            write_ccpp_kinds({'kind_phys': (_ISO, 'REAL64')}, subdir)
            self.assertTrue(os.path.isdir(subdir))

    def test_returns_absolute_path(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            path = write_ccpp_kinds({'kind_phys': (_ISO, 'REAL64')}, tmpdir)
            self.assertTrue(os.path.isabs(path))


def load_tests(loader, tests, ignore):
    import generator.kinds_writer as kw
    tests.addTests(doctest.DocTestSuite(kw))
    return tests


if __name__ == '__main__':
    unittest.main(verbosity=2)
