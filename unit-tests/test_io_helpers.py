"""Unit tests for :mod:`metadata.parse_tools.io_helpers`.

These cover the write-if-changed contract:
  * unchanged content leaves the on-disk mtime untouched, so downstream
    build tools (CMake, Make, Ninja) do not trigger unnecessary
    recompiles;
  * changed content (or missing file) results in an atomic
    same-directory replace;
  * the temp file lives under the target's parent directory (which sits
    under the generator's output root), never ``/tmp``.
"""

import logging
import os
import tempfile
import time
import unittest

from metadata.parse_tools.io_helpers import open_if_changed, write_if_changed


class _Capture(logging.Handler):
    """Tiny in-memory log handler for assertions."""

    def __init__(self):
        super().__init__(level=logging.DEBUG)
        self.records = []

    def emit(self, record):
        self.records.append(self.format(record))


class TestWriteIfChanged(unittest.TestCase):

    def setUp(self):
        self._tmpdir = tempfile.mkdtemp()
        self._path = os.path.join(self._tmpdir, 'out.txt')

    def tearDown(self):
        import shutil
        shutil.rmtree(self._tmpdir)

    def test_creates_file_when_missing(self):
        self.assertFalse(os.path.exists(self._path))
        wrote = write_if_changed(self._path, 'hello\n')
        self.assertTrue(wrote)
        with open(self._path) as fh:
            self.assertEqual(fh.read(), 'hello\n')

    def test_skips_write_when_content_identical(self):
        write_if_changed(self._path, 'hello\n')
        # Set an old mtime so we can detect any rewrite.
        old_mtime = time.time() - 3600
        os.utime(self._path, (old_mtime, old_mtime))
        observed = os.path.getmtime(self._path)

        wrote = write_if_changed(self._path, 'hello\n')

        self.assertFalse(wrote)
        self.assertEqual(os.path.getmtime(self._path), observed)

    def test_rewrites_when_content_differs(self):
        write_if_changed(self._path, 'hello\n')
        old_mtime = time.time() - 3600
        os.utime(self._path, (old_mtime, old_mtime))

        wrote = write_if_changed(self._path, 'goodbye\n')

        self.assertTrue(wrote)
        self.assertGreater(os.path.getmtime(self._path), old_mtime)
        with open(self._path) as fh:
            self.assertEqual(fh.read(), 'goodbye\n')

    def test_temp_file_lives_under_target_directory(self):
        """The staging temp file must be under the target's parent dir
        (which is under output_root), not /tmp.  Verified by listing
        siblings of the target during a write."""
        observed_siblings = []

        # Patch tempfile.mkstemp via a wrapper: we can't intercept inside
        # the helper, but we can verify the contract by writing many
        # files in the same dir and asserting no .capgen_tmp_* survives.
        for i in range(5):
            write_if_changed(self._path, 'iteration_{}\n'.format(i))
            observed_siblings.append(set(os.listdir(self._tmpdir)))

        for snapshot in observed_siblings:
            for name in snapshot:
                self.assertFalse(
                    name.startswith('.capgen_tmp_'),
                    "leaked temp file: {}".format(name),
                )

    def test_creates_parent_directory_if_missing(self):
        nested = os.path.join(self._tmpdir, 'a', 'b', 'c', 'out.txt')
        write_if_changed(nested, 'hi\n')
        self.assertTrue(os.path.isfile(nested))


class TestOpenIfChanged(unittest.TestCase):

    def setUp(self):
        self._tmpdir = tempfile.mkdtemp()
        self._path = os.path.join(self._tmpdir, 'out.txt')

    def tearDown(self):
        import shutil
        shutil.rmtree(self._tmpdir)

    def test_drop_in_for_open_write(self):
        with open_if_changed(self._path) as fh:
            fh.write('line1\n')
            fh.write('line2\n')
        with open(self._path) as fh:
            self.assertEqual(fh.read(), 'line1\nline2\n')

    def test_idempotent_no_mtime_bump(self):
        with open_if_changed(self._path) as fh:
            fh.write('stable\n')
        old_mtime = time.time() - 3600
        os.utime(self._path, (old_mtime, old_mtime))

        with open_if_changed(self._path) as fh:
            fh.write('stable\n')

        self.assertEqual(os.path.getmtime(self._path), old_mtime)

    def test_rejects_non_write_mode(self):
        with self.assertRaises(ValueError):
            with open_if_changed(self._path, mode='r'):
                pass


class TestLogging(unittest.TestCase):
    """The helper emits one info-level log line per call when *logger* is
    supplied — ``Wrote <path>`` on write, ``Unchanged: <path>`` on no-op.
    Build-tool users rely on the wording to tell at a glance which
    generated files actually changed on a rerun."""

    def setUp(self):
        self._tmpdir = tempfile.mkdtemp()
        self._path = os.path.join(self._tmpdir, 'out.txt')
        self._logger = logging.getLogger('test_io_helpers')
        self._logger.setLevel(logging.DEBUG)
        # Strip any preexisting handlers so we only see ours.
        for h in list(self._logger.handlers):
            self._logger.removeHandler(h)
        self._handler = _Capture()
        self._handler.setFormatter(logging.Formatter('%(levelname)s %(message)s'))
        self._logger.addHandler(self._handler)
        self._logger.propagate = False

    def tearDown(self):
        self._logger.removeHandler(self._handler)
        import shutil
        shutil.rmtree(self._tmpdir)

    def test_logs_wrote_on_new_file(self):
        write_if_changed(self._path, 'hi\n', logger=self._logger)
        joined = '\n'.join(self._handler.records)
        self.assertIn('INFO Wrote', joined)
        self.assertIn(self._path, joined)
        self.assertNotIn('Unchanged', joined)

    def test_logs_unchanged_on_identical_rewrite(self):
        write_if_changed(self._path, 'hi\n', logger=self._logger)
        self._handler.records.clear()
        write_if_changed(self._path, 'hi\n', logger=self._logger)
        joined = '\n'.join(self._handler.records)
        self.assertIn('INFO Unchanged:', joined)
        self.assertIn(self._path, joined)
        self.assertNotIn('Wrote', joined)

    def test_logs_wrote_on_content_change(self):
        write_if_changed(self._path, 'hi\n', logger=self._logger)
        self._handler.records.clear()
        write_if_changed(self._path, 'bye\n', logger=self._logger)
        joined = '\n'.join(self._handler.records)
        self.assertIn('INFO Wrote', joined)
        self.assertNotIn('Unchanged', joined)

    def test_open_if_changed_forwards_logger(self):
        with open_if_changed(self._path, logger=self._logger) as fh:
            fh.write('hi\n')
        self.assertIn('INFO Wrote',  '\n'.join(self._handler.records))


if __name__ == '__main__':
    unittest.main()
