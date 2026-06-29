#!/usr/bin/env python3
"""Convenience script to run all capgen unit tests.

Usage::

    python unit-tests/run_tests.py        # run all tests
    python unit-tests/run_tests.py -v     # verbose output
    python unit-tests/run_tests.py --doctest  # include doctests

This script sets up ``sys.path`` and then delegates to ``unittest.main`` so
that the tests can be run without installing the package.
"""

import os
import sys
import unittest

# ---- path setup ------------------------------------------------------------
_TESTS_DIR  = os.path.dirname(os.path.abspath(__file__))
_REPO_ROOT  = os.path.dirname(_TESTS_DIR)
_CAPGEN_DIR = os.path.join(_REPO_ROOT, 'capgen')

for _p in (_CAPGEN_DIR, _REPO_ROOT):
    if _p not in sys.path:
        sys.path.insert(0, _p)

# ---- optional --doctest flag -----------------------------------------------
_include_doctests = '--doctest' in sys.argv
if _include_doctests:
    sys.argv.remove('--doctest')

if __name__ == '__main__':
    loader = unittest.TestLoader()
    suite  = loader.discover(start_dir=_TESTS_DIR, pattern='test_*.py')

    if _include_doctests:
        import doctest
        import metadata.metadata_table as _mt
        suite.addTests(doctest.DocTestSuite(_mt))

    runner = unittest.TextTestRunner(verbosity=2 if '-v' in sys.argv else 1)
    result = runner.run(suite)
    sys.exit(0 if result.wasSuccessful() else 1)
