"""pytest configuration for capgen unit tests.

Adds the capgen package root to sys.path so that ``import metadata`` and
``import generator`` work regardless of where pytest is invoked from.

Layout assumed::

    <repo-root>/
        capgen/        <-- the package being tested
        unit-tests/       <-- this file's parent directory
            conftest.py
            test_*.py
"""

import os
import sys

# this directory (unit-tests/) — needed so tests can ``from <sibling>
# import …``.  The directory name contains a hyphen so it can't be
# used as a Python module, but adding it to sys.path makes each
# top-level test_*.py importable as a flat module.
_TESTS_DIR = os.path.dirname(os.path.abspath(__file__))
# repository root (parent of unit-tests/)
_REPO_ROOT = os.path.dirname(_TESTS_DIR)
# capgen/ package directory (sibling of unit-tests/)
_CAPGEN_DIR = os.path.join(_REPO_ROOT, 'capgen')

for _path in (_TESTS_DIR, _CAPGEN_DIR, _REPO_ROOT):
    if _path not in sys.path:
        sys.path.insert(0, _path)
