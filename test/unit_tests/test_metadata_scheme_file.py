#! /usr/bin/env python

"""
-----------------------------------------------------------------------
 Description:  capgen needs to compare a metadata header against the
               associated CCPP Fortran interface routine.  This set of
               tests is testing the parse_scheme_files function in
               ccpp_capgen.py which performs the operations in the first
               bullet below. Each test calls this function.

               * This script contains unit tests that do the following:
                  1) Read a metadata file (to collect the metadata headers)
                  2) Read the associated CCPP Fortran scheme file (to
                     collect Fortran interfaces)
                  3) Compare the metadata header against the Fortran

               * Tests include:
                  - Correctly identify when the metadata file matches the
                    Fortran, even if the routines are not in the same order
                  - Correctly detect a missing metadata header
                  - Correctly detect a missing Fortran interface
                  - Correctly detect a mismatch between the metadata and the
                    Fortran
                  - Correctly detect invalid Fortran subroutine statements,
                    invalid dummy argument statements, and invalid Fortran
                    between the subroutine statement and the end of the
                    variable declaration block.
                  - Correctly interpret Fortran with preprocessor logic
                    which affects the subroutine statement and/or the dummy
                    argument statements
                  - Correctly interpret Fortran with preprocessor logic
                    which affects the subroutine statement and/or the dummy
                    argument statements resulting in a mismatch between the
                    metadata header and the Fortran
                  - Correctly interpret Fortran with preprocessor logic
                    which affects the subroutine statement and/or the dummy
                    argument statements resulting in incorrect Fortran

 Assumptions:

 Command line arguments: none

 Usage: python test_metadata_scheme_file.py       # run the unit tests
-----------------------------------------------------------------------
"""
import sys
import os
import logging
import unittest

_TEST_DIR = os.path.dirname(os.path.abspath(__file__))
_SCRIPTS_DIR = os.path.abspath(os.path.join(_TEST_DIR, os.pardir,
                                            os.pardir, "scripts"))
if not os.path.exists(_SCRIPTS_DIR):
    raise ImportError("Cannot find scripts directory")

sys.path.append(_SCRIPTS_DIR)

# pylint: disable=wrong-import-position
from ccpp_capgen import parse_scheme_files
# pylint: enable=wrong-import-position

class MetadataHeaderTestCase(unittest.TestCase):
    """Unit tests for parse_scheme_files"""

    def setUp(self):
        """Setup important directories and logging"""
        self._sample_files_dir = os.path.join(_TEST_DIR, "sample_scheme_files")
        self._logger = logging.getLogger(self.__class__.__name__)

    def test_good_scheme_file(self):
        """Test that good metadata file matches the Fortran, with routines in the same order """
        #Setup
        scheme_files = [os.path.join(self._sample_files_dir, "temp_adjust.meta")]
        preproc_defs = {}
        #Exercise
        scheme_headers = parse_scheme_files(scheme_files, preproc_defs,
                                          self._logger)
        #Verify size of returned list equals number of scheme headers in the test file
        #       and that header (subroutine) names are 'temp_adjust_[init,run,finalize]'
        self.assertEqual(len(scheme_headers), 3)
        #Verify header titles
        titles = [elem.title for elem in scheme_headers]
        self.assertTrue('temp_adjust_init' in titles)
        self.assertTrue('temp_adjust_run' in titles)
        self.assertTrue('temp_adjust_finalize' in titles)

    def test_reordered_scheme_file(self):
        """Test that metadata file matches the Fortran when the routines are not in the same order """
        #Setup
        scheme_files = [os.path.join(self._sample_files_dir, "reorder.meta")]
        preproc_defs = {}
        #Exercise
        scheme_headers = parse_scheme_files(scheme_files, preproc_defs,
                                            self._logger)
        #Verify size of returned list equals number of scheme headers in the test file
        #       and that header (subroutine) names are 'reorder_[init,run,finalize]'
        self.assertEqual(len(scheme_headers), 3)
        #Verify header titles
        titles = [elem.title for elem in scheme_headers]
        self.assertTrue('reorder_init' in titles)
        self.assertTrue('reorder_run' in titles)
        self.assertTrue('reorder_finalize' in titles)

if __name__ == '__main__':
    unittest.main()
