#! /usr/bin/env python3
"""
-----------------------------------------------------------------------
 Description:  Contains unit tests for parsing Fortran
               in scripts files scripts/fortran_tools/parse_fortran_file.py and
               scripts/fortran_tools/parse_fortran.py

 Assumptions:

 Command line arguments: none

 Usage: python3 test_fortran_parse.py         # run the unit tests
-----------------------------------------------------------------------
"""

import os
import sys
import logging
import unittest

_TEST_DIR = os.path.dirname(os.path.abspath(__file__))
_SCRIPTS_DIR = os.path.abspath(os.path.join(_TEST_DIR, os.pardir,
                                            os.pardir, "scripts"))
_SAMPLE_FILES_DIR = os.path.join(_TEST_DIR, "sample_files", "fortran_files")
_PRE_TMP_DIR = os.path.join(_TEST_DIR, "tmp")
_TMP_DIR = os.path.join(_PRE_TMP_DIR, "fortran_files")

if not os.path.exists(_SCRIPTS_DIR):
    raise ImportError(f"Cannot find scripts directory, {_SCRIPTS_DIR}")

sys.path.append(_SCRIPTS_DIR)

# pylint: disable=wrong-import-position
from fortran_tools import parse_fortran_file
from framework_env import CCPPFrameworkEnv
# pylint: enable=wrong-import-position

###############################################################################
def remove_files(file_list):
###############################################################################
    """Remove files in <file_list> if they exist"""
    if isinstance(file_list, str):
        file_list = [file_list]
    # end if
    for fpath in file_list:
        if os.path.exists(fpath):
            os.remove(fpath)
        # End if
    # End for

class FortranParseTestCase(unittest.TestCase):

    """Tests for `parse_fortran_file`."""

    _run_env = None

    @classmethod
    def setUpClass(cls):
        """Clean output directory (tmp) before running tests"""
        # Does "tmp" directory exist?  If not then create it:
        if not os.path.exists(_PRE_TMP_DIR):
            os.makedirs(_PRE_TMP_DIR)
        # end if

        # We need a run environment
        logger = logging.getLogger(cls.__name__)
        cls._run_env = CCPPFrameworkEnv(logger, ndict={'host_files':'',
                                                       'scheme_files':'',
                                                       'suites':''})

        # Run inherited setup method:
        super().setUpClass()

    def test_array_parsing(self):
        """Test that the Fortran parser outputs an informative
           error message for a badly formatted array specification.
           Also, test that allowed specification strings are allowed.
        """
        # Setup
        testname = "array_parsing_test"
        source = os.path.join(_SAMPLE_FILES_DIR, f"{testname}.F90")
        # Exercise
        header = "Test of parsing of Fortran array specification"

        with self.assertRaises(Exception) as context:
            # Parse the file
            _ = parse_fortran_file(source, self._run_env)
        # end if

        # Check exception for expected error messages
        self.assertTrue("bad_arr1: ';' is not a valid Fortran identifier"
                        in str(context.exception))
        self.assertTrue("bad_arr2: ';' is not a valid Fortran identifier"
                        in str(context.exception))
        self.assertTrue("bad_arr3: ';' is not a valid Fortran identifier"
                        in str(context.exception))
        self.assertTrue("Missing local_variables, ['bad_arr1', 'bad_arr2', 'bad_arr3'] in array_spec_test_run"
                        in str(context.exception))

if __name__ == "__main__":
    unittest.main()
