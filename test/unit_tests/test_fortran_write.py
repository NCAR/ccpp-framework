#! /usr/bin/env python3
"""
-----------------------------------------------------------------------
 Description:  Contains unit tests for FortranWriter
               in scripts file fortran/fortran_write.py

 Assumptions:

 Command line arguments: none

 Usage: python3 test_fortran_write.py         # run the unit tests
-----------------------------------------------------------------------
"""

import filecmp
import glob
import os
import sys
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
from fortran_tools import FortranWriter
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

class MetadataTableTestCase(unittest.TestCase):

    """Tests for `FortranWriter`."""

    @classmethod
    def setUpClass(cls):
        """Clean output directory (tmp) before running tests"""
        #Does "tmp" directory exist?  If not then create it:
        if not os.path.exists(_PRE_TMP_DIR):
            os.mkdir(_PRE_TMP_DIR)
        # Ensure the "sample_files/fortran_files" directory exists and is empty
        if os.path.exists(_TMP_DIR):
            # Clear out all files:
            remove_files(glob.iglob(os.path.join(_TMP_DIR, '*.*')))
        else:
            os.makedirs(_TMP_DIR)
        # end if

        #Run inherited setup method:
        super().setUpClass()

    def test_line_breaking(self):
        """Test that FortranWriter correctly breaks long lines"""
        # Setup
        testname = "linebreak_test"
        compare = os.path.join(_SAMPLE_FILES_DIR, f"{testname}.F90")
        generate = os.path.join(_TMP_DIR, f"{testname}.F90")
        # Exercise
        header = "Test of line breaking for FortranWriter"
        with FortranWriter(generate, 'w', header, f"{testname}") as gen:
            # Test long declaration
            data_items = ', '.join([f"name{x:03}" for x in range(100)])
            gen.write(f"character(len=7) :: data = (/ {data_items} /)", 1)
            gen.end_module_header()
            # Test long code lines
            line_items = ["call endrun('Cannot read columns_on_task from ",
                          "file'//', columns_on_task has no horizontal ",
                          "dimension; columns_on_task is a ",
                          "protected variable')"]
            gen.write(f"{''.join(line_items)}", 2)
        # end with

        # Check that file was generated
        amsg = f"{generate} does not exist"
        self.assertTrue(os.path.exists(generate), msg=amsg)
        amsg = f"{generate} does not match {compare}"
        self.assertTrue(filecmp.cmp(generate, compare, shallow=False), msg=amsg)

    def test_good_comments(self):
        """Test that comments are written and broken correctly."""
        # Setup
        testname = "comments_test"
        compare = os.path.join(_SAMPLE_FILES_DIR, f"{testname}.F90")
        generate = os.path.join(_TMP_DIR, f"{testname}.F90")
        # Exercise
        header = "Test of comment writing for FortranWriter"
        with FortranWriter(generate, 'w', header, f"{testname}") as gen:
            gen.comment("We can write comments in the module header", 0)
            gen.comment("We can write indented comments in the header", 1)
            gen.write("integer :: foo ! Comment at end of line works", 1)
            # Test long comments at end of line
            gen.write(f"integer :: bar ! {'x'*100}", 1)
            gen.write(f"integer :: baz ! {'y'*130}", 1)
            gen.end_module_header()
            # Test comment line in body
            gen.comment("We can write comments in the module body", 1)

        # end with

        # Check that file was generated
        amsg = f"{generate} does not exist"
        self.assertTrue(os.path.exists(generate), msg=amsg)
        amsg = f"{generate} does not match {compare}"
        self.assertTrue(filecmp.cmp(generate, compare, shallow=False), msg=amsg)

if __name__ == "__main__":
    unittest.main()

