#! /usr/bin/env python3

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

 Usage: python3 test_metadata_scheme_file.py       # run the unit tests
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
from framework_env import CCPPFrameworkEnv
from parse_tools import CCPPError
# pylint: enable=wrong-import-position

class MetadataHeaderTestCase(unittest.TestCase):
    """Unit tests for parse_scheme_files"""

    def setUp(self):
        """Setup important directories and logging"""
        self._sample_files_dir = os.path.join(_TEST_DIR, "sample_scheme_files")
        self._host_files_dir = os.path.join(_TEST_DIR, "sample_host_files")
        logger = logging.getLogger(self.__class__.__name__)
        self._run_env = CCPPFrameworkEnv(logger, ndict={'host_files':'',
                                                        'scheme_files':'',
                                                        'suites':''})
        self._run_env_ccpp = CCPPFrameworkEnv(logger,
                                              ndict={'host_files':'',
                                                     'scheme_files':'',
                                                     'suites':'',
                                                     'preproc_directives':
                                                     'CCPP=1'})
        self._run_env_ccpp2 = CCPPFrameworkEnv(logger,
                                               ndict={'host_files':'',
                                                      'scheme_files':'',
                                                      'suites':'',
                                                      'preproc_directives':
                                                      'CCPP=2'})

    def test_good_scheme_file(self):
        """Test that good metadata file matches the Fortran,
           with routines in the same order """
        # Setup
        scheme_files = [os.path.join(self._sample_files_dir,
                                     "temp_adjust.meta")]
        # Exercise
        scheme_headers, table_dict = parse_scheme_files(scheme_files,
                                                        self._run_env)
        # Verify size of returned list equals number of scheme headers
        #   in the test file and that header (subroutine) names are
        #   'temp_adjust_[init,run,finalize]'
        self.assertEqual(len(scheme_headers), 3)
        # Verify header titles
        titles = [elem.title for elem in scheme_headers]
        self.assertTrue('temp_adjust_init' in titles)
        self.assertTrue('temp_adjust_run' in titles)
        self.assertTrue('temp_adjust_finalize' in titles)
        # Verify size and name of table_dict matches scheme name
        self.assertEqual(len(table_dict), 1)
        self.assertTrue('temp_adjust' in table_dict)

    def test_reordered_scheme_file(self):
        """Test that metadata file matches the Fortran when the
           routines are not in the same order """
        # Setup
        scheme_files = [os.path.join(self._sample_files_dir, "reorder.meta")]
        # Exercise
        scheme_headers, table_dict = parse_scheme_files(scheme_files,
                                                        self._run_env)
        # Verify size of returned list equals number of scheme headers
        #   in the test file and that header (subroutine) names are
        #   'reorder_[init,run,finalize]'
        self.assertEqual(len(scheme_headers), 3)
        # Verify header titles
        titles = [elem.title for elem in scheme_headers]
        self.assertTrue('reorder_init' in titles)
        self.assertTrue('reorder_run' in titles)
        self.assertTrue('reorder_finalize' in titles)
        # Verify size and name of table_dict matches scheme name
        self.assertEqual(len(table_dict), 1)
        self.assertTrue('reorder' in table_dict)

    def test_missing_metadata_header(self):
        """Test that a missing metadata header (aka arg table) is
           corretly detected """
        # Setup
        scheme_files = [os.path.join(self._sample_files_dir,
                                     "missing_arg_table.meta")]
        # Exercise
        with self.assertRaises(CCPPError) as context:
            parse_scheme_files(scheme_files, self._run_env)
        # Verify correct error message returned
        emsg = "No matching metadata header found for missing_arg_table_run in"
        self.assertTrue(emsg in str(context.exception))

    def test_missing_fortran_header(self):
        """Test that a missing fortran header is corretly detected """
        # Setup
        scheme_files = [os.path.join(self._sample_files_dir,
                                     "missing_fort_header.meta")]
        # Exercise
        with self.assertRaises(CCPPError) as context:
            parse_scheme_files(scheme_files, self._run_env)
        # Verify correct error message returned
        emsg = "No matching Fortran routine found for missing_fort_header_run in"
        self.assertTrue(emsg in str(context.exception))

    def test_mismatch_intent(self):
        """Test that differing intent, kind, rank, and type between
           metadata and fortran is corretly detected """
        # Setup
        scheme_files = [os.path.join(self._sample_files_dir,
                                     "mismatch_intent.meta")]
        # Exercise
        with self.assertRaises(CCPPError) as context:
            parse_scheme_files(scheme_files, self._run_env)
        # Verify 4 correct error messages returned
        emsg = "intent mismatch (in != inout) in mismatch_intent_run, at"
        self.assertTrue(emsg in str(context.exception))
        emsg = "kind mismatch (kind_fizz != kind_phys) in mismatch_intent_run, at"
        self.assertTrue(emsg in str(context.exception))
        emsg = "rank mismatch in mismatch_intent_run/potential_temperature (0 != 1), at"
        self.assertTrue(emsg in str(context.exception))
        emsg = "type mismatch (integer != real) in mismatch_intent_run, at"
        self.assertTrue(emsg in str(context.exception))
        self.assertTrue("4 errors found comparing" in str(context.exception))

    def test_invalid_subr_stmnt(self):
        """Test that invalid Fortran subroutine statements are correctly
           detected """
        # Setup
        scheme_files = [os.path.join(self._sample_files_dir,
                                     "invalid_subr_stmnt.meta")]
        # Exercise
        with self.assertRaises(CCPPError) as context:
            parse_scheme_files(scheme_files, self._run_env)
        # Verify correct error message returned
        self.assertTrue("Invalid dummy argument, 'errmsg', at"
                        in str(context.exception))

    def test_invalid_dummy_arg(self):
        """Test that invalid dummy argument statements are correctly detected"""
        # Setup
        scheme_files = [os.path.join(self._sample_files_dir,
                                     "invalid_dummy_arg.meta")]
        # Exercise
        with self.assertRaises(CCPPError) as context:
            parse_scheme_files(scheme_files, self._run_env)
        # Verify correct error message returned
        emsg = "Invalid dummy argument, 'woohoo', at"
        self.assertTrue(emsg in str(context.exception))

    def test_ccpp_notset_var_missing_in_meta(self):
        """Test for correct detection of a variable that REMAINS in the
           subroutine argument list
           (due to an undefined pre-processor directive: #ifndef CCPP),
            BUT IS NOT PRESENT in meta file"""
        # Setup
        scheme_files = [os.path.join(self._sample_files_dir,
                                     "CCPPnotset_var_missing_in_meta.meta")]
        # Exercise
        with self.assertRaises(CCPPError) as context:
            parse_scheme_files(scheme_files, self._run_env)
        # Verify 3 correct error messages returned
        emsg = "Variable mismatch in CCPPnotset_var_missing_in_meta_run, " +  \
            "variables missing from metadata header."
        self.assertTrue(emsg in str(context.exception))
        emsg = "Out of order argument, errmsg in CCPPnotset_var_missing_in_meta_run"
        self.assertTrue(emsg in str(context.exception))
        emsg = "Out of order argument, errflg in CCPPnotset_var_missing_in_meta_run"
        self.assertTrue(emsg in str(context.exception))
        self.assertTrue("3 errors found comparing" in str(context.exception))

    def test_ccpp_eq1_var_missing_in_fort(self):
        """Test for correct detection of a variable that IS REMOVED the
           subroutine argument list
           (due to a pre-processor directive: #ifndef CCPP),
            but IS PRESENT in meta file"""
        # Setup
        scheme_files = [os.path.join(self._sample_files_dir,
                                     "CCPPeq1_var_missing_in_fort.meta")]
        # Exercise
        with self.assertRaises(CCPPError) as context:
            parse_scheme_files(scheme_files, self._run_env_ccpp)
        # Verify 3 correct error messages returned
        emsg = "Variable mismatch in CCPPeq1_var_missing_in_fort_run, " +     \
            "variables missing from Fortran scheme."
        self.assertTrue(emsg in str(context.exception))
        emsg = "Variable mismatch in CCPPeq1_var_missing_in_fort_run, " +     \
            "no Fortran variable bar."
        self.assertTrue(emsg in str(context.exception))
        emsg = "Out of order argument, errmsg in CCPPeq1_var_missing_in_fort_run"
        self.assertTrue(emsg in str(context.exception))
        self.assertTrue("3 errors found comparing" in str(context.exception))

    def test_ccpp_eq1_var_in_fort_meta(self):
        """Test positive case of a variable that IS PRESENT the
           subroutine argument list
           (due to a pre-processor directive: #ifdef CCPP),
            and IS PRESENT in meta file"""
        # Setup
        scheme_files = [os.path.join(self._sample_files_dir,
                                     "CCPPeq1_var_in_fort_meta.meta")]
        # Exercise
        scheme_headers, table_dict = parse_scheme_files(scheme_files,
                                                        self._run_env_ccpp)
        # Verify size of returned list equals number of scheme headers in
        #   the test file (1) and that header (subroutine) name is
        #   'CCPPeq1_var_in_fort_meta_run'
        self.assertEqual(len(scheme_headers), 1)
        # Verify header titles
        titles = [elem.title for elem in scheme_headers]
        self.assertTrue("CCPPeq1_var_in_fort_meta_run" in titles)

        # Verify size and name of table_dict matches scheme name
        self.assertEqual(len(table_dict), 1)
        self.assertTrue("CCPPeq1_var_in_fort_meta" in table_dict)

    def test_ccpp_gt1_var_in_fort_meta(self):
        """Test positive case of a variable that IS PRESENT the
           subroutine argument list
           (due to a pre-processor directive: #if CCPP > 1),
            and IS PRESENT in meta file"""
        # Setup
        scheme_files = [os.path.join(self._sample_files_dir,
                                     "CCPPgt1_var_in_fort_meta.meta")]
        # Exercise
        # Set CCPP directive to > 1
        scheme_headers, table_dict = parse_scheme_files(scheme_files,
                                                        self._run_env_ccpp2)
        # Verify size of returned list equals number of scheme headers
        #   in the test file (1) and that header (subroutine) name is
        #   'CCPPgt1_var_in_fort_meta_init'
        self.assertEqual(len(scheme_headers), 1)
        # Verify header titles
        titles = [elem.title for elem in scheme_headers]
        self.assertTrue("CCPPgt1_var_in_fort_meta_init" in titles)

        # Verify size and name of table_dict matches scheme name
        self.assertEqual(len(table_dict), 1)
        self.assertTrue("CCPPgt1_var_in_fort_meta" in table_dict)

    def test_ccpp_gt1_var_in_fort_meta2(self):
        """Test correct detection of a variable that
           IS NOT PRESENT the subroutine argument list
           (due to a pre-processor directive: #if CCPP > 1),
            but IS PRESENT in meta file"""
        # Setup
        scheme_files = [os.path.join(self._sample_files_dir,
                                     "CCPPgt1_var_in_fort_meta.meta")]
        # Exercise
        with self.assertRaises(CCPPError) as context:
            _, _ = parse_scheme_files(scheme_files, self._run_env_ccpp)
        # Verify 3 correct error messages returned
        emsg = "Variable mismatch in CCPPgt1_var_in_fort_meta_init, " +       \
            "variables missing from Fortran scheme."
        self.assertTrue(emsg in str(context.exception))
        emsg = "Variable mismatch in CCPPgt1_var_in_fort_meta_init, " +       \
            "no Fortran variable bar."
        self.assertTrue(emsg in str(context.exception))
        emsg = "Out of order argument, errmsg in CCPPgt1_var_in_fort_meta_init"
        self.assertTrue(emsg in str(context.exception))
        self.assertTrue("3 errors found comparing" in str(context.exception))

    def test_ccpp_eq1_var_missing_in_meta(self):
        """Test correct detection of a variable that
           IS PRESENT the subroutine argument list
           (due to a pre-processor directive: #ifdef CCPP),
            and IS NOT PRESENT in meta file"""
        # Setup
        scheme_files = [os.path.join(self._sample_files_dir,
                                     "CCPPeq1_var_missing_in_meta.meta")]
        # Exercise
        with self.assertRaises(CCPPError) as context:
             _, _ = parse_scheme_files(scheme_files, self._run_env_ccpp)
        # Verify 3 correct error messages returned
        emsg = "Variable mismatch in CCPPeq1_var_missing_in_meta_finalize, "+ \
            "variables missing from metadata header."
        self.assertTrue(emsg in str(context.exception))
        emsg = "Out of order argument, errmsg in CCPPeq1_var_missing_in_meta_finalize"
        self.assertTrue(emsg in str(context.exception))
        emsg = "Out of order argument, errflg in CCPPeq1_var_missing_in_meta_finalize"
        self.assertTrue(emsg in str(context.exception))
        self.assertTrue("3 errors found comparing" in str(context.exception))

    def test_scheme_ddt_only(self):
        """Test correct detection of a "scheme" file which contains only
           DDT definitions"""
        # Setup
        scheme_files = [os.path.join(self._host_files_dir, "ddt2.meta")]
        # Exercise
        scheme_headers, table_dict = parse_scheme_files(scheme_files,
                                                        self._run_env_ccpp)

if __name__ == "__main__":
    unittest.main()

