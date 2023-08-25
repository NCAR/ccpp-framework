#! /usr/bin/env python3

"""
-----------------------------------------------------------------------
 Description:  capgen needs to compare a metadata header against the
               associated CCPP Fortran interface routine.  This set of
               tests is testing the parse_host_model_files function in
               ccpp_capgen.py which performs the operations in the first
               bullet below. Each test calls this function.

               * This script contains unit tests that do the following:
                  1) Read one or more metadata files (to collect
                     the metadata headers)
                  2) Read the associated CCPP Fortran host file(s) (to
                     collect Fortran interfaces)
                  3) Create a CCPP host model object
                  3) Test the properties of the CCPP host model object

               * Tests include:
                  - Correctly parse and match a simple module file with
                       data fields (a data block)
                  - Correctly parse and match a simple module file with
                       a DDT definition
                  - Correctly parse and match a simple module file with
                       two DDT definitions
                  - Correctly parse and match a simple module file with
                       two DDT definitions and a data block

 Assumptions:

 Command line arguments: none

 Usage: python3 test_metadata_host_file.py       # run the unit tests
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
from ccpp_capgen import parse_host_model_files
from framework_env import CCPPFrameworkEnv
from parse_tools import CCPPError
# pylint: enable=wrong-import-position

class MetadataHeaderTestCase(unittest.TestCase):
    """Unit tests for parse_host_model_files"""

    def setUp(self):
        """Setup important directories and logging"""
        self._sample_files_dir = os.path.join(_TEST_DIR, "sample_host_files")
        logger = logging.getLogger(self.__class__.__name__)
        self._run_env = CCPPFrameworkEnv(logger, ndict={'host_files':'',
                                                        'scheme_files':'',
                                                        'suites':''})

    def test_module_with_data(self):
        """Test that a module containing a data block is parsed and matched
           correctly."""
        # Setup
        module_files = [os.path.join(self._sample_files_dir, "data1_mod.meta")]
        # Exercise
        hname = 'host_name_data1'
        host_model = parse_host_model_files(module_files, hname, self._run_env)
        # Verify the name of the host model
        self.assertEqual(host_model.name, hname)
        module_headers = host_model.metadata_tables()
        self.assertEqual(len(module_headers), 1)
        # Verify header titles
        self.assertTrue('data1_mod' in module_headers)
        # Verify host model variable list
        vlist = host_model.variable_list()
        self.assertEqual(len(vlist), 3)
        std_names = [x.get_prop_value('standard_name') for x in vlist]
        self.assertTrue('play_station' in std_names)
        self.assertTrue('xbox' in std_names)
        self.assertTrue('nintendo_switch' in std_names)

    def test_module_with_one_ddt(self):
        """Test that a module containing a DDT definition is parsed and matched
           correctly."""
        # Setup
        ddt_name = 'ddt1_t'
        module_files = [os.path.join(self._sample_files_dir, "ddt1.meta")]
        # Exercise
        hname = 'host_name_ddt1'
        host_model = parse_host_model_files(module_files, hname, self._run_env)
        # Verify the name of the host model
        self.assertEqual(host_model.name, hname)
        module_headers = host_model.metadata_tables()
        self.assertEqual(len(module_headers), 1)
        # Verify header titles
        self.assertTrue(ddt_name in module_headers)
        # Verify host model variable list
        vlist = host_model.variable_list()
        self.assertEqual(len(vlist), 0)
        # Verify that the DDT was found and parsed
        ddt_lib = host_model.ddt_lib
        self.assertEqual(ddt_lib.name, f"{hname}_ddts_ddt_lib")
        # Check DDT variables
        ddt_mod = ddt_lib[ddt_name]
        self.assertEqual(ddt_mod.name, ddt_name)
        vlist = ddt_mod.variable_list()
        self.assertEqual(len(vlist), 2)
        std_names = [x.get_prop_value('standard_name') for x in vlist]
        self.assertTrue('ddt_var_array_dimension' in std_names)
        self.assertTrue('vars_array' in std_names)

    def test_module_with_two_ddts(self):
        """Test that a module containing two DDT definitions is parsed and
           matched correctly."""
        # Setup
        ddt_names = ['ddt1_t', 'ddt2_t']
        ddt_vars = [(), ('ddt_var_array_dimension', 'vars_array')]

        module_files = [os.path.join(self._sample_files_dir, "ddt2.meta")]
        # Exercise
        hname = 'host_name_ddt2'
        host_model = parse_host_model_files(module_files, hname, self._run_env)
        # Verify the name of the host model
        self.assertEqual(host_model.name, hname)
        module_headers = host_model.metadata_tables()
        self.assertEqual(len(module_headers), len(ddt_names))
        # Verify header titles
        for ddt_name in ddt_names:
            self.assertTrue(ddt_name in module_headers)
        # end for
        # Verify host model variable list
        vlist = host_model.variable_list()
        self.assertEqual(len(vlist), 0)
        # Verify that each DDT was found and parsed
        ddt_lib = host_model.ddt_lib
        self.assertEqual(ddt_lib.name, f"{hname}_ddts_ddt_lib")
        # Check DDT variables
        for index, ddt_name in enumerate(ddt_names):
            ddt_mod = ddt_lib[ddt_name]
            self.assertEqual(ddt_mod.name, ddt_name)
            vlist = ddt_mod.variable_list()
            self.assertEqual(len(vlist), len(ddt_vars[index]))
            std_names = [x.get_prop_value('standard_name') for x in vlist]
            for sname in ddt_vars[index]:
                self.assertTrue(sname in std_names)
            # end for
        # end for

    def test_module_with_two_ddts_and_data(self):
        """Test that a module containing two DDT definitions and a block of
           module data is parsed and matched correctly."""
        # Setup
        ddt_names = ['ddt1_t', 'ddt2_t']
        ddt_vars = [(), ('ddt_var_array_dimension', 'vars_array')]

        module_files = [os.path.join(self._sample_files_dir,
                                     "ddt_data1_mod.meta")]
        # Exercise
        hname = 'host_name_ddt_data'
        host_model = parse_host_model_files(module_files, hname, self._run_env)
        # Verify the name of the host model
        self.assertEqual(host_model.name, hname)
        module_headers = host_model.metadata_tables()
        self.assertEqual(len(module_headers), len(ddt_names) + 1)
        # Verify header titles
        for ddt_name in ddt_names:
            self.assertTrue(ddt_name in module_headers)
        # end for
        # Verify host model variable list
        vlist = host_model.variable_list()
        self.assertEqual(len(vlist), 3)
        # Verify that each DDT was found and parsed
        ddt_lib = host_model.ddt_lib
        self.assertEqual(ddt_lib.name, f"{hname}_ddts_ddt_lib")
        # Check DDT variables
        for index, ddt_name in enumerate(ddt_names):
            ddt_mod = ddt_lib[ddt_name]
            self.assertEqual(ddt_mod.name, ddt_name)
            vlist = ddt_mod.variable_list()
            self.assertEqual(len(vlist), len(ddt_vars[index]))
            std_names = [x.get_prop_value('standard_name') for x in vlist]
            for sname in ddt_vars[index]:
                self.assertTrue(sname in std_names)
            # end for
        # end for
        # Verify header titles
        self.assertTrue('ddt_data1_mod' in module_headers)
        # Verify host model variable list
        vlist = host_model.variable_list()
        self.assertEqual(len(vlist), 3)
        std_names = [x.get_prop_value('standard_name') for x in vlist]
        self.assertTrue('play_station' in std_names)
        self.assertTrue('xbox' in std_names)
        self.assertTrue('nintendo_switch' in std_names)

    def test_module_with_one_ddt_plus_undoc(self):
        """Test that a module containing a one documented DDT definition
           (i.e., with metadata) and one DDT without (i.e., no metadata)
           is parsed and matched correctly."""
        # Setup
        ddt_name = 'ddt2_t'
        module_files = [os.path.join(self._sample_files_dir, "ddt1_plus.meta")]
        # Exercise
        hname = 'host_name_ddt1_plus'
        host_model = parse_host_model_files(module_files, hname, self._run_env)
        # Verify the name of the host model
        self.assertEqual(host_model.name, hname)
        module_headers = host_model.metadata_tables()
        self.assertEqual(len(module_headers), 1)
        # Verify header titles
        self.assertTrue(ddt_name in module_headers)
        # Verify host model variable list
        vlist = host_model.variable_list()
        self.assertEqual(len(vlist), 0)
        # Verify that the DDT was found and parsed
        ddt_lib = host_model.ddt_lib
        self.assertEqual(ddt_lib.name, f"{hname}_ddts_ddt_lib")
        # Check DDT variables
        ddt_mod = ddt_lib[ddt_name]
        self.assertEqual(ddt_mod.name, ddt_name)
        vlist = ddt_mod.variable_list()
        self.assertEqual(len(vlist), 2)
        std_names = [x.get_prop_value('standard_name') for x in vlist]
        self.assertTrue('ddt_var_array_dimension' in std_names)
        self.assertTrue('vars_array' in std_names)

    def test_module_with_two_ddts_and_extra_var(self):
        """Test that a module containing two DDT definitions is parsed and
           a useful error message is produced if the DDT metadata has an
           extra variable."""
        # Setup
        ddt_names = ['ddt1_t', 'ddt2_t']
        ddt_vars = [(), ('ddt_var_array_dimension', 'vars_array')]

        module_files = [os.path.join(self._sample_files_dir,
                                     "ddt2_extra_var.meta")]
        # Exercise
        hname = 'host_name_ddt_extra_var'
        with self.assertRaises(CCPPError) as context:
            host_model = parse_host_model_files(module_files, hname,
                                                self._run_env)
        # end with
        # Check error messages
        except_str = str(context.exception)
        emsgs = ["Variable mismatch in ddt2_t, variables missing from Fortran ddt.",

                 "No Fortran variable for bogus in ddt2_t",
                 "2 errors found comparing"]
        for emsg in emsgs:
            self.assertTrue(emsg in except_str)
        # end for

if __name__ == "__main__":
    unittest.main()

