#! /usr/bin/env python3
"""
-----------------------------------------------------------------------
 Description:  Test capgen database report python interface

 Assumptions:

 Command line arguments: build_dir database_filepath

 Usage: python test_reports <build_dir> <database_filepath>
-----------------------------------------------------------------------
"""
import os
import unittest
from test_stub import BaseTests

_BUILD_DIR = os.path.join(os.path.abspath(os.environ['BUILD_DIR']), "test", "var_compatibility_test")
_DATABASE = os.path.abspath(os.path.join(_BUILD_DIR, "ccpp", "datatable.xml"))

_TEST_DIR = os.path.dirname(os.path.abspath(__file__))
_FRAMEWORK_DIR = os.path.abspath(os.path.join(_TEST_DIR, os.pardir, os.pardir))
_SCRIPTS_DIR = os.path.join(_FRAMEWORK_DIR, "scripts")
_SRC_DIR = os.path.join(_FRAMEWORK_DIR, "src")

# Check data
_HOST_FILES = [os.path.join(_BUILD_DIR, "ccpp", "test_host_ccpp_cap.F90")]
_SUITE_FILES = [os.path.join(_BUILD_DIR, "ccpp", "ccpp_var_compatibility_suite_cap.F90")]
_UTILITY_FILES = [os.path.join(_BUILD_DIR, "ccpp", "ccpp_kinds.F90"),
                  os.path.join(_SRC_DIR, "ccpp_constituent_prop_mod.F90"),
                  os.path.join(_SRC_DIR, "ccpp_scheme_utils.F90"),
                  os.path.join(_SRC_DIR, "ccpp_hashable.F90"),
                  os.path.join(_SRC_DIR, "ccpp_hash_table.F90")]
_CCPP_FILES = _UTILITY_FILES + \
              [os.path.join(_BUILD_DIR, "ccpp", "test_host_ccpp_cap.F90"),
               os.path.join(_BUILD_DIR, "ccpp", "ccpp_var_compatibility_suite_cap.F90")]
_PROCESS_LIST = [""]
_MODULE_LIST = ["effr_calc", "effrs_calc", "effr_diag", "effr_post", "mod_effr_pre", "rad_lw", "rad_sw"]
_SUITE_LIST = ["var_compatibility_suite"]
_DEPENDENCIES = [ os.path.join(_TEST_DIR, "module_rad_ddt.F90")]
_INPUT_VARS_VAR_ACTION = ["horizontal_loop_begin", "horizontal_loop_end", "horizontal_dimension", "vertical_layer_dimension",
                          "flag_indicating_cloud_microphysics_has_graupel",
                          "flag_indicating_cloud_microphysics_has_ice",
                          "num_subcycles_for_effr",
                          "physics_state_derived_type",
                          "effective_radius_of_stratiform_cloud_snow_particle"]
_OUTPUT_VARS_VAR_ACTION = ["ccpp_error_code", "ccpp_error_message","physics_state_derived_type",
                           "effective_radius_of_stratiform_cloud_snow_particle"]
_REQUIRED_VARS_VAR_ACTION = _INPUT_VARS_VAR_ACTION + _OUTPUT_VARS_VAR_ACTION


class TestVarCompatibilityHostDataTables(unittest.TestCase, BaseTests.TestHostDataTables):
    database = _DATABASE
    host_files = _HOST_FILES
    suite_files = _SUITE_FILES
    utility_files = _UTILITY_FILES
    ccpp_files = _CCPP_FILES
    process_list = _PROCESS_LIST
    module_list = _MODULE_LIST
    dependencies = _DEPENDENCIES
    suite_list = _SUITE_LIST


class CommandLineVarCompatibilityHostDatafileRequiredFiles(unittest.TestCase, BaseTests.TestHostCommandLineDataFiles):
    database = _DATABASE
    host_files = _HOST_FILES
    suite_files = _SUITE_FILES
    utility_files = _UTILITY_FILES
    ccpp_files = _CCPP_FILES
    process_list = _PROCESS_LIST
    module_list = _MODULE_LIST
    dependencies = _DEPENDENCIES
    suite_list = _SUITE_LIST
    datafile_script = f"{_SCRIPTS_DIR}/ccpp_datafile.py"


class TestCapgenDdtSuite(unittest.TestCase, BaseTests.TestSuite):
    database = _DATABASE
    required_vars = _REQUIRED_VARS_VAR_ACTION
    input_vars = _INPUT_VARS_VAR_ACTION
    output_vars = _OUTPUT_VARS_VAR_ACTION
    suite_name = "var_compatibility_suite"


class CommandLineCapgenDdtSuite(unittest.TestCase, BaseTests.TestSuiteCommandLine):
    database = _DATABASE
    required_vars = _REQUIRED_VARS_VAR_ACTION
    input_vars = _INPUT_VARS_VAR_ACTION
    output_vars = _OUTPUT_VARS_VAR_ACTION
    suite_name = "var_compatibility_suite"
    datafile_script = f"{_SCRIPTS_DIR}/ccpp_datafile.py"
