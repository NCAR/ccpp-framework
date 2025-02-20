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
                  os.path.join(_SRC_DIR, "ccpp_hashable.F90"),
                  os.path.join(_SRC_DIR, "ccpp_hash_table.F90")]
_CCPP_FILES = _UTILITY_FILES + \
              [os.path.join(_BUILD_DIR, "ccpp", "test_host_ccpp_cap.F90"),
               os.path.join(_BUILD_DIR, "ccpp", "ccpp_var_compatibility_suite_cap.F90")]
_DEPENDENCIES = [""]
_PROCESS_LIST = [""]
_MODULE_LIST = ["effr_calc", "effr_diag", "effr_post", "effr_pre"]
_SUITE_LIST = ["var_compatibility_suite"]
_INPUT_VARS_VAR_ACTION = ["horizontal_loop_begin", "horizontal_loop_end", "horizontal_dimension", "vertical_layer_dimension",
                          "effective_radius_of_stratiform_cloud_liquid_water_particle",
                          "effective_radius_of_stratiform_cloud_rain_particle",
                          "effective_radius_of_stratiform_cloud_snow_particle",
                          "effective_radius_of_stratiform_cloud_graupel",
                          "cloud_graupel_number_concentration",
                          "scalar_variable_for_testing",
                          "scalar_variable_for_testing_a",
                          "scalar_variable_for_testing_b",
                          "scalar_variable_for_testing_c",
                          "scheme_order_in_suite",
                          "flag_indicating_cloud_microphysics_has_graupel",
                          "flag_indicating_cloud_microphysics_has_ice"]
_OUTPUT_VARS_VAR_ACTION = ["ccpp_error_code", "ccpp_error_message",
                           "effective_radius_of_stratiform_cloud_ice_particle",
                           "effective_radius_of_stratiform_cloud_liquid_water_particle",
                           "effective_radius_of_stratiform_cloud_snow_particle",
                           "cloud_ice_number_concentration",
                           "effective_radius_of_stratiform_cloud_rain_particle",
                           "scalar_variable_for_testing",
                           "scheme_order_in_suite"]
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
