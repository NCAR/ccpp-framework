#! /usr/bin/env python3
"""
-----------------------------------------------------------------------
 Description:  Test advection database report python interface

 Assumptions:

 Command line arguments: build_dir database_filepath

 Usage: python test_reports <build_dir> <database_filepath>
-----------------------------------------------------------------------
"""
import os
import unittest

from test_stub import BaseTests

_BUILD_DIR = os.path.join(os.path.abspath(os.environ['BUILD_DIR']), "test", "advection_test")
_DATABASE = os.path.abspath(os.path.join(_BUILD_DIR, "ccpp", "datatable.xml"))

_TEST_DIR = os.path.dirname(os.path.abspath(__file__))
_FRAMEWORK_DIR = os.path.abspath(os.path.join(_TEST_DIR, os.pardir, os.pardir))
_SCRIPTS_DIR = os.path.abspath(os.path.join(_FRAMEWORK_DIR, "scripts"))

# Check data
_HOST_FILES = [os.path.join(_BUILD_DIR, "ccpp", "test_host_ccpp_cap.F90")]
_SUITE_FILES = [os.path.join(_BUILD_DIR, "ccpp", "ccpp_cld_suite_cap.F90")]
_UTILITY_FILES = [os.path.join(_BUILD_DIR, "ccpp", "ccpp_kinds.F90"),
                  os.path.join(_FRAMEWORK_DIR, "src",
                               "ccpp_constituent_prop_mod.F90"),
                  os.path.join(_FRAMEWORK_DIR, "src",
                               "ccpp_scheme_utils.F90"),
                  os.path.join(_FRAMEWORK_DIR, "src", "ccpp_hashable.F90"),
                  os.path.join(_FRAMEWORK_DIR, "src", "ccpp_hash_table.F90")]
_CCPP_FILES = _UTILITY_FILES + _HOST_FILES + _SUITE_FILES
_DEPENDENCIES = [""]
_PROCESS_LIST = [""]
_MODULE_LIST = ["cld_ice", "cld_liq", "const_indices", "apply_constituent_tendencies"]
_SUITE_LIST = ["cld_suite"]
_REQUIRED_VARS_CLD = ["ccpp_error_code", "ccpp_error_message",
                      "horizontal_loop_begin", "horizontal_loop_end",
                      "surface_air_pressure", "temperature",
                      "tendency_of_cloud_liquid_dry_mixing_ratio",
                      "time_step_for_physics", "water_temperature_at_freezing",
                      "water_vapor_specific_humidity",
                      "cloud_ice_dry_mixing_ratio",
                      "cloud_liquid_dry_mixing_ratio",
                      "ccpp_constituents",
                      "ccpp_constituent_tendencies",
                      "number_of_ccpp_constituents",
                      "dynamic_constituents_for_cld_ice",
                      "dynamic_constituents_for_cld_liq",
                      "test_banana_constituent_indices", "test_banana_name",
                      "banana_array_dim",
                      "test_banana_name_array",
                      "test_banana_constituent_index",
                      # Added by --debug option
                      "horizontal_dimension",
                      "vertical_layer_dimension"]
_INPUT_VARS_CLD = ["surface_air_pressure", "temperature",
                   "horizontal_loop_begin", "horizontal_loop_end",
                   "time_step_for_physics", "water_temperature_at_freezing",
                   "water_vapor_specific_humidity",
                   "cloud_ice_dry_mixing_ratio",
                   "cloud_liquid_dry_mixing_ratio",
                   "tendency_of_cloud_liquid_dry_mixing_ratio",
                   "ccpp_constituents",
                   "ccpp_constituent_tendencies",
                   "number_of_ccpp_constituents",
                   "banana_array_dim",
                   "test_banana_name_array", "test_banana_name",
                   # Added by --debug option
                   "horizontal_dimension",
                   "vertical_layer_dimension"]
_OUTPUT_VARS_CLD = ["ccpp_error_code", "ccpp_error_message",
                    "water_vapor_specific_humidity", "temperature",
                    "tendency_of_cloud_liquid_dry_mixing_ratio",
                    "cloud_ice_dry_mixing_ratio",
                    "ccpp_constituents",
                    "ccpp_constituent_tendencies",
                    "cloud_liquid_dry_mixing_ratio",
                    "dynamic_constituents_for_cld_ice",
                    "dynamic_constituents_for_cld_liq",
                    "dynamic_constituents_for_cld_liq",
                    "test_banana_constituent_indices",
                    "test_banana_constituent_index"]


class TestAdvectionHostDataTables(unittest.TestCase, BaseTests.TestHostDataTables):
    database = _DATABASE
    host_files = _HOST_FILES
    suite_files = _SUITE_FILES
    utility_files = _UTILITY_FILES
    ccpp_files = _CCPP_FILES
    process_list = _PROCESS_LIST
    module_list = _MODULE_LIST
    dependencies = _DEPENDENCIES
    suite_list = _SUITE_LIST

class CommandLineAdvectionHostDatafileRequiredFiles(unittest.TestCase, BaseTests.TestHostCommandLineDataFiles):
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


class TestCapgenCldSuite(unittest.TestCase, BaseTests.TestSuite):
    database = _DATABASE
    required_vars = _REQUIRED_VARS_CLD
    input_vars = _INPUT_VARS_CLD
    output_vars = _OUTPUT_VARS_CLD
    suite_name = "cld_suite"


class CommandLineCapgenDdtSuite(unittest.TestCase, BaseTests.TestSuiteCommandLine):
    database = _DATABASE
    required_vars = _REQUIRED_VARS_CLD
    input_vars = _INPUT_VARS_CLD
    output_vars = _OUTPUT_VARS_CLD
    suite_name = "cld_suite"
    datafile_script = f"{_SCRIPTS_DIR}/ccpp_datafile.py"
