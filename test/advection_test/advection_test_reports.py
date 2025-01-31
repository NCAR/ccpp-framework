#! /usr/bin/env python3
"""
-----------------------------------------------------------------------
 Description:  Test advection database report python interface

 Assumptions:

 Command line arguments: build_dir database_filepath

 Usage: python test_reports <build_dir> <database_filepath>
-----------------------------------------------------------------------
"""
import sys
import os
import unittest
import subprocess

_BUILD_DIR = os.path.join(os.path.abspath(os.environ['BUILD_DIR']), "test", "advection_test")
_TEST_DIR = os.path.dirname(os.path.abspath(__file__))
_FRAMEWORK_DIR = os.path.abspath(os.path.join(_TEST_DIR, os.pardir, os.pardir))
_SCRIPTS_DIR = os.path.abspath(os.path.join(_FRAMEWORK_DIR, "scripts"))

sys.path.append(_SCRIPTS_DIR)
# pylint: disable=wrong-import-position
from ccpp_datafile import datatable_report, DatatableReport
# pylint: enable=wrong-import-position

_DATABASE = os.path.abspath(os.path.join(_BUILD_DIR, "ccpp", "datatable.xml"))

# Check data
_HOST_FILES = [os.path.join(_BUILD_DIR, "ccpp", "test_host_ccpp_cap.F90")]
_SUITE_FILES = [os.path.join(_BUILD_DIR, "ccpp", "ccpp_cld_suite_cap.F90")]
_UTILITY_FILES = [os.path.join(_BUILD_DIR, "ccpp", "ccpp_kinds.F90"),
                  os.path.join(_FRAMEWORK_DIR, "src",
                               "ccpp_constituent_prop_mod.F90"),
                  os.path.join(_FRAMEWORK_DIR, "src", "ccpp_hashable.F90"),
                  os.path.join(_FRAMEWORK_DIR, "src", "ccpp_hash_table.F90")]
_CCPP_FILES = _UTILITY_FILES + _HOST_FILES + _SUITE_FILES
_PROCESS_LIST = list()
_MODULE_LIST = ["cld_ice", "cld_liq", "apply_constituent_tendencies"]
_SUITE_LIST = ["cld_suite"]
_DYN_CONST_ROUTINES = ["cld_ice_dynamic_constituents", "cld_liq_dynamic_constituents"]
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
                    "dynamic_constituents_for_cld_liq"]
_SEP = ","

class TestAdvectionDataTables(unittest.TestCase):
    def test_host_files(self):
        test_str = datatable_report(_DATABASE, DatatableReport("host_files"), _SEP)
        self.assertSetEqual(set(_HOST_FILES), set(test_str.split(_SEP)))

    def test_suite_files(self):
        test_str = datatable_report(_DATABASE, DatatableReport("suite_files"), _SEP)
        self.assertSetEqual(set(_SUITE_FILES), set(test_str.split(_SEP)))

    def test_utility_files(self):
        test_str = datatable_report(_DATABASE, DatatableReport("utility_files"), _SEP)
        self.assertSetEqual(set(_UTILITY_FILES), set(test_str.split(_SEP)))

    def test_ccpp_files(self):
        test_str = datatable_report(_DATABASE, DatatableReport("ccpp_files"), _SEP)
        self.assertSetEqual(set(_CCPP_FILES), set(test_str.split(_SEP)))
    
    def test_process_list(self):
        test_str = datatable_report(_DATABASE, DatatableReport("process_list"), _SEP)
        self.assertSetEqual(set(_PROCESS_LIST), set(test_str.split(_SEP)))

    def test_module_list(self):
        test_str = datatable_report(_DATABASE, DatatableReport("module_list"), _SEP)
        self.assertSetEqual(set(_MODULE_LIST), set(test_str.split(_SEP)))

    def test_suite_list(self):
        test_str = datatable_report(_DATABASE, DatatableReport("suite_list"), _SEP)
        self.assertSetEqual(set(_SUITE_LIST), set(test_str.split(_SEP)))

class CommandLineAdvectionDatafileRequiredFiles(unittest.TestCase):
    def test_host_files(self):
        completedProcess = subprocess.run([f"{_SCRIPTS_DIR}/ccpp_datafile.py", _DATABASE, "--host-files"],
                                          capture_output=True,
                                          text=True)
        actualOutput = {s.strip() for s in completedProcess.stdout.split(_SEP)}
        self.assertSetEqual(set(_HOST_FILES), actualOutput)

    def test_suite_files(self):
        completedProcess = subprocess.run([f"{_SCRIPTS_DIR}/ccpp_datafile.py", _DATABASE, "--suite-files"],
                                          capture_output=True,
                                          text=True)
        self.assertEqual(_SEP.join(_SUITE_FILES), completedProcess.stdout.strip())

    def test_utility_files(self):
        completedProcess = subprocess.run([f"{_SCRIPTS_DIR}/ccpp_datafile.py", _DATABASE, "--utility-files"],
                                          capture_output=True,
                                          text=True)
        self.assertEqual(_SEP.join(_UTILITY_FILES), completedProcess.stdout.strip())

    def test_ccpp_files(self):
        completedProcess = subprocess.run([f"{_SCRIPTS_DIR}/ccpp_datafile.py", _DATABASE, "--ccpp-files"],
                                          capture_output=True,
                                          text=True)
        self.assertEqual(_SEP.join(_CCPP_FILES), completedProcess.stdout.strip())

    def test_process_list(self):
        completedProcess = subprocess.run([f"{_SCRIPTS_DIR}/ccpp_datafile.py", _DATABASE, "--process-list"],
                                          capture_output=True,
                                          text=True)
        actualOutput = {s.strip() for s in completedProcess.stdout.split(_SEP)}
        self.assertSetEqual(set(_PROCESS_LIST), actualOutput)

    def test_module_list(self):
        completedProcess = subprocess.run([f"{_SCRIPTS_DIR}/ccpp_datafile.py", _DATABASE, "--module-list"],
                                          capture_output=True,
                                          text=True)
        self.assertEqual(_SEP.join(_MODULE_LIST), completedProcess.stdout.strip())

    def test_dependencies(self):
        completedProcess = subprocess.run([f"{_SCRIPTS_DIR}/ccpp_datafile.py", _DATABASE, "--dependencies"],
                                          capture_output=True,
                                          text=True)
        self.assertEqual(_SEP.join(_DEPENDENCIES), completedProcess.stdout.strip())

    def test_suite_list(self):
        completedProcess = subprocess.run([f"{_SCRIPTS_DIR}/ccpp_datafile.py", _DATABASE, "--suite-list", "--sep=;"],
                                          capture_output=True,
                                          text=True)
        # actualOutput = {s.strip() for s in completedProcess.stdout.split(";")}
        self.assertEqual(";".join(_SUITE_LIST), completedProcess.stdout.strip())

class TestCldSuite(unittest.TestCase):
    def test_required_variables(self):
        test_str = datatable_report(_DATABASE, DatatableReport("required_variables", value="cld_suite"), _SEP)
        self.assertSetEqual(set(_REQUIRED_VARS_CLD), set(test_str.split(_SEP)))

    def test_input_variables(self):
        test_str = datatable_report(_DATABASE, DatatableReport("input_variables", value="cld_suite"), _SEP)
        self.assertSetEqual(set(_INPUT_VARS_CLD), set(test_str.split(_SEP)))

    def test_output_variables(self):
        test_str = datatable_report(_DATABASE, DatatableReport("output_variables", value="cld_suite"), _SEP)
        self.assertSetEqual(set(_OUTPUT_VARS_CLD), set(test_str.split(_SEP)))

class CommandLineCldSuite(unittest.TestCase):
    def test_required_variables(self):
        completedProcess = subprocess.run([f"{_SCRIPTS_DIR}/ccpp_datafile.py", _DATABASE, "--required-variables", "cld_suite"],
                                          capture_output=True,
                                          text=True)
        actualOutput = {s.strip() for s in completedProcess.stdout.split(_SEP)}
        self.assertSetEqual(set(_REQUIRED_VARS_CLD), actualOutput)

    def test_input_variables(self):
        completedProcess = subprocess.run([f"{_SCRIPTS_DIR}/ccpp_datafile.py", _DATABASE, "--input-variables", "cld_suite"],
                                          capture_output=True,
                                          text=True)
        actualOutput = {s.strip() for s in completedProcess.stdout.split(_SEP)}
        self.assertSetEqual(set(_INPUT_VARS_CLD), actualOutput)

    def test_output_variables(self):
        completedProcess = subprocess.run([f"{_SCRIPTS_DIR}/ccpp_datafile.py", _DATABASE, "--output-variables", "cld_suite"],
                                          capture_output=True,
                                          text=True)
        self.assertEqual(_SEP.join(_OUTPUT_VARS_CLD), completedProcess.stdout.strip())
