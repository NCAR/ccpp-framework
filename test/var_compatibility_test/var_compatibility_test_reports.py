#! /usr/bin/env python3
"""
-----------------------------------------------------------------------
 Description:  Test capgen database report python interface

 Assumptions:

 Command line arguments: build_dir database_filepath

 Usage: python test_reports <build_dir> <database_filepath>
-----------------------------------------------------------------------
"""
import sys
import os
import unittest
import subprocess

_BUILD_DIR = os.path.join(os.path.abspath(os.environ['BUILD_DIR']), "test", "var_compatibility_test")

_TEST_DIR = os.path.dirname(os.path.abspath(__file__))
_FRAMEWORK_DIR = os.path.abspath(os.path.join(_TEST_DIR, os.pardir, os.pardir))
_SCRIPTS_DIR = os.path.join(_FRAMEWORK_DIR, "scripts")
_SRC_DIR = os.path.join(_FRAMEWORK_DIR, "src")

sys.path.append(_SCRIPTS_DIR)
# pylint: disable=wrong-import-position
from ccpp_datafile import datatable_report, DatatableReport
# pylint: enable=wrong-import-position

_DATABASE = os.path.abspath(os.path.join(_BUILD_DIR, "ccpp", "datatable.xml"))

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
                          "flag_indicating_cloud_microphysics_has_graupel",
                          "flag_indicating_cloud_microphysics_has_ice"]
_OUTPUT_VARS_VAR_ACTION = ["ccpp_error_code", "ccpp_error_message",
                           "effective_radius_of_stratiform_cloud_ice_particle",
                           "effective_radius_of_stratiform_cloud_liquid_water_particle",
                           "effective_radius_of_stratiform_cloud_snow_particle",
                           "cloud_ice_number_concentration",
                           "effective_radius_of_stratiform_cloud_rain_particle",
                           "scalar_variable_for_testing"]
_REQUIRED_VARS_VAR_ACTION = _INPUT_VARS_VAR_ACTION + _OUTPUT_VARS_VAR_ACTION
_SEP = ","

class TestVarCompatibilityDataTables(unittest.TestCase):
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


class CommandLineVarCompatibilityDatafileRequiredFiles(unittest.TestCase):
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


class TestVarCompatibilitySuite(unittest.TestCase):
    def test_required_variables(self):
        test_str = datatable_report(_DATABASE, DatatableReport("required_variables", value="var_compatibility_suite"), _SEP)
        self.assertSetEqual(set(_REQUIRED_VARS_VAR_ACTION), set(test_str.split(_SEP)))

    def test_input_variables(self):
        test_str = datatable_report(_DATABASE, DatatableReport("input_variables", value="var_compatibility_suite"), _SEP)
        self.assertSetEqual(set(_INPUT_VARS_VAR_ACTION), set(test_str.split(_SEP)))

    def test_output_variables(self):
        test_str = datatable_report(_DATABASE, DatatableReport("output_variables", value="var_compatibility_suite"), _SEP)
        self.assertSetEqual(set(_OUTPUT_VARS_VAR_ACTION), set(test_str.split(_SEP)))


class CommandLineVarCompatibilitySuite(unittest.TestCase):
    def test_required_variables(self):
        completedProcess = subprocess.run([f"{_SCRIPTS_DIR}/ccpp_datafile.py", _DATABASE, "--required-variables", "var_compatibility_suite"],
                                          capture_output=True,
                                          text=True)
        actualOutput = {s.strip() for s in completedProcess.stdout.split(_SEP)}
        self.assertSetEqual(set(_REQUIRED_VARS_VAR_ACTION), actualOutput)

    def test_input_variables(self):
        completedProcess = subprocess.run([f"{_SCRIPTS_DIR}/ccpp_datafile.py", _DATABASE, "--input-variables", "var_compatibility_suite"],
                                          capture_output=True,
                                          text=True)
        actualOutput = {s.strip() for s in completedProcess.stdout.split(_SEP)}
        self.assertSetEqual(set(_INPUT_VARS_VAR_ACTION), actualOutput)

    def test_output_variables(self):
        completedProcess = subprocess.run([f"{_SCRIPTS_DIR}/ccpp_datafile.py", _DATABASE, "--output-variables", "var_compatibility_suite"],
                                          capture_output=True,
                                          text=True)
        actualOutput = {s.strip() for s in completedProcess.stdout.split(_SEP)}
        self.assertSetEqual(set(_OUTPUT_VARS_VAR_ACTION), actualOutput)

