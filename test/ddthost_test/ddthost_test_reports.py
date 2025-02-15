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

from test_stub import BaseTests

_BUILD_DIR = os.path.join(os.path.abspath(os.environ['BUILD_DIR']), "test", "ddthost_test")

_TEST_DIR = os.path.dirname(os.path.abspath(__file__))
_FRAMEWORK_DIR = os.path.abspath(os.path.join(_TEST_DIR, os.pardir, os.pardir))
_SCRIPTS_DIR = os.path.join(_FRAMEWORK_DIR, "scripts")
_SRC_DIR = os.path.join(_FRAMEWORK_DIR, "src")

# sys.path.append(_SCRIPTS_DIR)
# pylint: disable=wrong-import-position
from ccpp_datafile import datatable_report, DatatableReport
# pylint: enable=wrong-import-position

_DATABASE = os.path.abspath(os.path.join(_BUILD_DIR, "ccpp", "datatable.xml"))

# Check data
_HOST_FILES = [os.path.join(_BUILD_DIR, "ccpp", "test_host_ccpp_cap.F90")]
_SUITE_FILES = [os.path.join(_BUILD_DIR, "ccpp", "ccpp_ddt_suite_cap.F90"),
                os.path.join(_BUILD_DIR, "ccpp", "ccpp_temp_suite_cap.F90")]
_UTILITY_FILES = [os.path.join(_BUILD_DIR, "ccpp", "ccpp_kinds.F90"),
                  os.path.join(_SRC_DIR, "ccpp_constituent_prop_mod.F90"),
                  os.path.join(_SRC_DIR, "ccpp_hashable.F90"),
                  os.path.join(_SRC_DIR, "ccpp_hash_table.F90")]
_CCPP_FILES = _UTILITY_FILES + \
              [os.path.join(_BUILD_DIR, "ccpp", "test_host_ccpp_cap.F90"),
               os.path.join(_BUILD_DIR, "ccpp", "ccpp_ddt_suite_cap.F90"),
               os.path.join(_BUILD_DIR, "ccpp", "ccpp_temp_suite_cap.F90")]
_DEPENDENCIES = [os.path.join(_TEST_DIR, "adjust", "qux.F90"),
                 os.path.join(_TEST_DIR, "bar.F90"),
                 os.path.join(_TEST_DIR, "foo.F90")]
_PROCESS_LIST = ["setter=temp_set", "adjusting=temp_calc_adjust"]
_MODULE_LIST = ["environ_conditions", "make_ddt", "setup_coeffs", "temp_adjust",
                "temp_calc_adjust", "temp_set"]
_SUITE_LIST = ["ddt_suite", "temp_suite"]
_INPUT_VARS_DDT = ["model_times", "number_of_model_times",
                   "horizontal_loop_begin", "horizontal_loop_end",
                   "surface_air_pressure", "horizontal_dimension"]
_OUTPUT_VARS_DDT = ["ccpp_error_code", "ccpp_error_message", "model_times",
                    "number_of_model_times", "surface_air_pressure"]
_REQUIRED_VARS_DDT = _INPUT_VARS_DDT + _OUTPUT_VARS_DDT
_PROT_VARS_TEMP = ["horizontal_loop_begin", "horizontal_loop_end",
                   "horizontal_dimension", "vertical_layer_dimension",
                   "number_of_tracers",
                   # Added for --debug
                   "index_of_water_vapor_specific_humidity",
                   "vertical_interface_dimension"]
_REQUIRED_VARS_TEMP = ["ccpp_error_code", "ccpp_error_message",
                       "potential_temperature",
                       "potential_temperature_at_interface",
                       "coefficients_for_interpolation",
                       "potential_temperature_increment",
                       "surface_air_pressure", "time_step_for_physics",
                       "water_vapor_specific_humidity"]
_INPUT_VARS_TEMP = ["potential_temperature",
                    "potential_temperature_at_interface",
                    "coefficients_for_interpolation",
                    "potential_temperature_increment",
                    "surface_air_pressure", "time_step_for_physics",
                    "water_vapor_specific_humidity"]
_OUTPUT_VARS_TEMP = ["ccpp_error_code", "ccpp_error_message",
                     "potential_temperature",
                     "potential_temperature_at_interface",
                     "coefficients_for_interpolation",
                     "surface_air_pressure", "water_vapor_specific_humidity"]
_SEP = ","

class TestDdtHostDataTables(unittest.TestCase, BaseTests.TestHostDataTables):
    database = _DATABASE
    host_files = _HOST_FILES
    suite_files = _SUITE_FILES
    utility_files = _UTILITY_FILES
    ccpp_files = _CCPP_FILES
    process_list = _PROCESS_LIST
    module_list = _MODULE_LIST
    dependencies = _DEPENDENCIES
    suite_list = _SUITE_LIST


class CommandLineDdtHostDatafileRequiredFiles(unittest.TestCase, BaseTests.TestHostCommandLineDataFiles):
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


class TestDdtSuite(unittest.TestCase):
    def test_required_variables(self):
        test_str = datatable_report(_DATABASE, DatatableReport("required_variables", value="ddt_suite"), _SEP)
        self.assertSetEqual(set(_REQUIRED_VARS_DDT), set(test_str.split(_SEP)))

    def test_input_variables(self):
        test_str = datatable_report(_DATABASE, DatatableReport("input_variables", value="ddt_suite"), _SEP)
        self.assertSetEqual(set(_INPUT_VARS_DDT), set(test_str.split(_SEP)))

    def test_output_variables(self):
        test_str = datatable_report(_DATABASE, DatatableReport("output_variables", value="ddt_suite"), _SEP)
        self.assertSetEqual(set(_OUTPUT_VARS_DDT), set(test_str.split(_SEP)))


class CommandLineDddtSuite(unittest.TestCase):
    def test_required_variables(self):
        completedProcess = subprocess.run([f"{_SCRIPTS_DIR}/ccpp_datafile.py", _DATABASE, "--required-variables", "ddt_suite"],
                                          capture_output=True,
                                          text=True)
        actualOutput = {s.strip() for s in completedProcess.stdout.split(_SEP)}
        self.assertSetEqual(set(_REQUIRED_VARS_DDT), actualOutput)

    def test_input_variables(self):
        completedProcess = subprocess.run([f"{_SCRIPTS_DIR}/ccpp_datafile.py", _DATABASE, "--input-variables", "ddt_suite"],
                                          capture_output=True,
                                          text=True)
        actualOutput = {s.strip() for s in completedProcess.stdout.split(_SEP)}
        self.assertSetEqual(set(_INPUT_VARS_DDT), actualOutput)

    def test_output_variables(self):
        completedProcess = subprocess.run([f"{_SCRIPTS_DIR}/ccpp_datafile.py", _DATABASE, "--output-variables", "ddt_suite"],
                                          capture_output=True,
                                          text=True)
        self.assertEqual(_SEP.join(_OUTPUT_VARS_DDT), completedProcess.stdout.strip())


class TestTempSuite(unittest.TestCase):
    def test_required_variables(self):
        test_str = datatable_report(_DATABASE, DatatableReport("required_variables", value="temp_suite"), _SEP)
        self.assertSetEqual(set(_REQUIRED_VARS_TEMP + _PROT_VARS_TEMP), set(test_str.split(_SEP)))

    def test_required_variables_excluding_protected(self):
        test_str = datatable_report(_DATABASE, DatatableReport("required_variables", value="temp_suite"), _SEP, exclude_protected=True)
        self.assertSetEqual(set(_REQUIRED_VARS_TEMP), set(test_str.split(_SEP)))

    def test_input_variables(self):
        test_str = datatable_report(_DATABASE, DatatableReport("input_variables", value="temp_suite"), _SEP)
        self.assertSetEqual(set(_INPUT_VARS_TEMP + _PROT_VARS_TEMP), set(test_str.split(_SEP)))

    def test_input_variables_excluding_protected(self):
        test_str = datatable_report(_DATABASE, DatatableReport("input_variables", value="temp_suite"), _SEP, exclude_protected=True)
        self.assertSetEqual(set(_INPUT_VARS_TEMP), set(test_str.split(_SEP)))

    def test_output_variables(self):
        test_str = datatable_report(_DATABASE, DatatableReport("output_variables", value="temp_suite"), _SEP)
        self.assertSetEqual(set(_OUTPUT_VARS_TEMP), set(test_str.split(_SEP)))


class CommandLineTempSuite(unittest.TestCase):
    def test_required_variables(self):
        completedProcess = subprocess.run([f"{_SCRIPTS_DIR}/ccpp_datafile.py", _DATABASE, "--required-variables", "temp_suite"],
                                          capture_output=True,
                                          text=True)
        actualOutput = {s.strip() for s in completedProcess.stdout.split(_SEP)}
        self.assertSetEqual(set(_REQUIRED_VARS_TEMP + _PROT_VARS_TEMP), actualOutput)

    def test_input_variables(self):
        completedProcess = subprocess.run([f"{_SCRIPTS_DIR}/ccpp_datafile.py", _DATABASE, "--input-variables", "temp_suite"],
                                          capture_output=True,
                                          text=True)
        actualOutput = {s.strip() for s in completedProcess.stdout.split(_SEP)}
        self.assertSetEqual(set(_INPUT_VARS_TEMP + _PROT_VARS_TEMP), actualOutput)

    def test_output_variables(self):
        completedProcess = subprocess.run([f"{_SCRIPTS_DIR}/ccpp_datafile.py", _DATABASE, "--output-variables", "temp_suite"],
                                          capture_output=True,
                                          text=True)
        actualOutput = {s.strip() for s in completedProcess.stdout.split(_SEP)}
        self.assertSetEqual(set(_OUTPUT_VARS_TEMP), actualOutput)
