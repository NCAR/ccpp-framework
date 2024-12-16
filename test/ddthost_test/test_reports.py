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

_TEST_DIR = os.path.dirname(os.path.abspath(__file__))
_FRAMEWORK_DIR = os.path.abspath(os.path.join(_TEST_DIR, os.pardir, os.pardir))
_SCRIPTS_DIR = os.path.join(_FRAMEWORK_DIR, "scripts")
_SRC_DIR = os.path.join(_FRAMEWORK_DIR, "src")

if not os.path.exists(_SCRIPTS_DIR):
    raise ImportError("Cannot find scripts directory")
# end if

sys.path.append(_SCRIPTS_DIR)
# pylint: disable=wrong-import-position
from ccpp_datafile import datatable_report, DatatableReport
# pylint: enable=wrong-import-position

import argparse

parser = argparse.ArgumentParser(description="Test capgen database report python interface")
parser.add_argument('build_dir')
parser.add_argument('database_filepath')
if len(sys.argv) > 3:
    parser.error("Too many arguments")
# end if
args = parser.parse_args()
_BUILD_DIR = os.path.abspath(args.build_dir)
_DATABASE = os.path.abspath(args.database_filepath)
if not os.path.isdir(_BUILD_DIR):
    parser.error("<build_dir> must be an existing build directory")
# end if
if (not os.path.exists(_DATABASE)) or (not os.path.isfile(_DATABASE)):
    parser.error("<database_filepath> must be an existing CCPP database file")
# end if

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

def fields_string(field_type, field_list, sep):
    """Create an error string for <field_type> field(s), <field_list>.
    <sep> is used to separate items in <field_list>"""
    indent = ' '*11
    fmsg = ""
    if field_list:
        if len(field_list) > 1:
            field_str = f"{field_type} Fields: "
        else:
            field_str = f"{field_type} Field: "
        # end if
        fmsg = f"\n{indent}{field_str}{sep.join(sorted(field_list))}"
    # end if
    return fmsg

def check_datatable(database, report_type, check_list,
                    sep=',', exclude_protected=False):
    """Run a database report and check the return string.
    If an error is found, print an error message.
    Return the number of errors"""
    if sep is None:
        sep = ','
    # end if
    test_str = datatable_report(database, report_type, sep, exclude_protected=exclude_protected)
    test_list = [x for x in test_str.split(sep) if x]
    tests_run = set(test_list)
    expected_tests = set(check_list)
    missing = expected_tests - tests_run
    unexpected = tests_run - expected_tests
    if missing or unexpected:
        vmsg = f"ERROR in {report_type.action} datafile check:"
        vmsg += fields_string("Missing", missing, sep)
        vmsg += fields_string("Unexpected", unexpected, sep)
        print(vmsg)
    else:
        print(f"{report_type.action} report okay")
    # end if
    return len(missing) + len(unexpected)

NUM_ERRORS = 0
print("Checking required files from python:")
NUM_ERRORS += check_datatable(_DATABASE, DatatableReport("host_files"),
                              _HOST_FILES)
NUM_ERRORS += check_datatable(_DATABASE, DatatableReport("suite_files"),
                              _SUITE_FILES)
NUM_ERRORS += check_datatable(_DATABASE, DatatableReport("utility_files"),
                              _UTILITY_FILES)
NUM_ERRORS += check_datatable(_DATABASE, DatatableReport("ccpp_files"),
                              _CCPP_FILES)
print("\nChecking lists from python")
NUM_ERRORS += check_datatable(_DATABASE, DatatableReport("process_list"),
                              _PROCESS_LIST)
NUM_ERRORS += check_datatable(_DATABASE, DatatableReport("module_list"),
                              _MODULE_LIST)
NUM_ERRORS += check_datatable(_DATABASE, DatatableReport("suite_list"),
                              _SUITE_LIST)
print("\nChecking variables for DDT suite from python")
NUM_ERRORS += check_datatable(_DATABASE, DatatableReport("required_variables",
                                                         value="ddt_suite"),
                              _REQUIRED_VARS_DDT)
NUM_ERRORS += check_datatable(_DATABASE, DatatableReport("input_variables",
                                                         value="ddt_suite"),
                              _INPUT_VARS_DDT)
NUM_ERRORS += check_datatable(_DATABASE, DatatableReport("output_variables",
                                                         value="ddt_suite"),
                              _OUTPUT_VARS_DDT)
print("\nChecking variables for temp suite from python")
NUM_ERRORS += check_datatable(_DATABASE, DatatableReport("required_variables",
                                                         value="temp_suite"),
                              _REQUIRED_VARS_TEMP + _PROT_VARS_TEMP)
NUM_ERRORS += check_datatable(_DATABASE, DatatableReport("required_variables",
                                                         value="temp_suite"),
                              _REQUIRED_VARS_TEMP, exclude_protected=True)
NUM_ERRORS += check_datatable(_DATABASE, DatatableReport("input_variables",
                                                         value="temp_suite"),
                              _INPUT_VARS_TEMP + _PROT_VARS_TEMP)
NUM_ERRORS += check_datatable(_DATABASE, DatatableReport("input_variables",
                                                         value="temp_suite"),
                              _INPUT_VARS_TEMP, exclude_protected=True)
NUM_ERRORS += check_datatable(_DATABASE, DatatableReport("output_variables",
                                                         value="temp_suite"),
                              _OUTPUT_VARS_TEMP)

sys.exit(NUM_ERRORS)
