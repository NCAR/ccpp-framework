#! /usr/bin/env python
#-----------------------------------------------------------------------
# Description:  Test capgen database report python interface
#
# Assumptions:
#
# Command line arguments: build_dir database_filepath
#
# Usage: python test_reports <build_dir> <database_filepath>
#-----------------------------------------------------------------------
import sys
import os
import logging
import unittest

test_dir = os.path.dirname(os.path.abspath(__file__))
scripts_dir = os.path.abspath(os.path.join(test_dir, os.pardir, os.pardir, "scripts"))

if not os.path.exists(scripts_dir):
    raise ImportError("Cannot find scripts directory")
# end if

sys.path.append(scripts_dir)
from ccpp_datafile import datatable_report, DatatableReport

def usage(errmsg=None):
    """Raise an exception with optional error message and usage message"""
    emsg = "usage: {} <build_dir> <database_filepath>"
    if errmsg:
        emsg = errmsg + '\n' + emsg
    # end if
    raise ValueError(emsg.format(sys.argv[0]))

if len(sys.argv) != 3:
    usage()
# end if

_BUILD_DIR = os.path.abspath(sys.argv[1])
_DATABASE = os.path.abspath(sys.argv[2])
if not os.path.isdir(_BUILD_DIR):
    emsg = "<build_dir> must be an existing build directory"
    usage(emsg)
# end if
if (not os.path.exists(_DATABASE)) or (not os.path.isfile(_DATABASE)):
    emsg = "<database_filepath> must be an existing CCPP database file"
    usage(emsg)
# end if

# Check data
_HOST_FILES = [os.path.join(_BUILD_DIR, "ccpp", "test_host_ccpp_cap.F90")]
_SUITE_FILES = [os.path.join(_BUILD_DIR, "ccpp", "ccpp_ddt_suite_cap.F90"),
                os.path.join(_BUILD_DIR, "ccpp", "ccpp_temp_suite_cap.F90")]
_UTILITY_FILES = [os.path.join(_BUILD_DIR, "ccpp", "ccpp_kinds.F90")]
_CCPP_FILES = [os.path.join(_BUILD_DIR, "ccpp", "ccpp_kinds.F90"),
               os.path.join(_BUILD_DIR, "ccpp", "test_host_ccpp_cap.F90"),
               os.path.join(_BUILD_DIR, "ccpp", "ccpp_ddt_suite_cap.F90"),
               os.path.join(_BUILD_DIR, "ccpp", "ccpp_temp_suite_cap.F90")]
_PROCESS_LIST = ["setter=temp_set", "adjusting=temp_calc_adjust"]
_MODULE_LIST = ["environ_conditions", "make_ddt", "temp_adjust",
                "temp_calc_adjust", "temp_set"]
_SUITE_LIST = ["ddt_suite", "temp_suite"]
_REQUIRED_VARS_DDT = ["ccpp_error_flag", "ccpp_error_message",
                      "horizontal_dimension", "horizontal_loop_begin",
                      "horizontal_loop_end", "model_times",
                      "number_of_model_times", "surface_air_pressure"]
_INPUT_VARS_DDT = ["horizontal_dimension", "horizontal_loop_begin",
                   "horizontal_loop_end", "model_times",
                   "number_of_model_times", "surface_air_pressure"]
_OUTPUT_VARS_DDT = ["ccpp_error_flag", "ccpp_error_message", "model_times",
                    "number_of_model_times"]
_REQUIRED_VARS_TEMP = ["ccpp_error_flag", "ccpp_error_message",
                       "horizontal_dimension", "horizontal_loop_begin",
                       "horizontal_loop_end", "potential_temperature",
                       "potential_temperature_at_interface",
                       "potential_temperature_increment",
                       "surface_air_pressure", "time_step_for_physics",
                       "vertical_interface_dimension",
                       "vertical_layer_dimension",
                       "water_vapor_specific_humidity"]
_INPUT_VARS_TEMP = ["horizontal_dimension", "horizontal_loop_begin",
                    "horizontal_loop_end", "potential_temperature",
                    "potential_temperature_at_interface",
                    "potential_temperature_increment",
                    "surface_air_pressure", "time_step_for_physics",
                    "vertical_interface_dimension",
                    "vertical_layer_dimension", "water_vapor_specific_humidity"]
_OUTPUT_VARS_TEMP = ["ccpp_error_flag", "ccpp_error_message",
                     "potential_temperature",
                     "potential_temperature_at_interface",
                     "surface_air_pressure", "water_vapor_specific_humidity"]

def check_datatable(database, report_type, check_list, sep=None):
    """Run a database report and check the return string"""
    print("Checking {} report".format(report_type.action))
    if sep is None:
        sep = ','
    # end if
    test_str = datatable_report(database, report_type, sep)
    check_str = sep.join(check_list)
    if test_str != check_str:
        vmsg = "datafile check:\nExpected: {}'\nGot: '{}'"
        raise ValueError(vmsg.format(check_str, test_str))
    # end if

check_datatable(_DATABASE, DatatableReport("host_files"), _HOST_FILES)
check_datatable(_DATABASE, DatatableReport("suite_files"), _SUITE_FILES)
check_datatable(_DATABASE, DatatableReport("utility_files"), _UTILITY_FILES)
check_datatable(_DATABASE, DatatableReport("ccpp_files"), _CCPP_FILES)
check_datatable(_DATABASE, DatatableReport("process_list"), _PROCESS_LIST)
check_datatable(_DATABASE, DatatableReport("module_list"), _MODULE_LIST)
check_datatable(_DATABASE, DatatableReport("suite_list"), _SUITE_LIST)
check_datatable(_DATABASE, DatatableReport("required_variables",
                                           value="ddt_suite"),
                _REQUIRED_VARS_DDT)
check_datatable(_DATABASE, DatatableReport("input_variables",
                                           value="ddt_suite"),
                _INPUT_VARS_DDT)
check_datatable(_DATABASE, DatatableReport("output_variables",
                                           value="ddt_suite"),
                _OUTPUT_VARS_DDT)
check_datatable(_DATABASE, DatatableReport("required_variables",
                                           value="temp_suite"),
                _REQUIRED_VARS_TEMP)
check_datatable(_DATABASE, DatatableReport("input_variables",
                                           value="temp_suite"),
                _INPUT_VARS_TEMP)
check_datatable(_DATABASE, DatatableReport("output_variables",
                                           value="temp_suite"),
                _OUTPUT_VARS_TEMP)
