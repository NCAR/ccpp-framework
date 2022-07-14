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

_TEST_DIR = os.path.dirname(os.path.abspath(__file__))
_FRAMEWORK_DIR = os.path.abspath(os.path.join(_TEST_DIR, os.pardir, os.pardir))
_SCRIPTS_DIR = os.path.abspath(os.path.join(_FRAMEWORK_DIR, "scripts"))

if not os.path.exists(_SCRIPTS_DIR):
    raise ImportError("Cannot find scripts directory")
# end if

if ((sys.version_info[0] < 3) or
    (sys.version_info[0] == 3) and (sys.version_info[1] < 6)):
    raise Exception("Python 3.6 or greater required")
# end if

sys.path.append(_SCRIPTS_DIR)
# pylint: disable=wrong-import-position
from ccpp_datafile import datatable_report, DatatableReport
# pylint: enable=wrong-import-position

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
    _EMSG = "<build_dir> must be an existing build directory"
    usage(_EMSG)
# end if
if (not os.path.exists(_DATABASE)) or (not os.path.isfile(_DATABASE)):
    _EMSG = "<database_filepath> must be an existing CCPP database file"
    usage(_EMSG)
# end if

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
_MODULE_LIST = ["cld_ice", "cld_liq"]
_SUITE_LIST = ["cld_suite"]
_REQUIRED_VARS_CLD = ["ccpp_error_code", "ccpp_error_message",
                      "horizontal_loop_begin", "horizontal_loop_end",
                      "surface_air_pressure", "temperature",
                      "time_step_for_physics", "water_temperature_at_freezing",
                      "water_vapor_specific_humidity",
                      "cloud_ice_dry_mixing_ratio",
                      "cloud_liquid_dry_mixing_ratio"]
_INPUT_VARS_CLD = ["surface_air_pressure", "temperature",
                   "horizontal_loop_begin", "horizontal_loop_end",
                   "time_step_for_physics", "water_temperature_at_freezing",
                   "water_vapor_specific_humidity",
                   "cloud_ice_dry_mixing_ratio",
                   "cloud_liquid_dry_mixing_ratio"]
_OUTPUT_VARS_CLD = ["ccpp_error_code", "ccpp_error_message",
                    "water_vapor_specific_humidity", "temperature",
                    "cloud_ice_dry_mixing_ratio",
                    "cloud_liquid_dry_mixing_ratio"]

def fields_string(field_type, field_list, sep):
    """Create an error string for <field_type> field(s), <field_list>.
    <sep> is used to separate items in <field_list>"""
    indent = ' '*11
    if field_list:
        if len(field_list) > 1:
            field_str = "{} Fields: ".format(field_type)
        else:
            field_str = "{} Field: ".format(field_type)
        # end if
        fmsg = "\n{}{}{}".format(indent, field_str, sep.join(field_list))
    else:
        fmsg = ""
    # end if
    return fmsg

def check_datatable(database, report_type, check_list, sep=','):
    """Run a database report and check the return string.
    If an error is found, print an error message.
    Return the number of errors"""
    if sep is None:
        sep = ','
    # end if
    test_str = datatable_report(database, report_type, sep)
    test_list = [x for x in test_str.split(sep) if x]
    missing = list()
    unexpected = list()
    for item in check_list:
        if item not in test_list:
            missing.append(item)
        # end if
    # end for
    for item in test_list:
        if item not in check_list:
            unexpected.append(item)
        # end if
    # end for
    if missing or unexpected:
        vmsg = "ERROR in {} datafile check:".format(report_type.action)
        vmsg += fields_string("Missing", missing, sep)
        vmsg += fields_string("Unexpected", unexpected, sep)
        print(vmsg)
    else:
        print("{} report okay".format(report_type.action))
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
print("\nChecking variables for CLD suite from python")
NUM_ERRORS += check_datatable(_DATABASE, DatatableReport("required_variables",
                                                         value="cld_suite"),
                              _REQUIRED_VARS_CLD)
NUM_ERRORS += check_datatable(_DATABASE, DatatableReport("input_variables",
                                                         value="cld_suite"),
                              _INPUT_VARS_CLD)
NUM_ERRORS += check_datatable(_DATABASE, DatatableReport("output_variables",
                                                         value="cld_suite"),
                              _OUTPUT_VARS_CLD)

sys.exit(NUM_ERRORS)
