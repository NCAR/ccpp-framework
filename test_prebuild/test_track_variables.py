#! /usr/bin/env python3
"""
-----------------------------------------------------------------------
 Description:  Contains unit tests for ccpp_track_variables.py script

 Assumptions:

 Command line arguments: none

 Usage: python test_var_transforms.py         # run the unit tests
-----------------------------------------------------------------------
"""
import sys
import os
import pytest

TEST_DIR = os.path.dirname(os.path.abspath(__file__))
SCRIPTS_DIR = os.path.abspath(os.path.join(TEST_DIR, os.pardir, "scripts"))
SAMPLE_FILES_DIR = "test_track_variables"
SUITE_FILE = f'{SAMPLE_FILES_DIR}/suite_TEST_SUITE.xml'
SMALL_SUITE_FILE = f'{SAMPLE_FILES_DIR}/suite_small_suite.xml'
CONFIG_FILE = f'{SAMPLE_FILES_DIR}/ccpp_prebuild_config.py'
if not os.path.exists(SCRIPTS_DIR):
    raise ImportError(f"Cannot find scripts directory {SCRIPTS_DIR}")

sys.path.append(SCRIPTS_DIR)

from ccpp_track_variables import track_variables

def test_successful_match(capsys):
    expected_output = """For suite test_track_variables/suite_small_suite.xml, the following schemes (in order for each group) use the variable air_pressure:
In group group1
  scheme_1_run (intent in)
  scheme_1_run (intent in)"""
    track_variables(SMALL_SUITE_FILE,SAMPLE_FILES_DIR,CONFIG_FILE,'air_pressure',False)
    streams = capsys.readouterr()
    expected_output_list = expected_output.splitlines()
    streams_err_list = streams.err.splitlines()
    # Split into lines to ignore whitespace differences
    i = 0
    for line in streams_err_list:
        assert streams_err_list[i].strip() == expected_output_list[i].strip()
        i+=1

    expected_output = """For suite test_track_variables/suite_TEST_SUITE.xml, the following schemes (in order for each group) use the variable surface_air_pressure:
In group group1
  scheme_3_run (intent inout)
  scheme_3_timestep_finalize (intent inout)
  scheme_3_timestep_finalize (intent out)
  scheme_4_run (intent in)
  scheme_3_run (intent inout)
  scheme_3_timestep_finalize (intent inout)
  scheme_3_timestep_finalize (intent out)
  scheme_4_run (intent in)
In group group2
  scheme_4_run (intent in)
  scheme_4_run (intent in)
  scheme_4_run (intent in)"""
    track_variables(SUITE_FILE,SAMPLE_FILES_DIR,CONFIG_FILE,'surface_air_pressure',False)
    streams = capsys.readouterr()
    expected_output_list = expected_output.splitlines()
    streams_err_list = streams.err.splitlines()
    # Split into lines to ignore whitespace differences
    i = 0
    for line in streams_err_list:
        assert streams_err_list[i].strip() == expected_output_list[i].strip()
        i+=1


#
#    def test_successful_match_with_subcycles(self):
#
#    def test_partial_match(self):
#
#    def test_no_match(self):
#
#    def test_bad_config(self):
#
#    def test_bad_suite(self):
#
def test_debug(capsys,caplog):
    """Test output with debug flag enabled"""

    expected_output_match = """Logging level set to DEBUG
Reading sdf test_track_variables/suite_small_suite.xml and populating Suite object
Successfully read sdf test_track_variables/suite_small_suite.xml
reading .meta files in path:
 test_track_variables
DEBUG:ccpp_track_variables:reading .meta files in path:
 test_track_variables
for group group1 
DEBUG:ccpp_track_variables:for group group1 
reading meta file for scheme scheme_1 
DEBUG:ccpp_track_variables:reading meta file for scheme scheme_1 
reading metadata file test_track_variables/scheme_1.meta for scheme scheme_1
DEBUG:ccpp_track_variables:reading metadata file test_track_variables/scheme_1.meta for scheme scheme_1
Parsing scheme scheme_1_run, at test_track_variables/scheme_1.meta:7
INFO:ccpp_track_variables:Parsing scheme scheme_1_run, at test_track_variables/scheme_1.meta:7
Adding p_lay to scheme_1_run
DEBUG:ccpp_track_variables:Adding p_lay to scheme_1_run
Adding p_lev to scheme_1_run
DEBUG:ccpp_track_variables:Adding p_lev to scheme_1_run
Found variable air_pressure in scheme scheme_1_run
DEBUG:ccpp_track_variables:Found variable air_pressure in scheme scheme_1_run
Exact match found for variable air_pressure in scheme scheme_1_run, intent in
DEBUG:ccpp_track_variables:Exact match found for variable air_pressure in scheme scheme_1_run, intent in
for group group2 
DEBUG:ccpp_track_variables:for group group2 
reading meta file for scheme scheme_4 
DEBUG:ccpp_track_variables:reading meta file for scheme scheme_4 
reading metadata file test_track_variables/scheme_4.meta for scheme scheme_4
DEBUG:ccpp_track_variables:reading metadata file test_track_variables/scheme_4.meta for scheme scheme_4
Parsing scheme scheme_4_run, at test_track_variables/scheme_4.meta:6
INFO:ccpp_track_variables:Parsing scheme scheme_4_run, at test_track_variables/scheme_4.meta:6
Adding ps to scheme_4_run
DEBUG:ccpp_track_variables:Adding ps to scheme_4_run
Adding errmsg to scheme_4_run
DEBUG:ccpp_track_variables:Adding errmsg to scheme_4_run
Adding errflg to scheme_4_run
DEBUG:ccpp_track_variables:Adding errflg to scheme_4_run
air_pressure matches surface_air_pressure
DEBUG:ccpp_track_variables:air_pressure matches surface_air_pressure
Found inexact matches for variable(s) air_pressure in scheme scheme_4_run:
['surface_air_pressure']
DEBUG:ccpp_track_variables:Found inexact matches for variable(s) air_pressure in scheme scheme_4_run:
['surface_air_pressure']
Successfully generated variable graph for sdf test_track_variables/suite_small_suite.xml

DEBUG:ccpp_track_variables:Successfully generated variable graph for sdf test_track_variables/suite_small_suite.xml

For suite test_track_variables/suite_small_suite.xml, the following schemes (in order for each group) use the variable air_pressure:
In group group1
  scheme_1_run (intent in)"""

    expected_output_nomatch = """Logging level set to DEBUG
Reading sdf test_track_variables/suite_small_suite.xml and populating Suite object
Successfully read sdf test_track_variables/suite_small_suite.xml
reading .meta files in path:
 test_track_variables
DEBUG:ccpp_track_variables:reading .meta files in path:
 test_track_variables
for group group1 
DEBUG:ccpp_track_variables:for group group1 
reading meta file for scheme scheme_1 
DEBUG:ccpp_track_variables:reading meta file for scheme scheme_1 
reading metadata file test_track_variables/scheme_1.meta for scheme scheme_1
DEBUG:ccpp_track_variables:reading metadata file test_track_variables/scheme_1.meta for scheme scheme_1
Parsing scheme scheme_1_run, at test_track_variables/scheme_1.meta:7
INFO:ccpp_track_variables:Parsing scheme scheme_1_run, at test_track_variables/scheme_1.meta:7
Adding p_lay to scheme_1_run
DEBUG:ccpp_track_variables:Adding p_lay to scheme_1_run
Adding p_lev to scheme_1_run
DEBUG:ccpp_track_variables:Adding p_lev to scheme_1_run
Did not find variable abc in scheme scheme_1_run
DEBUG:ccpp_track_variables:Did not find variable abc in scheme scheme_1_run
for group group2 
DEBUG:ccpp_track_variables:for group group2 
reading meta file for scheme scheme_4 
DEBUG:ccpp_track_variables:reading meta file for scheme scheme_4 
reading metadata file test_track_variables/scheme_4.meta for scheme scheme_4
DEBUG:ccpp_track_variables:reading metadata file test_track_variables/scheme_4.meta for scheme scheme_4
Parsing scheme scheme_4_run, at test_track_variables/scheme_4.meta:6
INFO:ccpp_track_variables:Parsing scheme scheme_4_run, at test_track_variables/scheme_4.meta:6
Adding ps to scheme_4_run
DEBUG:ccpp_track_variables:Adding ps to scheme_4_run
Adding errmsg to scheme_4_run
DEBUG:ccpp_track_variables:Adding errmsg to scheme_4_run
Adding errflg to scheme_4_run
DEBUG:ccpp_track_variables:Adding errflg to scheme_4_run
Did not find variable abc in scheme scheme_4_run
DEBUG:ccpp_track_variables:Did not find variable abc in scheme scheme_4_run
Variable abc not found in any suites for sdf test_track_variables/suite_small_suite.xml

ERROR:ccpp_track_variables:Variable abc not found in any suites for sdf test_track_variables/suite_small_suite.xml"""

    track_variables(SMALL_SUITE_FILE,SAMPLE_FILES_DIR,CONFIG_FILE,'abc',True)
    streams = capsys.readouterr()
    caplogmsg = caplog.text.splitlines()
    expected_output_list = expected_output_nomatch.splitlines()
    streams_err_list = streams.err.splitlines()
    i = 0
    print("\n")
    for line in streams_err_list:
#        print(i)
#        print(streams_err_list[i].strip())
#        print(expected_output_list[i].strip())
#        print(caplogmsg[i])
        assert streams_err_list[i].strip() == expected_output_list[i].strip()
        i+=1

    track_variables(SMALL_SUITE_FILE,SAMPLE_FILES_DIR,CONFIG_FILE,'air_pressure',True)
    streams = capsys.readouterr()
    expected_output_list = expected_output_match.splitlines()
    streams_err_list = streams.err.splitlines()
    i = 0
    for line in streams_err_list:
        assert streams_err_list[i].strip() == expected_output_list[i].strip()
        i+=1


if __name__ == "__main__":
    print("This test file is designed to be run with pytest; can not be run directly")


