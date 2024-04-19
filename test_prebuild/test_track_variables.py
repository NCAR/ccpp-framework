#! /usr/bin/env python3
"""
-----------------------------------------------------------------------
 Description:  Contains unit tests for ccpp_track_variables.py script

 Assumptions:  Assumes user has correct environment for running ccpp_track_variables.py script.
               This script should not be run directly, but rather invoked with pytest.

 Command line arguments: none

 Usage: pytest test_track_variables.py         # run the unit tests
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
    """Tests whether test_track_variables.py produces expected output from sample suite and
       metadata files for a case with a successful match (user provided a variable that exists
       within the schemes specified by the test suite)"""
    expected_output = """For suite test_track_variables/suite_small_suite.xml, the following schemes (in order for each group) use the variable air_pressure:
In group group1
  scheme_1_run (intent in)
  scheme_1_run (intent in)"""
    track_variables(SMALL_SUITE_FILE,SAMPLE_FILES_DIR,CONFIG_FILE,'air_pressure',False)
    streams = capsys.readouterr()
    expected_output_list = expected_output.splitlines()
    streams_err_list = streams.err.splitlines()
    for (err, expected) in zip(streams_err_list, expected_output_list):
        assert err.strip() == expected.strip()

def test_successful_match_with_subcycles(capsys):
    """Tests whether test_track_variables.py produces expected output from sample suite and
       metadata files for a case with a successful match (user provided a variable that exists
       within the schemes specified by the test suite). In this case, the test suite file
       contains subcycles, so the output should reflect this."""

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
    for (err, expected) in zip(streams_err_list, expected_output_list):
        assert err.strip() == expected.strip()


def test_partial_match(capsys):
    """Tests whether test_track_variables.py produces expected output from sample suite and
       metadata files for a case with a partial match: user provided a variable that does not
       exist in the test suite, but is a substring of one or more other variables that do
       exist."""

    expected_output = """Variable surface not found in any suites for sdf test_track_variables/suite_TEST_SUITE.xml

ERROR:ccpp_track_variables:Variable surface not found in any suites for sdf test_track_variables/suite_TEST_SUITE.xml

Did find partial matches that may be of interest:

In scheme_2_init found variable(s) ['surface_emissivity_data_file']
In scheme_2_run found variable(s) ['surface_roughness_length', 'surface_ground_temperature_for_radiation', 'surface_air_temperature_for_radiation', 'surface_skin_temperature_over_ice', 'baseline_surface_longwave_emissivity', 'surface_longwave_emissivity', 'surface_albedo_components', 'surface_albedo_for_diffused_shortwave_on_radiation_timestep']
In scheme_3_init found variable(s) ['flag_for_mellor_yamada_nakanishi_niino_surface_layer_scheme']
In scheme_3_timestep_init found variable(s) ['flag_for_mellor_yamada_nakanishi_niino_surface_layer_scheme']
In scheme_3_run found variable(s) ['do_compute_surface_scalar_fluxes', 'do_compute_surface_diagnostics', 'surface_air_pressure', 'reference_air_pressure_normalized_by_surface_air_pressure']
In scheme_3_timestep_finalize found variable(s) ['surface_air_pressure']
In scheme_4_run found variable(s) ['surface_air_pressure']
In scheme_B_run found variable(s) ['flag_nonzero_wet_surface_fraction', 'sea_surface_temperature', 'surface_skin_temperature_after_iteration_over_water']
"""
    track_variables(SUITE_FILE,SAMPLE_FILES_DIR,CONFIG_FILE,'surface',False)
    streams = capsys.readouterr()
    expected_output_list = expected_output.splitlines()
    streams_err_list = streams.err.splitlines()
    for (err, expected) in zip(streams_err_list, expected_output_list):
        assert err.strip() == expected.strip()


def test_no_match(capsys):
    """Tests whether test_track_variables.py produces expected output from sample suite and
       metadata files for a case with no match (user provided a variable that does not exist
       within the schemes specified by the test suite)"""

    expected_output = """Variable abc not found in any suites for sdf test_track_variables/suite_TEST_SUITE.xml

ERROR:ccpp_track_variables:Variable abc not found in any suites for sdf test_track_variables/suite_TEST_SUITE.xml"""
    track_variables(SUITE_FILE,SAMPLE_FILES_DIR,CONFIG_FILE,'abc',False)
    streams = capsys.readouterr()
    expected_output_list = expected_output.splitlines()
    streams_err_list = streams.err.splitlines()
    for (err, expected) in zip(streams_err_list, expected_output_list):
        assert err.strip() == expected.strip()


def test_bad_config(capsys):
    """Tests whether test_track_variables.py fails gracefully when provided a config file that does
       not exist."""
    with pytest.raises(Exception) as excinfo:
        track_variables(SUITE_FILE,SAMPLE_FILES_DIR,f'{SAMPLE_FILES_DIR}/nofile','abc',False)
    assert str(excinfo.value) == "Call to import_config failed."


if __name__ == "__main__":
    print("This test file is designed to be run with pytest; can not be run directly")
    sys.exit(1)

