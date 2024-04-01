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

def test_successful_match_with_subcycles(capsys):
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


def test_partial_match(capsys):
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
    # Split into lines to ignore whitespace differences
    i = 0
    for line in streams_err_list:
        assert streams_err_list[i].strip() == expected_output_list[i].strip()
        i+=1


def test_no_match(capsys):
    expected_output = """Variable abc not found in any suites for sdf test_track_variables/suite_TEST_SUITE.xml

ERROR:ccpp_track_variables:Variable abc not found in any suites for sdf test_track_variables/suite_TEST_SUITE.xml"""
    track_variables(SUITE_FILE,SAMPLE_FILES_DIR,CONFIG_FILE,'abc',False)
    streams = capsys.readouterr()
    expected_output_list = expected_output.splitlines()
    streams_err_list = streams.err.splitlines()
    # Split into lines to ignore whitespace differences
    i = 0
    for line in streams_err_list:
        assert streams_err_list[i].strip() == expected_output_list[i].strip()
        i+=1


def test_bad_config(capsys):
    with pytest.raises(Exception) as excinfo:
        track_variables(SUITE_FILE,SAMPLE_FILES_DIR,f'{SAMPLE_FILES_DIR}/nofile','abc',False)
    assert str(excinfo.value) == "Call to import_config failed."


if __name__ == "__main__":
    print("This test file is designed to be run with pytest; can not be run directly")


