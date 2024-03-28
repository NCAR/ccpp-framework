#!/usr/bin/env python3

# Standard modules
import os
import argparse
import logging
import glob

# CCPP framework imports
from metadata_table import find_scheme_names, parse_metadata_file
from ccpp_prebuild import import_config, gather_variable_definitions
from mkstatic import Suite
from parse_checkers import registered_fortran_ddt_names
from parse_tools import init_log, set_log_level
from framework_env import CCPPFrameworkEnv

###############################################################################
# Set up the command line argument parser and other global variables          #
###############################################################################

###############################################################################
# Functions and subroutines                                                   #
###############################################################################

def parse_arguments():
    """Parse command line arguments."""
    parser = argparse.ArgumentParser()
    parser.add_argument('-s', '--sdf', help='suite definition file to parse', required=True)
    parser.add_argument('-m', '--metadata_path',
                        help='path to CCPP scheme metadata files', required=True)
    parser.add_argument('-c', '--config',
                        help='path to CCPP prebuild configuration file', required=True)
    parser.add_argument('-v', '--variable', help='variable to track through CCPP suite',
                        required=True)
    parser.add_argument('--debug', action='store_true', help='enable debugging output',
                        default=False)

    args = parser.parse_args()

    return args

def setup_logging(debug):
    """Sets up the logging module and logging level."""

    #Use capgen logging tools
    logger = init_log('ccpp_track_variables')

    if debug:
        set_log_level(logger, logging.DEBUG)
        logger.info('Logging level set to DEBUG')
    else:
        set_log_level(logger, logging.WARNING)
    return logger

def parse_suite(sdf, run_env):
    """Reads the provided sdf, parses into a Suite data structure, including the "call tree":
       the ordered list of schemes for the suite specified by the provided sdf"""
    run_env.logger.info(f'Reading sdf {sdf} and populating Suite object')
    suite = Suite(sdf_name=sdf)
    run_env.logger.info(f'Reading sdf {sdf} and populating Suite object')
    success = suite.parse(make_call_tree=True)
    if not success:
        raise Exception(f'Parsing suite definition file {sdf} failed.')
    run_env.logger.info(f'Successfully read sdf {suite.sdf_name}')
    return suite

def create_metadata_filename_dict(metapath):
    """Given a path, read all .meta files in that directory and add them to a dictionary: the keys
       are the name of the scheme, and the values are the filename of the .meta file associated
       with that scheme"""

    metadata_dict = {}
    scheme_filenames = glob.glob(os.path.join(metapath, "*.meta"))
    if not scheme_filenames:
        raise Exception(f'No files found in {metapath} with ".meta" extension')

    for scheme_fn in scheme_filenames:
        schemes = find_scheme_names(scheme_fn)
        # The above returns a list of schemes in each filename, but
        # we want a dictionary of schemes associated with filenames:
        for scheme in schemes:
            metadata_dict[scheme] = scheme_fn

    return metadata_dict


def create_var_graph(suite, var, config, metapath, run_env):
    """Given a suite, variable name, a 'config' dictionary, and a path to .meta files:
         1. Creates a dictionary associating schemes with their .meta files
         2. Loops through the call tree of the provided suite by group
         3. For each scheme, reads .meta file for said scheme, checks for variable within that
            scheme, and if it exists, adds an entry to a list of tuples for the corresponding
            group, where each tuple includes the name of the scheme and the intent of the variable
            within that scheme"""

    # Create a list of tuples for each group that will hold the in/out information for each scheme
    var_graph={}
    var_graph_empty = True

    run_env.logger.debug(f"reading .meta files in path:\n {metapath}")
    metadata_dict=create_metadata_filename_dict(metapath)

    run_env.logger.debug(f"reading metadata files for schemes defined in config file: "
                  f"{config['scheme_files']}")

    # Loop through call tree, find matching filename for scheme via dictionary schemes_in_files,
    # then parse that metadata file to find variable info
    partial_matches = {}
    for group in suite.call_tree:
        run_env.logger.debug(f"for group {group} ")
        # Create list of tuples that will hold the in/out information for each scheme in this group
        var_graph[group] = []
        for scheme in suite.call_tree[group]:
            run_env.logger.debug(f"reading meta file for scheme {scheme} ")

            if scheme in metadata_dict:
                scheme_filename = metadata_dict[scheme]
            else:
                raise Exception(f"Error, scheme '{scheme}' from suite '{suite.sdf_name}' "
                                f"not found in metadata files in {metapath}")

            run_env.logger.debug(f"reading metadata file {scheme_filename} for scheme {scheme}")

            new_metadata_headers = parse_metadata_file(scheme_filename,
                                                       known_ddts=registered_fortran_ddt_names(),
                                                       run_env=run_env)
            for scheme_metadata in new_metadata_headers:
                if scheme_metadata.table_name != scheme:
                    # Some metadata files contain information for multiple schemes,
                    # need to make sure we only read the relevant section
                    continue
                for section in scheme_metadata.sections():
                    found_var = []
                    intent = ''
                    for scheme_var in section.variable_list():
                        exact_match = False
                        if var == scheme_var.get_prop_value('standard_name'):
                            run_env.logger.debug(f"Found variable {var} in scheme {section.title}")
                            found_var=var
                            exact_match = True
                            intent = scheme_var.get_prop_value('intent')
                            break
                        scheme_var_standard_name = scheme_var.get_prop_value('standard_name')
                        if scheme_var_standard_name.find(var) != -1:
                            run_env.logger.debug(f"{var} matches {scheme_var_standard_name}")
                            found_var.append(scheme_var_standard_name)
                    if not found_var:
                        run_env.logger.debug(f"Did not find variable {var} in scheme {section.title}")
                    elif exact_match:
                        run_env.logger.debug(f"Exact match found for variable {var} in scheme "
                                             f"{section.title}, intent {intent}")
                        var_graph[group].append((section.title,intent))
                        var_graph_empty = False
                    else:
                        run_env.logger.debug(f"Found inexact matches for variable(s) {var} "
                                      f"in scheme {section.title}:\n{found_var}")
                        partial_matches[section.title] = found_var


    if not var_graph_empty:
        success = True
        run_env.logger.debug(f"Successfully generated variable graph for sdf {suite.sdf_name}\n")
    else:
        success = False
        run_env.logger.error(f"Variable {var} not found in any suites for sdf {suite.sdf_name}\n")
        if partial_matches:
            print("Did find partial matches that may be of interest:\n")
            for key in partial_matches:
                print(f"In {key} found variable(s) {partial_matches[key]}")

    return (success,var_graph)

def track_variables(sdf,metadata_path,config,variable,debug):
    """Main routine that traverses a CCPP suite and outputs the list of schemes that use given
       variable, broken down by group

    Args:
        sdf           (str) : The full path of the suite definition file to parse
        metadata_path (str) : path to CCPP scheme metadata files
        config        (str) : path to CCPP prebuild configuration file
        variable      (str) : variable to track through CCPP suite
        debug        (bool) : Enable extra output for debugging

    Returns:
        None
"""

    logger = setup_logging(debug)

    #Use new capgen class CCPPFrameworkEnv
    run_env = CCPPFrameworkEnv(logger, host_files="", scheme_files="", suites="")

    suite = parse_suite(sdf,run_env)

    (success, config) = import_config(config, None)
    if not success:
        raise Exception('Call to import_config failed.')

    # Variables defined by the host model; this call is necessary because it converts some old
    # metadata formats so they can be used later in the script
    (success, _, _) = gather_variable_definitions(config['variable_definition_files'],
                                                   config['typedefs_new_metadata'])
    if not success:
        raise Exception('Call to gather_variable_definitions failed.')

    (success, var_graph) = create_var_graph(suite, variable, config, metadata_path, run_env)
    if success:
        print(f"For suite {suite.sdf_name}, the following schemes (in order for each group) "
              f"use the variable {variable}:")
        for group in var_graph:
            if var_graph[group]:
                print(f"In group {group}")
                for entry in var_graph[group]:
                    print(f"  {entry[0]} (intent {entry[1]})")


if __name__ == '__main__':

    args = parse_arguments()

    track_variables(args.sdf,args.metadata_path,args.config,args.variable,args.debug)
