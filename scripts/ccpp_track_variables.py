#!/usr/bin/env python

# Standard modules
import argparse
import logging
import collections
import glob

# CCPP framework imports
from metadata_table import find_scheme_names, parse_metadata_file
from ccpp_prebuild import import_config, gather_variable_definitions
from mkstatic import Suite
from parse_checkers import registered_fortran_ddt_names

###############################################################################
# Set up the command line argument parser and other global variables          #
###############################################################################

parser = argparse.ArgumentParser()
parser.add_argument('-s', '--sdf',           action='store', \
                    help='suite definition file to parse', required=True)
parser.add_argument('-m', '--metadata_path', action='store', \
                    help='path to CCPP scheme metadata files', required=True)
parser.add_argument('-c', '--config',        action='store', \
                    help='path to CCPP prebuild configuration file', required=True)
parser.add_argument('-v', '--variable',      action='store', \
                    help='variable to track through CCPP suite', required=True)
#parser.add_argument('--draw',  action='store_true', \
#                    help='draw graph of calling tree for given variable', default=False)
parser.add_argument('--debug', action='store_true', help='enable debugging output', default=False)
args = parser.parse_args()

###############################################################################
# Functions and subroutines                                                   #
###############################################################################

def parse_arguments(args):
    """Parse command line arguments."""
    success = True
    sdf = args.sdf
    var = args.variable
    configfile = args.config
    metapath = args.metadata_path
    debug = args.debug
    return(success,sdf,var,configfile,metapath,debug)

def setup_logging(debug):
    """Sets up the logging module and logging level."""
    success = True
    if debug:
        level = logging.DEBUG
    else:
        level = logging.WARNING
    logging.basicConfig(format='%(levelname)s: %(message)s', level=level)
    if debug:
        logging.info('Logging level set to DEBUG')
    return success

def parse_suite(sdf):
    """Reads the provided sdf, parses into a Suite data structure, including the "call tree": 
       the ordered list of schemes for the suite specified by the provided sdf"""
    logging.info(f'Reading sdf {sdf} and populating Suite object')
    suite = Suite(sdf_name=sdf)
    success = suite.parse()
    if not success:
        logging.error(f'Parsing suite definition file {sdf} failed.')
        success = False
        return (success, suite)
    logging.info(f'Successfully read sdf {suite.sdf_name}')
    logging.info(f'Creating calling tree of schemes for suite {suite.name}')
    success = suite.make_call_tree()
    if not success:
        logging.error('Parsing suite definition file {0} failed.'.format(sdf))
        success = False
    return (success, suite)

def create_metadata_filename_dict(metapath):
    """Given a path, read all .meta files in that directory and add them to a dictionary: the keys
       are the name of the scheme, and the values are the filename of the .meta file associated
       with that scheme"""

    success = True
    metadata_dict = {}
    scheme_filenames=glob.glob(metapath + "*.meta")
    if not scheme_filenames:
        logging.error(f'No files found in {metapath} with ".meta" extension')
        success = False

    for scheme_fn in scheme_filenames:
        schemes=find_scheme_names(scheme_fn)
        # The above returns a list of schemes in each filename, but
        # we want a dictionary of schemes associated with filenames:
        for scheme in schemes:
            metadata_dict[scheme]=scheme_fn

    return (metadata_dict, success)


def create_var_graph(suite, var, config, metapath):
    """Given a suite, variable name, a 'config' dictionary, and a path to .meta files:
         1. Creates a dictionary associating schemes with their .meta files
         2. Loops through the call tree of the provided suite
         3. For each scheme, reads .meta file for said scheme, checks for variable within that
            scheme, and if it exists, adds an entry to a list of tuples, where each tuple includes
            the name of the scheme and the intent of the variable within that scheme"""

    # Create a list of tuples that will hold the in/out information for each scheme
    var_graph=[]

    logging.debug("reading .meta files in path:\n {0}".format(metapath))
    (metadata_dict, success)=create_metadata_filename_dict(metapath)
    if not success:
        raise Exception('Call to create_metadata_filename_dict failed')

    logging.debug(f"reading metadata files for schemes defined in config file: "
                  f"{config['scheme_files']}")

    # Loop through call tree, find matching filename for scheme via dictionary schemes_in_files,
    # then parse that metadata file to find variable info
    partial_matches = {}
    for scheme in suite.call_tree:
        logging.debug("reading meta file for scheme {0} ".format(scheme))

        if scheme in metadata_dict:
            scheme_filename = metadata_dict[scheme]
        else:
            raise Exception(f"Error, scheme '{scheme}' from suite '{suite.sdf_name}' "
                            f"not found in metadata files in {metapath}")

        logging.debug("reading metadata file {0} for scheme {1}".format(scheme_filename, scheme))

        new_metadata_headers = parse_metadata_file(scheme_filename, \
                                                   known_ddts=registered_fortran_ddt_names(), \
                                                   logger=logging.getLogger(__name__))
        for scheme_metadata in new_metadata_headers:
            for section in scheme_metadata.sections():
                found_var = []
                intent = ''
                for scheme_var in section.variable_list():
                    exact_match = False
                    if var == scheme_var.get_prop_value('standard_name'):
                        logging.debug("Found variable {0} in scheme {1}".format(var,section.title))
                        found_var=var
                        exact_match = True
                        intent = scheme_var.get_prop_value('intent')
                        break
                    scheme_var_standard_name = scheme_var.get_prop_value('standard_name')
                    if scheme_var_standard_name.find(var) != -1:
                        logging.debug(f"{var} matches {scheme_var_standard_name}")
                        found_var.append(scheme_var_standard_name)
                if not found_var:
                    logging.debug(f"Did not find variable {var} in scheme {section.title}")
                elif exact_match:
                    logging.debug(f"Exact match found for variable {var} in scheme {section.title},"
                                  f" intent {intent}")
                    #print(f"{var_graph=}")
                    var_graph.append((section.title,intent))
                else:
                    logging.debug(f"Found inexact matches for variable(s) {var} "
                                  f"in scheme {section.title}:\n{found_var}")
                    partial_matches[section.title] = found_var
    if var_graph:
        success = True
        logging.debug("Successfully generated variable graph for sdf {0}\n".format(suite.sdf_name))
    else:
        success = False
        logging.error(f"Variable {var} not found in any suites for sdf {suite.sdf_name}\n")
        if partial_matches:
            print("Did find partial matches that may be of interest:\n")
            for key in partial_matches:
                print("In {0} found variable(s) {1}".format(key, partial_matches[key]))

    return (success,var_graph)

def draw_var_graph(var_graph):
    """Draw a graphical representation of the variable graph (not yet implemented)"""

    success = True

    return success

    return

def main():
    """Main routine that traverses a CCPP suite and outputs the list of schemes that modify given variable"""

    (success, sdf, var, configfile, metapath, debug) = parse_arguments(args)
    if not success:
        raise Exception('Call to parse_arguments failed.')

    success = setup_logging(debug)
    if not success:
        raise Exception('Call to setup_logging failed.')

    (success, suite) = parse_suite(sdf)
    if not success:
        raise Exception('Call to parse_suite failed.')

    (success, config) = import_config(configfile, None)
    if not success:
        raise Exception('Call to import_config failed.')

    # Variables defined by the host model
    (success, _, _) = gather_variable_definitions(config['variable_definition_files'], config['typedefs_new_metadata'])
    if not success:
        raise Exception('Call to gather_variable_definitions failed.')

    (success, var_graph) = create_var_graph(suite, var, config, metapath)
    if success:
        print(f"For suite {suite.sdf_name}, the following schemes (in order) "
              f"modify the variable {var}:")
        for entry in var_graph:
            print(f"{entry[0]} (intent {entry[1]})")


if __name__ == '__main__':
    main()
