#!/usr/bin/env python

# Standard modules
import argparse
import logging
import collections
#import importlib
#import itertools
import os
#import re
#import sys

# CCPP framework imports
#from common import encode_container, decode_container, decode_container_as_dict, execute
from metadata_parser import parse_scheme_tables, parse_variable_tables
from metadata_table import find_scheme_names
from ccpp_prebuild import collect_physics_subroutines, import_config, gather_variable_definitions
#from mkcap import CapsMakefile, CapsCMakefile, CapsSourcefile, \
#                  SchemesMakefile, SchemesCMakefile, SchemesSourcefile, \
#                  TypedefsMakefile, TypedefsCMakefile, TypedefsSourcefile
#from mkdoc import metadata_to_html, metadata_to_latex
from mkstatic import API, Suite, Group

###############################################################################
# Set up the command line argument parser and other global variables          #
###############################################################################

parser = argparse.ArgumentParser()
parser.add_argument('-s', '--sdf',           action='store', help='suite definition file to use', required=True)
parser.add_argument('-m', '--metadata_path', action='store', help='path to CCPP scheme metadata files (DEPRECATED FOR NOW)')
parser.add_argument('-c', '--config',        action='store', help='path to CCPP prebuild configuration file', required=True)
parser.add_argument('-v', '--variable',      action='store', help='remove files created by this script, then exit', required=True)
parser.add_argument('--debug',               action='store_true', help='enable debugging output', default=False)
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
    debug = args.debug
    return(success,sdf,var,configfile,debug)

def setup_logging(debug):
    """Sets up the logging module and logging level."""
    success = True
    if debug:
        level = logging.DEBUG
    else:
        level = logging.INFO
    logging.basicConfig(format='%(levelname)s: %(message)s', level=level)
    if debug:
        logging.info('Logging level set to DEBUG')
    else:
        logging.info('Logging level set to INFO')
    return success

def parse_suite(sdf):
    """Reads provided sdf, parses ordered list of schemes for the suite specified by said sdf"""
    print('reading sdf ' + sdf)
    suite = Suite(sdf_name=sdf)
    success = suite.parse()
    if not success:
        logging.error('Parsing suite definition file {0} failed.'.format(sdf))
        success = False
        return
    print('Successfully read sdf' + suite.sdf_name)
    print('reading list of schemes from suite ' + suite.name)
    print('creating calling tree of schemes')
    success = suite.make_call_tree()
    print(suite.call_tree)
    if not success:
        logging.error('Parsing suite definition file {0} failed.'.format(sdf))
        success = False
        return
    return (success, suite)

def create_var_graph(suite, config):
    """Given a suite, variable name, and a 'config' dictionary:
         1. Loops through the call tree of provided suite
         2. For each scheme, reads .meta file for said scheme, checks for variable within that scheme, and if it exists, adds an entry to an ordered dictionary with the name of the scheme and the intent of the variable"""

    success = True

    # Create an ordered dictionary that will hold the in/out information for each scheme
    var_graph=collections.OrderedDict()

    logging.debug("reading metadata files for schemes defined in config file:\n {0}".format(config['scheme_files']))

    (success, metadata_request, arguments_request, dependencies_request, schemes_in_files) = collect_physics_subroutines(config['scheme_files'])

    print("\n\n\n\n")
    print(metadata_request)
    print(arguments_request)
    print(dependencies_request)
    print("\n\n\n\n")

    print('reading .meta file for scheme [scheme]')
    # Loop through call tree, find matching filename for scheme via dictionary schemes_in_files, 
    # then parse that metadata file to find variable info
    for scheme in suite.call_tree:
        
        if scheme in schemes_in_files:
            print(scheme, '->', schemes_in_files[scheme])

#        logging.debug("reading metadata file {0} for scheme {1}".format(filename, scheme))

    print('found variable ' + args.variable + ' in [scheme], adding scheme to list [list]')
    return (success,var_graph) 

def check_var():
    """Check given variable against standard names"""
    # This function may ultimately end up being unnecessary
    success = True
    print('Checking if ' + args.variable + ' is in list of standard names')
    return success

def main():
    """Main routine that traverses a CCPP scheme and outputs the list of schemes that modify given variable"""

    (success, sdf, var, configfile, debug) = parse_arguments(args)
    if not success:
        raise Exception('Call to parse_arguments failed.')

    success = setup_logging(debug)
    if not success:
        raise Exception('Call to setup_logging failed.')

    success = check_var()
    if not success:
        raise Exception('Call to check_var failed.')

    (success, suite) = parse_suite(sdf)
    if not success:
        raise Exception('Call to parse_suite failed.')

    (success, config) = import_config(configfile, None)
    if not success:
        raise Exception('Call to import_config failed.')

    # Variables defined by the host model
    (success, metadata_define, dependencies_define) = gather_variable_definitions(config['variable_definition_files'], config['typedefs_new_metadata'])
    if not success:
        raise Exception('Call to gather_variable_definitions failed.')

    (success, var_graph) = create_var_graph(suite, config)
    if not success:
        raise Exception('Call to create_var_graph failed.')

    print('For suite [suite], the following schemes (in order) modify the variable ' + var)

if __name__ == '__main__':
    main()

