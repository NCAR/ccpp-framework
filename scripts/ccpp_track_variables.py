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
from ccpp_prebuild import collect_physics_subroutines
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
parser.add_argument('-m', '--metadata_path', action='store', help='path to CCPP scheme metadata files', required=True)
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
    metapath = args.metadata_path
    debug = args.debug
    return(success,sdf,var,metapath,debug)

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

def read_meta_files(suite, metapath):
    """Given a suite, variable name, and a directory containing scheme metadata files:
         1. Loops through the call tree of provided suite
         2. For each scheme, reads .meta file for said scheme, checks for variable within that scheme, and if it exists, adds an entry to an ordered dictionary with the name of the scheme and the intent of the variable"""

    success = True

    # Create an ordered dictionary that will hold the in/out information for each scheme
    var_graph=collections.OrderedDict()

    logging.debug('reading .meta files for schemes in {0}'.format(metapath))
    scheme_filenames=os.listdir(metapath)
    # The above line only gets us filenames, this line gets us a list with full paths
    print(scheme_filenames[0])
    scheme_filenames = [metapath + s for s in scheme_filenames]
    print(scheme_filenames[0])

    (success, metadata_request, arguments_request, dependencies_request, schemes_in_files) = collect_physics_subroutines(scheme_filenames)

    print(metadata_request)
    print(arguments_request)
    print(dependencies_request)
    print(schemes_in_files)

    print('reading .meta file for scheme [scheme]')
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

    (success, sdf, var, metapath, debug) = parse_arguments(args)
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

    (success, var_graph) = read_meta_files(suite, metapath)
    if not success:
        raise Exception('Call to read_meta_files failed.')

    print('For suite [suite], the following schemes (in order) modify the variable ' + var)

if __name__ == '__main__':
    main()

