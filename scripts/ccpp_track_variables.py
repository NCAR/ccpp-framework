#!/usr/bin/env python

# Standard modules
import argparse
#import importlib
#import itertools
#import os
#import re
#import sys

# CCPP framework imports
#from common import encode_container, decode_container, decode_container_as_dict, execute
#from metadata_parser import merge_dictionaries, parse_scheme_tables, parse_variable_tables
#from mkcap import CapsMakefile, CapsCMakefile, CapsSourcefile, \
#                  SchemesMakefile, SchemesCMakefile, SchemesSourcefile, \
#                  TypedefsMakefile, TypedefsCMakefile, TypedefsSourcefile
#from mkdoc import metadata_to_html, metadata_to_latex
#from mkstatic import API, Suite, Group

###############################################################################
# Set up the command line argument parser and other global variables          #
###############################################################################

parser = argparse.ArgumentParser()
parser.add_argument('-s', '--sdf',       action='store', help='suite definition file to use', required=True)
parser.add_argument('-v', '--variable',  action='store', help='remove files created by this script, then exit', required=True)
args = parser.parse_args()

###############################################################################
# Functions and subroutines                                                   #
###############################################################################

def parse_arguments(args):
    """Parse command line arguments."""
    success = True
    sdf = args.sdf
    var = args.variable
    return(success,sdf,var)

def parse_suite(sdf):
    """Reads provided sdf, parses ordered list of schemes for the suite specified by said sdf"""
    print('reading sdf ' + sdf + ' defining CCPP suite')
    print('reading list of schemes from suite [suite]')
    print('stored list of schemes in [list]')

def read_meta_file():
    """Given a scheme, variable name, and list, reads .meta file for said scheme, checks for variable within that scheme, and if it exists and is intent(out), appends to list"""
    print('reading .meta file for scheme [scheme]')
    print('found variable ' + args.variable + ' in [scheme], adding scheme to list [list]')

def check_var():
    """Check given variable against standard names"""
    print('Checking if ' + args.variable + ' is in list of standard names')

def main():
    """Main routine that traverses a CCPP scheme and outputs the list of schemes that modify given variable"""
    (success, sdf, var) = parse_arguments(args)
    check_var()
    parse_suite(sdf)
#    for scheme in schemes:
    read_meta_file()
    print('For suite [suite], the following schemes (in order) modify the variable ' + var)

if __name__ == '__main__':
    main()

