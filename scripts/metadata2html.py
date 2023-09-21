#!/usr/bin/env python3

import argparse
import logging
import importlib
import os
import sys

# CCPP framework imports
from common import CCPP_INTERNAL_VARIABLE_DEFINITON_FILE
from parse_checkers import registered_fortran_ddt_names
from parse_tools import init_log, set_log_level
from metadata_table import MetadataTable, parse_metadata_file
from framework_env import CCPPFrameworkEnv

###############################################################################
# Set up the command line argument parser and other global variables          #
###############################################################################

parser = argparse.ArgumentParser()
method = parser.add_mutually_exclusive_group(required=True)
method.add_argument('--config', '-c', action='store',
                    help='path to CCPP prebuild configuration file')
method.add_argument('--metafile', '-m', action='store',
                    help='name of metadata file to convert (requires -o)')
parser.add_argument('--outputdir', '-o', action='store',
                    help='directory where to write the html files',
                    required='--metafile' in sys.argv or '-m' in sys.argv)

# List and order of variable attributes to output to HTML
ATTRIBUTES = [ 'local_name', 'standard_name', 'long_name', 'units',
               'type', 'dimensions', 'kind', 'intent' ]

###############################################################################
# Functions and subroutines                                                   #
###############################################################################

def parse_arguments():
    """Parse command line arguments."""
    args = parser.parse_args()
    config = args.config
    filename = args.metafile
    outdir = args.outputdir
    return (config, filename, outdir)

def import_config(configfile, logger):
    """Import the configuration from a given configuration file"""

    if not os.path.isfile(configfile):
        raise Exception("Configuration file {0} not found".format(configfile))

    # Import the host-model specific CCPP prebuild config;
    # split into path and module name for import
    configpath = os.path.abspath(os.path.dirname(configfile))
    configmodule = os.path.splitext(os.path.basename(configfile))[0]
    sys.path.append(configpath)
    ccpp_prebuild_config = importlib.import_module(configmodule)

    # Get the base directory for running metadata2html.py from
    # the default build directory value in the CCPP prebuild config
    basedir = os.path.join(os.getcwd())
    logger.info('Relative path to CCPP directory from  CCPP prebuild config: {}'.format(
                                                ccpp_prebuild_config.DEFAULT_BUILD_DIR))

    config = {}
    # Definitions in host-model dependent CCPP prebuild config script
    config['variable_definition_files'] = ccpp_prebuild_config.VARIABLE_DEFINITION_FILES
    config['scheme_files']              = ccpp_prebuild_config.SCHEME_FILES
    # Add model-independent, CCPP-internal variable definition files
    config['variable_definition_files'].append(CCPP_INTERNAL_VARIABLE_DEFINITON_FILE)
    # Output directory for converted metadata tables
    config['metadata_html_output_dir'] = ccpp_prebuild_config.METADATA_HTML_OUTPUT_DIR.format(build_dir=basedir)

    return config

def get_metadata_files_from_config(config, logger):
    """Create a list of metadata filenames for a CCPP prebuild configuration"""
    filenames = []
    for sourcefile in config['variable_definition_files'] + config['scheme_files']:
        metafile = os.path.splitext(sourcefile)[0]+'.meta'
        if os.path.isfile(metafile):
            filenames.append(metafile)
        else:
            # DH* Warn for now, raise exception later when
            # old metadata format is no longer supported
            logger.warn("Metadata file {} for source file {} not found, assuming old metadata format".format(
                                                                                       metafile, sourcefile))
    return filenames

def get_output_directory_from_config(config, logger):
    """Return the html output directory for a CCPP prebuild configuration"""
    outdir = config['metadata_html_output_dir']
    if not os.path.isdir(outdir):
        raise Exception("Output directory {} for converted metadata tables does not exist".format(outdir))
    return outdir

def convert_to_html(filename_in, outdir, logger, run_env):
    """Convert a metadata file into html (one html file for each table)"""
    if not os.path.isfile(filename_in):
        raise Exception("Metadata file {} not found".format(filename_in))
    logger.info("Converting file {} to HTML".format(filename_in))
    metadata_headers = parse_metadata_file(filename_in,
                                           known_ddts=registered_fortran_ddt_names(),
                                           run_env=run_env)
    for metadata_header in metadata_headers:
        for metadata_section in metadata_header.sections():
            filename_out = metadata_section.to_html(outdir, ATTRIBUTES)
            if filename_out:
                logger.info("  ... wrote {}".format(filename_out))

def main():
    # Initialize logging
    logger = init_log('metadata2html')
    set_log_level(logger, logging.INFO)
    run_env = CCPPFrameworkEnv(logger, ndict={'host_files':'',
                                              'scheme_files':'',
                                              'suites':''})

    # Convert metadata file
    (configfile, filename, outdir) = parse_arguments()
    if configfile:
        config = import_config(configfile, logger)
        filenames = get_metadata_files_from_config(config, logger)
        outdir = get_output_directory_from_config(config, logger)
        for filename in filenames:
            convert_to_html(filename, outdir, logger, run_env)
    else:
        convert_to_html(filename, outdir, logger, run_env)

if __name__ == '__main__':
    main()
