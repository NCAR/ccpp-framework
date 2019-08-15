#!/usr/bin/env python

import argparse
import logging
import os
import sys

# CCPP framework imports
from parse_tools import init_log, set_log_level
from metadata_table import MetadataHeader

parser = argparse.ArgumentParser()
parser.add_argument('--metafile', '-m', action='store',
                    help='name of metadata file to convert',
                    required=True)

attributes = [ 'standard_name', 'long_name', 'units', 'local_name',
               'type', 'dimensions', 'kind', 'intent', 'optional' ]

def parse_arguments():
    """Parse command line arguments."""
    args = parser.parse_args()
    filename = args.metafile
    return filename

def convert_to_html(filename_in, logger):
    """Convert a metadata file into html (one html file for each table)"""
    if not os.path.isfile(filename_in):
        raise Exception("Metadata file {} not found".format(filename_in))
    logger.info("Converting file {} to HTML".format(filename_in))
    metadata_headers = MetadataHeader.parse_metadata_file(filename_in)
    for metadata_header in metadata_headers:
        filename_out = metadata_header.to_html(attributes)
        if filename_out:
            logger.info("  ... wrote {}".format(filename_out))

def main():
    # Initialize logging
    logger = init_log('metadata2html')
    set_log_level(logger, logging.INFO)
    # Convert metadata file
    filename = parse_arguments()
    convert_to_html(filename, logger)

if __name__ == '__main__':
    main()
