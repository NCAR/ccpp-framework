#!/usr/bin/env python

import argparse
import os
import sys

from metadata_table import MetadataHeader

parser = argparse.ArgumentParser()
parser.add_argument('--metafile', '-m', action='store', help='name of metadata file to convert', required=True)

attributes = [ 'standard_name', 'long_name', 'units', 'local_name', 'type', 'dimensions', 'kind', 'intent', 'optional' ]

def parse_arguments():
    """Parse command line arguments."""
    args = parser.parse_args()
    filename = args.metafile
    return filename

def convert_to_html(filename_in):
    """Convert a metadata file into html (one html file for each table)"""
    if not os.path.isfile(filename_in):
        raise Exception("Metadata file {} not found".format(filename_in))
    filename_out = filename_in.replace(".meta", ".html")
    metadata_headers = MetadataHeader.parse_metadata_file(filename_in)
    
    for metadata_header in metadata_headers:
        print metadata_header
        metadata_header.to_html(filename_out, attributes)

def main():
    filename = parse_arguments()
    print filename
    convert_to_html(filename)

if __name__ == '__main__':
    main()