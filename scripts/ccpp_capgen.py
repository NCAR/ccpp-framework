#!/usr/bin/env python

"""
Create CCPP parameterization caps, host-model interface code, optional
physics suite runtime code, and documentation.
"""

import argparse
import sys
import os
import os.path
import logging

logger = logging.getLogger(__name__)

###############################################################################
def check_for_existing_file(filename, description):
###############################################################################
    if not os.path.exists(filename):
        logger.error("{}, '{}', must exist".format(description, filename))

###############################################################################
def parse_command_line(args, description):
###############################################################################
    parser = argparse.ArgumentParser(description=description,
                                     formatter_class=argparse.RawTextHelpFormatter)

    parser.add_argument("host_pathnames", metavar='<host files filename>',
                        type=str, help="File with host filenames to process")

    parser.add_argument("scheme_pathnames", metavar='<scheme files filename>',
                        type=str, help="File with scheme filenames to process")

    parser.add_argument("--sdf-pathlist", metavar='<sdf files filename>',
                        type=str, help="File with names of suite definition fileames to process")

    parser.add_argument("--host-model-def", metavar='<host model def filename>',
                        type=str,
                        help="XML filename with host model definition")

    parser.add_argument("--preproc-directives",
                        metavar='VARDEF1[,VARDEF2 ...]', type=str,
                        help="Proprocessor directives used to correctly parse source files")

    parser.add_argument("--cap-pathlist", type=str,
                        metavar='<filename for list of cap filenames>',
                        default=os.path.join(os.getcwd(), "capfiles.txt"),
                        help="Filename for list of generated cap files")

    parser.add_argument("--host-pathlist", type=str,
                        metavar='<filename for generated host filenames>',
                        default=os.path.join(os.getcwd(), "hostfiles.txt"),
                        help="Filename for list of generated host files")

    parser.add_argument("--generate-docfiles",
                        metavar='HTML | Latex | HTML,Latex', type=str,
                        help="Generate LaTeX and/or HTML documentation")

    parser.add_argument("--verbose", action='count',
                        help="Log more activity, repeat for increased output")
    pargs = parser.parse_args(args)
    return pargs

###############################################################################
def _main_func():
    logger.addHandler(logging.StreamHandler())
    args = parse_command_line(sys.argv[1:], __doc__)
    check_for_existing_file(args.host_pathnames, 'Host pathnames file')
    check_for_existing_file(args.scheme_pathnames, 'Schemes pathnames file')

###############################################################################

if __name__ == "__main__":
    _main_func()
