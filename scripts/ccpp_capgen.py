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
from fortran_parser import parse_fortran_file

logger = logging.getLogger(__name__)

class CapgenAbort(ValueError):
    "Class so main can log user errors without backtrace"
    def __init__(self, message):
        super(CapgenAbort, self).__init__(message)

###############################################################################
def abort(message):
###############################################################################
    logger.error(message)
    raise CapgenAbort(message)

###############################################################################
def is_xml_file(filename):
###############################################################################
    parts = os.path.basename(filename).split('.')
    return (len(parts) > 1) and (parts[-1].lower() == 'xml')

###############################################################################
def check_for_existing_file(filename, description):
###############################################################################
    if not os.path.exists(filename):
        abort("{}, '{}', must exist".format(description, filename))
    # End if (else just return)

###############################################################################
def check_for_writeable_file(filename, description):
###############################################################################
    if os.path.exists(filename) and not os.access(filename, os.W_OK):
        abort("Cannot write {}, '{}'".format(description, filename))
    elif not os.access(os.path.dirname(filename), os.W_OK):
        abort("Cannot write {}, '{}'".format(description, filename))
    # End if (else just return)

###############################################################################
def parse_command_line(args, description):
###############################################################################
    parser = argparse.ArgumentParser(description=description,
                                     formatter_class=argparse.RawTextHelpFormatter)

    parser.add_argument("host_pathnames", metavar='<host files filename>',
                        type=str, help="File with host filenames to process")

    parser.add_argument("scheme_pathnames", metavar='<scheme files filename>',
                        type=str, help="File with scheme filenames to process")

    parser.add_argument("--sdf-pathlist", metavar='<sdf file(s)>',
                        type=str, default='',
                        help="File with names of suite definition fileames to process or name of single XML SDF file")

    parser.add_argument("--host-model-def", metavar='<host model def filename>',
                        type=str,
                        help="XML filename with host model definition")

    parser.add_argument("--preproc-directives",
                        metavar='VARDEF1[,VARDEF2 ...]', type=str, default=None,
                        help="Proprocessor directives used to correctly parse source files")

    parser.add_argument("--cap-pathlist", type=str,
                        metavar='<filename for list of cap filenames>',
                        default="capfiles.txt",
                        help="Filename for list of generated cap files")

    parser.add_argument("--host-pathlist", type=str,
                        metavar='<filename for generated host filenames>',
                        default="hostfiles.txt",
                        help="Filename for list of generated host files")

    parser.add_argument("--output-root", type=str,
                        metavar='<directory for generated files>',
                        default=os.getcwd(),
                        help="directory for generated files")

    parser.add_argument("--generate-docfiles",
                        metavar='HTML | Latex | HTML,Latex', type=str,
                        help="Generate LaTeX and/or HTML documentation")

    parser.add_argument("--verbose", action='count',
                        help="Log more activity, repeat for increased output")
    pargs = parser.parse_args(args)
    return pargs

###############################################################################
def parse_host_model_files(host_pathsfile, preproc_defs):
###############################################################################
    """
    Gather information from host files (e.g., DDTs, registry) and
    return resulting dictionary.
    """
    host_pdir = os.path.dirname(host_pathsfile)
    mheaders = list()
    xml_files = list()
    with open(host_pathsfile, 'r') as infile:
        filenames = [x.strip() for x in infile.readlines()]
    # End with
    for filename in filenames:
        if len(filename) == 0:
            # Allow for blank lines
            continue
        # End if
        if not os.path.isabs(filename):
            # The filename should be relative to host_pathsfile
            filename = os.path.join(host_pdir, filename)
        # End if
        if is_xml_file(filename):
            # We have to process XML files after processing Fortran files
            # since the Fortran files may define DDTs used by registry files.
            xml_files.append(filename)
        else:
            hheaders = parse_fortran_file(filename, preproc_defs==preproc_defs)
            mheaders.extend(hheaders)
        # End if
    # End for
    return mheaders

###############################################################################
def _main_func():
###############################################################################
    logger.addHandler(logging.StreamHandler())
    args = parse_command_line(sys.argv[1:], __doc__)
    verbosity = args.verbose
    host_pathsfile = os.path.abspath(args.host_pathnames)
    schemes_pathsfile = os.path.abspath(args.scheme_pathnames)
    if len(args.sdf_pathlist) > 0:
        sdf_pathsfile = os.path.abspath(args.sdf_pathlist)
    else:
        sdf_pathsfile = None
    # End if
    cap_output_file = os.path.abspath(args.cap_pathlist)
    host_output_file = os.path.abspath(args.host_pathlist)
    output_dir = os.path.abspath(args.output_root)
    preproc_defs = args.preproc_directives
    gen_docfiles = args.generate_docfiles
    ## A few sanity checks
    # Check required arguments
    check_for_existing_file(host_pathsfile, 'Host pathnames file')
    check_for_existing_file(schemes_pathsfile, 'Schemes pathnames file')
    check_for_existing_file(schemes_pathsfile, 'SDF pathnames or file')
    ## Make sure output directory is legit
    if os.path.exists(output_dir):
        if not os.path.isdir(output_dir):
            abort("output-root, '{}', is not a directory".format(args.output_root))
        elif not os.access(output_dir, os.W_OK):
            abort("Cannot write files to output-root ({})".format(args.output_root))
        # End if (output_dir is okay)
    else:
        # Try to create output_dir (let it crash if it fails)
        os.makedirs(output_dir)
    # End if
    # Make sure we can create output file lists
    if not os.path.isabs(cap_output_file):
        cap_output_file = os.path.join(output_dir, cap_output_file)
    # End if
    check_for_writeable_file(cap_output_file, "Cap output file")
    if not os.path.isabs(host_output_file):
        host_output_file = os.path.join(output_dir, host_output_file)
    # End if
    check_for_writeable_file(host_output_file, "Host output file")
    # Did we get an SDF input?
    if sdf_pathsfile is not None:
        sdf_is_xml = is_xml_file(sdf_pathsfile)
        if sdf_is_xml:
            check_for_existing_file(sdf_pathsfile, 'SDF file')
        else:
            check_for_existing_file(sdf_pathsfile, 'SDF pathnames file')
        # End if
    # End if
    ##XXgoldyXX: Temporary warning
    if gen_docfiles:
        abort("--gen-docfiles not yet supported")
    # End if
    # First up, handle the host files
    mheaders = parse_host_model_files(host_pathsfile, preproc_defs)

###############################################################################

if __name__ == "__main__":
    try:
        _main_func()
    except CapgenAbort as ca:
        pass # abort already logged the message
