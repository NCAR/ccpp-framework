#!/usr/bin/env python

"""
Create CCPP parameterization caps, host-model interface code, optional
physics suite runtime code, and documentation.
"""

# Python library imports
import argparse
import sys
import os
import os.path
import logging
# CCPP framework imports
from fortran_tools import parse_fortran_file
from metavar import VarDictionary
from host_registry import HostModel, parse_host_registry
from host_cap import write_host_cap
from ccpp_suite import API, Suite
from parse_tools import initLog, setLogToStdout, setLogLevel, CCPPError

###############################################################################
def is_xml_file(filename):
###############################################################################
    parts = os.path.basename(filename).split('.')
    return (len(parts) > 1) and (parts[-1].lower() == 'xml')

###############################################################################
def check_for_existing_file(filename, description, readable=True):
###############################################################################
    'Check for file existence and access, abort on error'
    if os.path.exists(filename):
        if readable:
            if not os.access(filename, os.R_OK):
                raise CCPPError("No read access to {}, '{}'".format(description, filename))
            # End if
        # End if (no else needed, checks all done
    else:
        raise CCPPError("{}, '{}', must exist".format(description, filename))
    # End if

###############################################################################
def check_for_writeable_file(filename, description):
###############################################################################
    if os.path.exists(filename) and not os.access(filename, os.W_OK):
        raise CCPPError("Cannot write {}, '{}'".format(description, filename))
    elif not os.access(os.path.dirname(filename), os.W_OK):
        raise CCPPError("Cannot write {}, '{}'".format(description, filename))
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

    parser.add_argument("--output-root", type=str,
                        metavar='<directory for generated files>',
                        default=os.getcwd(),
                        help="directory for generated files")

    parser.add_argument("--generate-host-cap",
                        action='store_true', default=False,
                        help="Generate a host cap with correct API calling sequence")

    parser.add_argument("--generate-docfiles",
                        metavar='HTML | Latex | HTML,Latex', type=str,
                        help="Generate LaTeX and/or HTML documentation")

    parser.add_argument("--verbose", action='count',
                        help="Log more activity, repeat for increased output")
    pargs = parser.parse_args(args)
    return pargs

###############################################################################
def read_pathnames_from_file(pathsfile):
###############################################################################
    'Read pathnames from <pathsfile> and return them as a list'
    pathnames = list()
    with open(pathsfile, 'r') as infile:
        for line in infile.readlines():
            path = line.strip()
            # Skip blank lines and lines which appear to start with a comment.
            if (len(path) > 0) and (path[0] != '#') and (path[0] != '!'):
                # Check for an absolute path
                if not os.path.isabs(path):
                    # Assume relative pathnames are relative to pathsfile
                    path = os.path.join(os.path.dirname(pathsfile), path)
                # End if
                pdesc = 'pathname in {}'.format(pathsfile)
                check_for_existing_file(path, pdesc)
                pathnames.append(path)
            # End if (else skip blank or comment line)
        # End for
    # End with
    return pathnames

###############################################################################
def parse_host_model_files(host_pathsfile, preproc_defs, logger):
###############################################################################
    """
    Gather information from host files (e.g., DDTs, registry) and
    return a host model object with the information.
    """
    mheaders = list()
    xml_files = list()
    host_variables = {}
    variables = VarDictionary(logger=logger)
    filenames = read_pathnames_from_file(host_pathsfile)
    for filename in filenames:
        if is_xml_file(filename):
            # We have to process XML files after processing Fortran files
            # since the Fortran files may define DDTs used by registry files.
            xml_files.append(filename)
        else:
            hheaders = parse_fortran_file(filename, preproc_defs==preproc_defs, logger=logger)
            mheaders.extend(hheaders)
        # End if
    # End for
    name = None
    for file in xml_files:
        hname, vars, host_vars = parse_host_registry(file, logger)
        if (name is not None) and (hname != name):
            raise CCPPError('Inconsistent host model names, {} and {}'.format(hname, name))
        else:
            name = hname
        # End if
        variables.merge(vars)
        for var in host_vars.keys():
            host_variables[var] = host_vars[var]
            logger.info("{} defined in {}".format(var, host_vars[var]))
        # End for
    # End for
    return HostModel(name, mheaders, host_variables, variables)

###############################################################################
def parse_scheme_files(scheme_pathsfile, preproc_defs):
###############################################################################
    """
    Gather information from scheme files (e.g., init, run, and finalize
    methods) and return resulting dictionary.
    """
    mheaders = list()
    filenames = read_pathnames_from_file(scheme_pathsfile)
    for filename in filenames:
        sheaders = parse_fortran_file(filename, preproc_defs==preproc_defs)
        mheaders.append(sheaders)
    # End for
    return mheaders

###############################################################################
def _main_func():
###############################################################################
    logger = initLog('ccpp_capgen')
    args = parse_command_line(sys.argv[1:], __doc__)
    verbosity = args.verbose
    if verbosity > 1:
        setLogLevel(logger, logging.DEBUG)
    if verbosity > 0:
        setLogLevel(logger, logging.INFO)
    # End if
    host_pathsfile = os.path.abspath(args.host_pathnames)
    schemes_pathsfile = os.path.abspath(args.scheme_pathnames)
    if len(args.sdf_pathlist) > 0:
        sdf_pathsfile = os.path.abspath(args.sdf_pathlist)
    else:
        sdf_pathsfile = None
    # End if
    cap_output_file = os.path.abspath(args.cap_pathlist)
    output_dir = os.path.abspath(args.output_root)
    preproc_defs = args.preproc_directives
    gen_hostcap = args.generate_host_cap
    gen_docfiles = args.generate_docfiles
    ## A few sanity checks
    # Check required arguments
    check_for_existing_file(host_pathsfile, 'Host pathnames file')
    check_for_existing_file(schemes_pathsfile, 'Schemes pathnames file')
    check_for_existing_file(schemes_pathsfile, 'SDF pathnames or file')
    ## Make sure output directory is legit
    if os.path.exists(output_dir):
        if not os.path.isdir(output_dir):
            raise CCPPError("output-root, '{}', is not a directory".format(args.output_root))
        elif not os.access(output_dir, os.W_OK):
            raise CCPPError("Cannot write files to output-root ({})".format(args.output_root))
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
        raise CCPPError("--gen-docfiles not yet supported")
    # End if
    # First up, handle the host files
    host_model = parse_host_model_files(host_pathsfile, preproc_defs, logger)
    # Next, parse the scheme files
    scheme_headers = parse_scheme_files(schemes_pathsfile, preproc_defs)
    # Last, we need a list of SDF file(s)
    if sdf_pathsfile is None:
        sdfs = list()
    else:
        if sdf_is_xml:
            sdfs = [sdf_pathsfile]
        else:
            sdfs = sdf_pathsfile
        # End if
    # End if
# XXgoldyXX: v debug only
    print("headers = {}".format([x._table_title for x in host_model._ddt_defs]))
    print("variables = {}".format([x.get_prop_value('local_name') for x in host_model._variables.variable_list()]))
    print("schemes = {}".format([[x._table_title for x in y] for y in scheme_headers]))
# XXgoldyXX: ^ debug only
    # Finally, we can get on with writing suites
    ccpp_api = API(sdfs, host_model, scheme_headers, logger)
    cap_filenames = ccpp_api.write(output_dir)
    if gen_hostcap:
        # Create a cap file
        hcap_filename = write_host_cap(host_model, output_dir, logger)
    else:
        hcap_filename = None
    # End if
    if not os.path.isabs(cap_output_file):
        cap_output_file = os.path.join(output_dir, cap_output_file)
    # End if
    with open(cap_output_file, 'w') as cap_names:
        for path in cap_filenames:
            cap_names.write('{}\n'.format(path))
        # End for
        if hcap_filename is not None:
            cap_names.write('{}\n'.format(hcap_filename))
        # End if
    # End with

###############################################################################

if __name__ == "__main__":
    try:
        _main_func()
    except CCPPError as ca:
        logger.abort(ca)
