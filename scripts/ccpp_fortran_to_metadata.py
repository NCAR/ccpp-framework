#!/usr/bin/env python3

#pylint: disable=anomalous-backslash-in-string
"""
Create prototype CCPP metadata tables from Fortran files

Parses annotated Fortran files to produce metadata files where the
standard_name, units, and dimension standard names must be filled in.
The annotation is a two line comment for every physics scheme, derived
data type (DDT) definition, or host model data section.
The annotation form is:

!> \section arg_table_<name>  Argument Table
!! \htmlinclude arg_table_<name>.html

where <name> is the name of the scheme, the name of the DDT, or the
name of the module containing data to be included in the metadata file.
For a scheme, the annotation must appear just before the subroutine statement.
For a DDT definition, the annotation must appear just before the type statement.
For module data, the annotation should occur after any module variables
  which should not be included in the metadata file.
Note that only CCPP interfaces (e.g., <name>_run, <name>_init, <name>_final)
  will be documented in this manner. All other routines should be left as is.
"""
#pylint: enable=anomalous-backslash-in-string

# Python library imports
from __future__ import absolute_import
from __future__ import unicode_literals
from __future__ import print_function

import argparse
import sys
import os
import os.path
import logging
# CCPP framework imports
from framework_env import CCPPFrameworkEnv
from parse_tools import init_log, set_log_level
from parse_tools import CCPPError, ParseInternalError
from parse_tools import reset_standard_name_counter, unique_standard_name
from fortran_tools import parse_fortran_file
from file_utils import create_file_list
from metadata_table import blank_metadata_line

## Init this now so that all Exceptions can be trapped
_LOGGER = init_log(os.path.basename(__file__))

## Recognized Fortran filename extensions
_FORTRAN_FILENAME_EXTENSIONS = ['F90', 'f90', 'F', 'f']

###############################################################################
def parse_command_line(args, description):
###############################################################################
    "Create an ArgumentParser to parse and return command-line arguments"
    parser = argparse.ArgumentParser(description=description,
                                     formatter_class=argparse.RawTextHelpFormatter)

    parser.add_argument("files", metavar='<list of files to process>',
                        type=str,
                        help="""Comma separated list of filenames to process
Filenames with a '.meta' suffix are treated as host model metadata files
Filenames with a '.txt' suffix are treated as containing a list of .meta
filenames""")

    parser.add_argument("--preproc-directives",
                        metavar='VARDEF1[,VARDEF2 ...]', type=str, default='',
                        help="Proprocessor directives used to correctly parse source files")

    parser.add_argument("--output-root", type=str,
                        metavar='<directory for generated files>',
                        default=os.getcwd(),
                        help="directory for generated files")

    parser.add_argument("--section-separator", type=str, default='',
                        help="""Comment line to separate CCPP metadata tables
(must start with a # or ; character)""")

    parser.add_argument("--verbose", action='count', default=0,
                        help="Log more activity, repeat for increased output")
    pargs = parser.parse_args(args)
    return pargs

###############################################################################
def write_metadata_file(mfilename, ftables, sep):
###############################################################################
    """
    Write the prototype metadata file, <mfilename>, based on the
    headers (<ftables>) parsed from Fortran.
    """
    # Write the metadata file with all the items collected from Fortran
    with open(mfilename, 'w') as outfile:
        header_sep = ''
        table_name = ''
        for table in ftables:
            # Write the table properties section
            # Note that there may be extra tables depending on how the
            # Fortran was parsed.
            if (not table_name) or (table_name != table.table_name):
                outfile.write("{}[ccpp-table-properties]{}".format(header_sep,
                                                                   os.linesep))
                header_sep = sep + os.linesep
                table_name = table.table_name
                outfile.write("  name = {}{}".format(table_name, os.linesep))
                outfile.write("  type = {}{}".format(table.table_type,
                                                     os.linesep))
            # end if
            for header in table.sections():
                lname_dict = {'1':'ccpp_constant_one'}
                outfile.write('{}[ccpp-arg-table]{}'.format(header_sep,
                                                            os.linesep))
                outfile.write('  name  = {}{}'.format(header.title,
                                                      os.linesep))
                outfile.write('  type  = {}{}'.format(header.header_type,
                                                      os.linesep))
                for var in header.variable_list():
                    lname = var.get_prop_value('local_name')
                    outfile.write('[ {} ]{}'.format(lname, os.linesep))
                    prop = var.get_prop_value('standard_name')
                    outfile.write('  standard_name = {}{}'.format(prop,
                                                                  os.linesep))
                    lname_dict[lname] = prop
                    prop = var.get_prop_value('units')
                    if not prop:
                        prop = 'enter_units'
                    # end if
                    outfile.write('  units = {}{}'.format(prop, os.linesep))
                    tprop = var.get_prop_value('type')
                    kprop = var.get_prop_value('kind')
                    if tprop == kprop:
                        outfile.write('  type = {}'.format(tprop))
                    else:
                        outfile.write('  type = {}'.format(tprop.lower()))
                        if kprop:
                            outfile.write(' | kind = {}'.format(kprop.lower()))
                        # end if
                    # end if
                    outfile.write(os.linesep)
                    dims = var.get_dimensions()
                    # Fill in standard names for dimensions
                    dlist = list()
                    if dims:
                        for dim in dims:
                            dslist = list()
                            for dimspec in dim.split(':'):
                                if dimspec and (dimspec in lname_dict):
                                    dstr = lname_dict[dimspec]
                                else:
                                    dstr = unique_standard_name()
                                # end if
                                dslist.append(dstr)
                            # end for
                            dlist.append(':'.join(dslist))
                        # end for
                    # end if
                    prop = '(' + ','.join(dlist) + ')'
                    outfile.write('  dimensions = {}{}'.format(prop,
                                                               os.linesep))
                    if header.header_type == 'scheme':
                        prop = var.get_prop_value('intent')
                        outfile.write('  intent = {}{}'.format(prop,
                                                               os.linesep))
                    # end if
                # end for
            # end for
        # end for
    # end with

###############################################################################
def parse_fortran_files(filenames, run_env, output_dir, sep, logger):
###############################################################################
    """
    Parse each file in <filenames> and produce a prototype metadata file
    with a metadata table for each arg_table entry in the file.
    """
    meta_filenames = list()
    for filename in filenames:
        logger.info('Looking for arg_tables from {}'.format(filename))
        reset_standard_name_counter()
        ftables, _ = parse_fortran_file(filename, run_env)
        # Create metadata filename
        filepath = '.'.join(os.path.basename(filename).split('.')[0:-1])
        fname = filepath + '.meta'
        mfilename = os.path.join(output_dir, fname)
        write_metadata_file(mfilename, ftables, sep)
        meta_filenames.append(mfilename)
    return meta_filenames

###############################################################################
def _main_func():
###############################################################################
    """Parse command line, then parse indicated Fortran files.
    Finally, generate a prototype metadata file for each Fortran file."""
    args = parse_command_line(sys.argv[1:], __doc__)
    verbosity = args.verbose
    if verbosity > 1:
        set_log_level(_LOGGER, logging.DEBUG)
    elif verbosity > 0:
        set_log_level(_LOGGER, logging.INFO)
    # end if
    # Make sure we know where output is going
    output_dir = os.path.abspath(args.output_root)
    # Optional table separator comment
    section_sep = args.section_separator
    if not blank_metadata_line(section_sep):
        emsg = "Illegal section separator, '{}', first character must be # or ;"
        raise CCPPError(emsg.format(section_sep))
    # We need to create a list of input Fortran files
    fort_files = create_file_list(args.files, _FORTRAN_FILENAME_EXTENSIONS,
                                  'Fortran', _LOGGER)
    preproc_defs = args.preproc_directives
    ## A few sanity checks
    ## Make sure output directory is legit
    if os.path.exists(output_dir):
        if not os.path.isdir(output_dir):
            errmsg = "output-root, '{}', is not a directory"
            raise CCPPError(errmsg.format(args.output_root))
        # end if
        if not os.access(output_dir, os.W_OK):
            errmsg = "Cannot write files to output-root ({})"
            raise CCPPError(errmsg.format(args.output_root))
        # end if (output_dir is okay)
    else:
        # Try to create output_dir (let it crash if it fails)
        os.makedirs(output_dir)
    # end if
    # Parse the files and create metadata
    run_env = CCPPFrameworkEnv(_LOGGER, verbose=verbosity,
                               host_files="", scheme_files="", suites="",
                               preproc_directives=preproc_defs)
    _ = parse_fortran_files(fort_files, run_env,
                            output_dir, section_sep, _LOGGER)

###############################################################################

if __name__ == "__main__":
    try:
        _main_func()
        sys.exit(0)
    except ParseInternalError as pie:
        _LOGGER.exception(pie)
        sys.exit(-1)
    except CCPPError as ccpp_err:
        if _LOGGER.getEffectiveLevel() <= logging.DEBUG:
            _LOGGER.exception(ccpp_err)
        else:
            _LOGGER.error(ccpp_err)
        # end if
        sys.exit(1)
    finally:
        logging.shutdown()
    # end try
