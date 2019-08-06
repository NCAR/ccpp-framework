#!/usr/bin/env python

"""
Create CCPP parameterization caps, host-model interface code,
physics suite runtime code, and CCPP framework documentation.
"""

# Python library imports
from __future__ import absolute_import
from __future__ import unicode_literals
from __future__ import print_function

import argparse
import sys
import os
import os.path
import logging
import re
# CCPP framework imports
from parse_tools import init_log, set_log_level, context_string
from parse_tools import CCPPError, ParseInternalError
from file_utils import check_for_existing_file, check_for_writeable_file
from file_utils import create_file_list
from fortran_tools import parse_fortran_file, FortranWriter
from host_model import HostModel
from host_cap import write_host_cap
from ccpp_suite import API, Suite, COPYRIGHT, KINDS_MODULE, KINDS_FILENAME
from metadata_table import MetadataTable

## Init this now so that all Exceptions can be trapped
logger = init_log(os.path.basename(__file__))

## Recognized Fortran filename extensions
__fortran_filename_extensions = ['F90', 'f90', 'F', 'f']

## Metadata table types which can have extra variables in Fortran
__extra_variable_table_types = ['module', 'host', 'ddt']

## Metadata table types where order is significant
__ordered_table_types = ['scheme']

## header for kinds file
kinds_header = '''
!>
!! @brief Auto-generated kinds for CCPP
!!
!
'''

###############################################################################
def parse_command_line(args, description):
###############################################################################
    "Create an ArgumentParser to parse and return command-line arguments"
    parser = argparse.ArgumentParser(description=description,
                                     formatter_class=argparse.RawTextHelpFormatter)

    parser.add_argument("--host-files", metavar='<host files filename>',
                        type=str, required=True,
                        help="""Comma separated list of host filenames to process
Filenames with a '.meta' suffix are treated as host model metadata files
Filenames with a '.txt' suffix are treated as containing a list of .meta
filenames""")

    parser.add_argument("--scheme-files", metavar='<scheme files filename>',
                        type=str, required=True,
                        help="""Comma separated list of scheme filenames to process
Filenames with a '.meta' suffix are treated as scheme metadata files
Filenames with a '.txt' suffix are treated as containing a list of .meta
filenames""")

    parser.add_argument("--suites", metavar='<Suite definition file(s)>',
                        type=str, required=True,
                        help="""Comma separated list of suite definition filenames to process
Filenames with a '.xml' suffix are treated as suite definition XML files
Other filenames are treated as containing a list of .xml filenames""")

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
                        action='store_true', default=True,
                        help="Generate a host cap with correct API calling sequence")

    parser.add_argument("--host-name", type=str, default='',
                        help='Name of host model to use in CCPP API')

    parser.add_argument("--clean", action='store_true',
                        help='Remove files created by this script, then exit',
                        default=False)

    parser.add_argument("--kind-phys", type=str, default='REAL64',
                        metavar="kind_phys",
                        help='Data size for real(kind_phys) data')

    parser.add_argument("--generate-docfiles",
                        metavar='HTML | Latex | HTML,Latex', type=str,
                        help="Generate LaTeX and/or HTML documentation")

    parser.add_argument("--verbose", action='count',
                        help="Log more activity, repeat for increased output")
    pargs = parser.parse_args(args)
    return pargs

###############################################################################
def delete_pathnames_from_file(capfile, logger):
###############################################################################
    """Remove all the filenames found in <capfile>, then delete <capfile>"""
    root_path = os.path.dirname(os.path.abspath(capfile))
    success = True
    with open(capfile, 'r') as infile:
        for line in infile.readlines():
            path = line.strip()
            # Skip blank lines and lines which appear to start with a comment.
            if (len(path) > 0) and (path[0] != '#') and (path[0] != '!'):
                # Check for an absolute path
                if not os.path.isabs(path):
                    # Assume relative pathnames are relative to pathsfile
                    path = os.path.normpath(os.path.join(root_path, path))
                # End if
                logger.info("Clean: Removing {}".format(path))
                try:
                    os.remove(path)
                except OSError as oserr:
                    success = False
                    errmsg = 'Unable to remove {}\n{}'
                    logger.warning(errmsg.format(path, oserr))
                # End try
            # End if (else skip blank or comment line)
        # End for
    # End with open
    logger.info("Clean: Removing {}".format(capfile))
    try:
        os.remove(capfile)
    except OSError as oserr:
        success = False
        errmsg = 'Unable to remove {}\n{}'
        logger.warning(errmsg.format(capfile, oserr))
    # End try
    if success:
        logger.info("ccpp_capgen clean successful, exiting")
    else:
        logger.info("ccpp_capgen clean encountered errors, exiting")
    # End if

###############################################################################
def find_associated_fortran_file(filename):
###############################################################################
    "Find the Fortran file associated with metadata file, <filename>"
    fort_filename = None
    lastdot = filename.rfind('.')
    ##XXgoldyXX: Should we check to make sure <filename> ends in '.meta.'?
    if lastdot < 0:
        base = filename + '.'
    else:
        base = filename[0:lastdot+1]
    # End if
    for extension in __fortran_filename_extensions:
        test_name = base + extension
        if os.path.exists(test_name):
            fort_filename = test_name
            break
        # End if
    # End for
    if fort_filename is None:
        raise CCPPError("Cannot find Fortran file associated with {}".format(filename))
    # End if
    return fort_filename

###############################################################################
def create_kinds_file(kind_phys, output_dir, logger):
###############################################################################
    "Create the kinds.F90 file to be used by CCPP schemes and suites"
    kinds_filepath = os.path.join(output_dir, KINDS_FILENAME)
    if logger is not None:
        msg = 'Writing {} to {}'
        logger.info(msg.format(KINDS_FILENAME, output_dir))
    # End if
    with FortranWriter(kinds_filepath, "w") as kw:
        kw.write(COPYRIGHT, 0)
        kw.write(kinds_header, 0)
        kw.write('module {}'.format(KINDS_MODULE), 0)
        kw.write('', 0)
        use_stmt = 'use ISO_FORTRAN_ENV, only: kind_phys => {}'
        kw.write(use_stmt.format(kind_phys), 1)
        kw.write('', 0)
        kw.write('implicit none', 1)
        kw.write('private', 1)
        kw.write('', 0)
        kw.write('public kind_phys', 1)
        kw.write('', 0)
        kw.write('end module {}'.format(KINDS_MODULE), 0)
    # End with
    return kinds_filepath

###############################################################################
def add_error(error_string, new_error):
###############################################################################
    '''Add an error (<new_error>) to <error_string>, separating errors by a
    newline'''
    if error_string:
        error_string += '\n'
    # End if
    return error_string + new_error

###############################################################################
def is_arrayspec(local_name):
###############################################################################
    "Return True iff <local_name> is an array reference"
    return '(' in local_name

###############################################################################
def find_var_in_list(local_name, var_list):
###############################################################################
    """Find a variable, <local_name>, in <var_list>.
    local name is used because Fortran metadata variables do not have
    real standard names.
    Note: The search is case insensitive.
    Return both the variable and the index where it was found.
    If not found, return None for the variable and -1 for the index
    """
    vvar = None
    vind = -1
    lname = local_name.lower()
    for lind, lvar in enumerate(var_list):
        if lvar.get_prop_value('local_name').lower() == lname:
            vvar = lvar
            vind = lind
            break
        # End if
    # End for
    return vvar, vind

###############################################################################
def var_comp(prop_name, mvar, fvar, title, case_sensitive=False):
###############################################################################
    "Compare a property between two variables"
    errors = ''
    mprop = mvar.get_prop_value(prop_name)
    fprop = fvar.get_prop_value(prop_name)
    if not case_sensitive:
        if isinstance(mprop, str):
            mprop = mprop.lower()
        # End if
        if isinstance(fprop, str):
            fprop = fprop.lower()
        # End if
    # End if
    comp = mprop == fprop
    if not comp:
        errmsg = '{} mismatch ({} != {}) in {}{}'
        ctx = context_string(mvar.context)
        errors = add_error(errors,
                           errmsg.format(prop_name, mprop, fprop, title, ctx))
    # End if
    return errors

###############################################################################
def dims_comp(mheader, mvar, fvar, title, logger, case_sensitive=False):
###############################################################################
    "Compare the dimensions attribute of two variables"
    errors = ''
    mdims = mvar.get_dimensions()
    fdims = mheader.convert_dims_to_standard_names(fvar, logger=logger)
    comp = len(mdims) == len(fdims)
    if not comp:
        errmsg = 'Error: rank mismatch in {}/{} ({} != {}){}'
        stdname = mvar.get_prop_value('standard_name')
        ctx = context_string(mvar.context)
        errors = add_error(errors, errmsg.format(title, stdname,
                                                 len(mdims), len(fdims), ctx))
    # End if
    if comp:
        # Now, compare the dims
        for dim_ind in range(len(mdims)):
            mdim = mdims[dim_ind]
            fdim = fdims[dim_ind]
            if not case_sensitive:
                mdim = mdim.lower()
                fdim = fdim.lower()
            # End if
            # Naked colon is okay for Fortran side
            comp = (fdim == ':') or (mdim == fdim)
            if not comp:
                errmsg = 'Error: dim {} mismatch ({} != {}) in {}/{}{}'
                stdname = mvar.get_prop_value('standard_name')
                ctx = context_string(mvar.context)
                errmsg = errmsg.format(dim_ind+1, mdim, fdims[dim_ind],
                                       title, stdname, ctx)
                errors = add_error(errors, errmsg)
            # End if
        # End for
    # End if
    return errors

###############################################################################
def compare_fheader_to_mheader(meta_header, fort_header, logger):
###############################################################################
    """Compare a metadata header against the header generated from the
    corresponding code in the associated Fortran file.
    Return a string with any errors found (empty string is no errors).
    """
    errors_found = ''
    title = meta_header.title
    mht = meta_header.header_type
    fht = fort_header.header_type
    if mht != fht:
        # Special case, host metadata can be in a Fortran module or scheme
        if (mht != 'host') or ((fht != 'module') and (fht != 'scheme')):
            errmsg = 'Metadata table type mismatch for {}, {} != {}{}'
            ctx = meta_header.start_context()
            raise CCPPError(errmsg.format(title, meta_header.header_type,
                                          fort_header.header_type, ctx))
        # End if
    else:
        # The headers should have the same variables in the same order
        # The exception is that a Fortran module can have variable declarations
        # after all the metadata variables.
        mlist = meta_header.variable_list()
        mlen = len(mlist)
        flist = fort_header.variable_list()
        flen = len(flist)
        # Remove array references from mlist before checking lengths
        for mvar in mlist:
            if is_arrayspec(mvar.get_prop_value('local_name')):
                mlen -= 1
            # End if
        # End for
        list_match = mlen == flen
        if not list_match:
            if fht in __extra_variable_table_types:
                if flen > mlen:
                    list_match= True
                else:
                    etype = 'Fortran {}'.format(fht)
                # End if
            elif flen > mlen:
                etype = 'metadata header'
            else:
                etype = 'Fortran {}'.format(fht)
            # End if
        # End if
        if not list_match:
            errmsg = 'Variable mismatch in {}, variables missing from {}.'
            errors_found = add_error(errors_found, errmsg.format(title, etype))
        # End if
        for mind, mvar in enumerate(mlist):
            std_name = mvar.get_prop_value('standard_name')
            lname = mvar.get_prop_value('local_name')
            arrayref = is_arrayspec(lname)
            fvar, find = find_var_in_list(lname, flist)
            if mind >= flen:
                if arrayref:
                    # Array reference, variable not in Fortran table
                    pass
                elif fvar is None:
                    errmsg = 'No Fortran variable for {} in {}'
                    errors_found = add_error(errors_found,
                                             errmsg.format(lname, title))
                # End if (no else, we already reported an out-of-place error
                # Do not break to collect all missing variables
                continue
            # End if
            # At this point, we should have a Fortran variable
            if fvar is None:
                errmsg = 'Variable mismatch in {}, no Fortran variable {}.'
                errors_found = add_error(errors_found, errmsg.format(title,
                                                                     lname))
                continue
            # End if
            # Check order dependence
            if fht in __ordered_table_types:
                if find != mind:
                    errmsg = 'Out of order argument, {} in {}'
                    errors_found = add_error(errors_found,
                                             errmsg.format(lname, title))
                    continue
                # End if
            # End if
            if arrayref:
                # Array reference, do not look for this in Fortran table
                continue
            # End if
            errs = var_comp('local_name', mvar, fvar, title)
            if errs:
                errors_found = add_error(errors_found, errs)
            else:
                errs = var_comp('type', mvar, fvar, title)
                if errs:
                    errors_found = add_error(errors_found, errs)
                # End if
                errs = var_comp('kind', mvar, fvar, title)
                if errs:
                    errors_found = add_error(errors_found, errs)
                # End if
                if meta_header.header_type == 'scheme':
                    errs = var_comp('intent', mvar, fvar, title)
                    if errs:
                        errors_found = add_error(errors_found, errs)
                    # End if
                # End if
                # Compare dimensions
                errs = dims_comp(meta_header, mvar, fvar, title, logger)
                if errs:
                    errors_found = add_error(errors_found, errs)
                # End if
            # End if
        # End for
    # End if
    return errors_found

###############################################################################
def check_fortran_against_metadata(meta_headers, fort_headers,
                                   mfilename, ffilename, logger):
###############################################################################
    """Compare a set of metadata headers from <mfilename> against the
    code in the associated Fortran file, <ffilename>.
    NB: This routine destroys the list, <fort_headers> but returns the
       contents in an association dictionary on successful completion."""
    header_dict = {} # Associate a Fortran header for every metadata header
    for mindex in range(len(meta_headers)):
        mheader = meta_headers[mindex]
        fheader = None
        mtitle = mheader.title
        for findex in range(len(fort_headers)):
            if fort_headers[findex].title == mtitle:
                fheader = fort_headers.pop(findex)
                break
            # End if
        # End for
        if fheader is None:
            tlist = '\n    '.join([x.title for x in fort_headers])
            logger.debug("CCPP routines in {}:{}".format(ffilename, tlist))
            errmsg = "No matching Fortran routine found for {} in {}"
            raise CCPPError(errmsg.format(mtitle, ffilename))
        else:
            header_dict[mheader] = fheader
        # End if
    # End while
    if len(fort_headers) > 0:
        errmsg = ""
        sep = ""
        for fheader in fort_headers:
            if fheader.has_variables:
                errmsg += sep + "No matching metadata header found for {} in {}"
                errmsg = errmsg.format(fheader.title, mfilename)
                sep = "\n"
            # End if
        # End for
        if errmsg:
            raise CCPPError(errmsg)
        # End if
    # End if
    # We have a one-to-one set, compare headers
    errors_found = ''
    for mheader in header_dict.keys():
        fheader = header_dict[mheader]
        errors_found += compare_fheader_to_mheader(mheader, fheader, logger)
    # End for
    if errors_found:
        num_errors = len(re.findall(r'\n', errors_found)) + 1
        errmsg = "{}\n{} error{} found comparing {} to {}"
        raise CCPPError(errmsg.format(errors_found, num_errors,
                                      's' if num_errors > 1 else '',
                                      mfilename, ffilename))
    # End if

    return header_dict

###############################################################################
def parse_host_model_files(host_filenames, preproc_defs, host_name, logger):
###############################################################################
    """
    Gather information from host files (e.g., DDTs, registry) and
    return a host model object with the information.
    """
    meta_headers = {}
    known_ddts = list()
    for filename in host_filenames:
        logger.info('Reading host model data from {}'.format(filename))
        # parse metadata file
        mheaders = MetadataTable.parse_metadata_file(filename, known_ddts,
                                                     logger)
        fort_file = find_associated_fortran_file(filename)
        fheaders = parse_fortran_file(fort_file, preproc_defs==preproc_defs,
                                      logger=logger)
        # Check Fortran against metadata (will raise an exception on error)
        hdr_dict = check_fortran_against_metadata(mheaders, fheaders,
                                                  filename, fort_file, logger)
        # Check for duplicates, then add to dict
        for header in mheaders:
            if header.title in meta_headers:
                errmsg = "Duplicate {typ}, {title}, found in {file}"
                edict = {'title':header.title,
                         'file':filename,
                         'typ':header.header_type}
                oheader = meta_headers[header.title]
                ofile = oheader.context.filename
                if ofile is not None:
                    errmsg = errmsg + ", original found in {ofile}"
                    edict['ofile'] = ofile
                # End if
                raise CCPPError(errmsg.format(**edict))
            else:
                meta_headers[header.title] = header
                if header.header_type == 'ddt':
                    known_ddts.append(header.title)
                # End if
            # End if
        # End for
    # End for
    if len(host_name) == 0:
        host_name = None
    # End if
    host_model = HostModel(meta_headers.values(), host_name, logger)
    return host_model

###############################################################################
def parse_scheme_files(scheme_filenames, preproc_defs, logger):
###############################################################################
    """
    Gather information from scheme files (e.g., init, run, and finalize
    methods) and return resulting dictionary.
    """
    meta_headers = list()
    header_dict = {} # To check for duplicates
    known_ddts = list()
    for filename in scheme_filenames:
        logger.info('Reading CCPP schemes from {}'.format(filename))
        # parse metadata file
        mheaders = MetadataTable.parse_metadata_file(filename, known_ddts,
                                                     logger)
        fort_file = find_associated_fortran_file(filename)
        fheaders = parse_fortran_file(fort_file, preproc_defs==preproc_defs,
                                      logger=logger)
        # Check Fortran against metadata (will raise an exception on error)
        hdr_dict = check_fortran_against_metadata(mheaders, fheaders,
                                                  filename, fort_file, logger)
        # Check for duplicates, then add to dict
        for header in mheaders:
            if header.title in header_dict:
                errmsg = "Duplicate {ttype}, {title}, found in {file}"
                edict = {'title':header.title,
                         'file':filename,
                         'ttype':header.header_type}
                oheader = header_dict[header.title]
                ofile = oheader.context.filename
                if ofile is not None:
                    errmsg = errmsg + ", original found in {ofile}"
                    edict['ofile'] = ofile
                # End if
                raise CCPPError(errmsg.format(**edict))
            else:
                meta_headers.append(header)
                header_dict[header.title] = header
                if header.header_type == 'ddt':
                    known_ddts.append(header.title)
                # End if
            # End if
        # End for
    # End for
    return meta_headers

###############################################################################
def _main_func():
###############################################################################
    """Parse command line, then parse indicated host, scheme, and suite files.
    Finally, generate code to allow host model to run indicated CCPP suites."""
    args = parse_command_line(sys.argv[1:], __doc__)
    verbosity = args.verbose
    if verbosity > 1:
        set_log_level(logger, logging.DEBUG)
    elif verbosity > 0:
        set_log_level(logger, logging.INFO)
    # End if
    # Make sure we know where output is going
    output_dir = os.path.abspath(args.output_root)
    if os.path.abspath(args.cap_pathlist):
        cap_output_file = args.cap_pathlist
    else:
        cap_output_file = os.path.abspath(os.path.join(output_dir,
                                                       args.cap_pathlist))
    # End if
    # Make sure we can create output file lists
    if not os.path.isabs(cap_output_file):
        cap_output_file = os.path.normpath(os.path.join(output_dir,
                                                        cap_output_file))
    # End if
    if args.clean:
        set_log_level(logger, logging.INFO)
        if os.path.exists(cap_output_file):
            logger.info("Cleaning capgen files from {}".format(cap_output_file))
            delete_pathnames_from_file(cap_output_file, logger)
        else:
            emsg = "Unable to run clean, {} not found"
            logger.error(emsg.format(cap_output_file))
        # End if
    else:
        # We need to create three lists of files, hosts, schemes, and SDFs
        host_files = create_file_list(args.host_files, ['meta'], 'Host', logger)
        scheme_files = create_file_list(args.scheme_files, ['meta'],
                                        'Scheme', logger)
        sdfs = create_file_list(args.suites, ['xml'], 'Suite', logger)
        preproc_defs = args.preproc_directives
        gen_hostcap = args.generate_host_cap
        gen_docfiles = args.generate_docfiles
        ## A few sanity checks
        ## Make sure output directory is legit
        if os.path.exists(output_dir):
            if not os.path.isdir(output_dir):
                errmsg = "output-root, '{}', is not a directory"
                raise CCPPError(errmsg.format(args.output_root))
            elif not os.access(output_dir, os.W_OK):
                errmsg = "Cannot write files to output-root ({})"
                raise CCPPError(errmsg.format(args.output_root))
            # End if (output_dir is okay)
        else:
            # Try to create output_dir (let it crash if it fails)
            os.makedirs(output_dir)
        # End if
        check_for_writeable_file(cap_output_file, "Cap output file")
        ##XXgoldyXX: Temporary warning
        if gen_docfiles:
            raise CCPPError("--gen-docfiles not yet supported")
        # End if
        # First up, handle the host files
        host_model = parse_host_model_files(host_files, preproc_defs,
                                            args.host_name, logger)
        # Next, parse the scheme files
        scheme_headers = parse_scheme_files(scheme_files, preproc_defs, logger)
        ddts = host_model._ddt_lib.keys()
        if ddts:
            logger.debug("DDT definitions = {}".format(ddts))
        # End if
        plist = host_model.prop_list('local_name')
        logger.debug("{} variables = {}".format(host_model.name, plist))
        logger.debug("schemes = {}".format([x.title for x in scheme_headers]))
        # Finally, we can get on with writing suites
        ccpp_api = API(sdfs, host_model, scheme_headers, logger)
        cap_filenames = ccpp_api.write(output_dir, logger)
        if gen_hostcap:
            # Create a cap file
            hcap_filename = write_host_cap(host_model, ccpp_api,
                                           output_dir, logger)
        else:
            hcap_filename = None
        # End if
        # Create the kinds file
        kinds_file = create_kinds_file(args.kind_phys, output_dir, logger)
        # Finally, create the list of generated files
        with open(cap_output_file, 'w') as cap_names:
            for path in cap_filenames:
                cap_names.write('{}\n'.format(path))
            # End for
            if hcap_filename is not None:
                cap_names.write('{}\n'.format(hcap_filename))
            # End if
            cap_names.write('{}\n'.format(kinds_file))
        # End with
    # End if (clean)

###############################################################################

if __name__ == "__main__":
    try:
        _main_func()
        sys.exit(0)
    except ParseInternalError as pie:
        logger.exception(pie)
        sys.exit(-1)
    except CCPPError as ca:
        if logger.getEffectiveLevel() <= logging.DEBUG:
            logger.exception(ca)
        else:
            logger.error(ca)
        # End if
        sys.exit(1)
    finally:
        logging.shutdown()
    # End try
