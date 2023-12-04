#!/usr/bin/env python3

"""
Create CCPP parameterization caps, host-model interface code,
physics suite runtime code, and CCPP framework documentation.
"""

# Python library imports
from __future__ import absolute_import
from __future__ import unicode_literals
from __future__ import print_function

import sys
import os
import logging
import re
# CCPP framework imports
from ccpp_database_obj import CCPPDatabaseObj
from ccpp_datafile import generate_ccpp_datatable
from ccpp_suite import API
from file_utils import check_for_writeable_file, remove_dir, replace_paths
from file_utils import create_file_list, move_modified_files
from file_utils import KINDS_FILENAME, KINDS_MODULE
from fortran_tools import parse_fortran_file, FortranWriter
from framework_env import parse_command_line
from host_cap import write_host_cap
from host_model import HostModel
from metadata_table import parse_metadata_file, SCHEME_HEADER_TYPE
from parse_tools import init_log, set_log_level, context_string
from parse_tools import register_fortran_ddt_name
from parse_tools import CCPPError, ParseInternalError

## Capture the Framework root
_SCRIPT_PATH = os.path.dirname(__file__)
_FRAMEWORK_ROOT = os.path.abspath(os.path.join(_SCRIPT_PATH, os.pardir))
_SRC_ROOT = os.path.join(_FRAMEWORK_ROOT, "src")
## Init this now so that all Exceptions can be trapped
_LOGGER = init_log(os.path.basename(__file__))

## Recognized Fortran filename extensions
_FORTRAN_FILENAME_EXTENSIONS = ['F90', 'f90', 'F', 'f']

## Metadata table types which can have extra variables in Fortran
_EXTRA_VARIABLE_TABLE_TYPES = ['module', 'host', 'ddt']

## Metadata table types where order is significant
_ORDERED_TABLE_TYPES = [SCHEME_HEADER_TYPE]

## CCPP Framework supported DDT types
_CCPP_FRAMEWORK_DDT_TYPES = ["ccpp_hash_table_t",
                             "ccpp_hashable_t",
                             "ccpp_hashable_char_t"]

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
            if path and (path[0] != '#') and (path[0] != '!'):
                # Check for an absolute path
                if not os.path.isabs(path):
                    # Assume relative pathnames are relative to pathsfile
                    path = os.path.normpath(os.path.join(root_path, path))
                # end if
                logger.info("Clean: Removing {}".format(path))
                try:
                    os.remove(path)
                except OSError as oserr:
                    success = False
                    errmsg = 'Unable to remove {}\n{}'
                    logger.warning(errmsg.format(path, oserr))
                # end try
            # end if (else skip blank or comment line)
        # end for
    # end with open
    logger.info("Clean: Removing {}".format(capfile))
    try:
        os.remove(capfile)
    except OSError as oserr:
        success = False
        errmsg = 'Unable to remove {}\n{}'
        logger.warning(errmsg.format(capfile, oserr))
    # end try
    if success:
        logger.info("ccpp_capgen clean successful, exiting")
    else:
        logger.info("ccpp_capgen clean encountered errors, exiting")
    # end if

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
    # end if
    for extension in _FORTRAN_FILENAME_EXTENSIONS:
        test_name = base + extension
        if os.path.exists(test_name):
            fort_filename = test_name
            break
        # end if
    # end for
    if fort_filename is None:
        raise CCPPError("Cannot find Fortran file associated with {}".format(filename))
    # end if
    return fort_filename

###############################################################################
def create_kinds_file(run_env, output_dir):
###############################################################################
    "Create the kinds.F90 file to be used by CCPP schemes and suites"
    kinds_filepath = os.path.join(output_dir, KINDS_FILENAME)
    if run_env.logger is not None:
        msg = 'Writing {} to {}'
        run_env.logger.info(msg.format(KINDS_FILENAME, output_dir))
    # end if
    kind_types = run_env.kind_types()
    with FortranWriter(kinds_filepath, "w",
                       "kinds for CCPP", KINDS_MODULE) as kindf:
        for kind_type in kind_types:
            use_stmt = "use ISO_FORTRAN_ENV, only: {} => {}"
            kindf.write(use_stmt.format(kind_type,
                                        run_env.kind_spec(kind_type)), 1)
        # end for
        kindf.write_preamble()
        for kind_type in kind_types:
            kindf.write("public :: {}".format(kind_type), 1)
        # end for
    # end with
    return kinds_filepath

###############################################################################
def add_error(error_string, new_error):
###############################################################################
    '''Add an error (<new_error>) to <error_string>, separating errors by a
    newline'''
    if error_string:
        error_string += '\n'
    # end if
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
        # end if
    # end for
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
        # end if
        if isinstance(fprop, str):
            fprop = fprop.lower()
        # end if
    # end if
    comp = mprop == fprop
    if not comp:
        errmsg = '{} mismatch ({} != {}) in {}{}'
        ctx = context_string(mvar.context)
        errors = add_error(errors,
                           errmsg.format(prop_name, mprop, fprop, title, ctx))
    # end if
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
    # end if
    if comp:
        # Now, compare the dims
        for dim_ind, mdim in enumerate(mdims):
            if ':' in mdim:
                mdim = ':'.join([x.strip() for x in mdim.split(':')])
            # end if
            fdim = fdims[dim_ind].strip()
            if ':' in fdim:
                fdim = ':'.join([x.strip() for x in fdim.split(':')])
            # end if
            if not case_sensitive:
                mdim = mdim.lower()
                fdim = fdim.lower()
            # end if
            # Naked colon is okay for Fortran side
            comp = fdim in (':', fdim)
            if not comp:
                errmsg = 'Error: dim {} mismatch ({} != {}) in {}/{}{}'
                stdname = mvar.get_prop_value('standard_name')
                ctx = context_string(mvar.context)
                errmsg = errmsg.format(dim_ind+1, mdim, fdims[dim_ind],
                                       title, stdname, ctx)
                errors = add_error(errors, errmsg)
            # end if
        # end for
    # end if
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
        if (mht != 'host') or (fht not in ('module', SCHEME_HEADER_TYPE)):
            errmsg = 'Metadata table type mismatch for {}, {} != {}{}'
            ctx = meta_header.start_context()
            raise CCPPError(errmsg.format(title, meta_header.header_type,
                                          fort_header.header_type, ctx))
        # end if
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
            # end if
        # end for
        list_match = mlen == flen
        # Check for optional Fortran variables that are not in metadata
        if flen > mlen:
            for find, fvar in enumerate(flist):
                lname = fvar.get_prop_value('local_name')
                _, mind = find_var_in_list(lname, mlist)
                if mind < 0:
                    if fvar.get_prop_value('optional'):
                        # This is an optional variable
                        flen -= 1
                    # end if
                # end if
            # end for
            list_match = mlen == flen
        # end if
        if not list_match:
            if fht in _EXTRA_VARIABLE_TABLE_TYPES:
                if flen > mlen:
                    list_match = True
                else:
                    etype = 'Fortran {}'.format(fht)
                # end if
            elif flen > mlen:
                etype = 'metadata header'
            else:
                etype = 'Fortran {}'.format(fht)
            # end if
        # end if
        if not list_match:
            errmsg = 'Variable mismatch in {}, variables missing from {}.'
            errors_found = add_error(errors_found, errmsg.format(title, etype))
        # end if
        for mind, mvar in enumerate(mlist):
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
                # end if (no else, we already reported an out-of-place error
                # Do not break to collect all missing variables
                continue
            # end if
            # At this point, we should have a Fortran variable
            if (not arrayref) and (fvar is None):
                errmsg = 'Variable mismatch in {}, no Fortran variable {}.'
                errors_found = add_error(errors_found, errmsg.format(title,
                                                                     lname))
                continue
            # end if
            # Check order dependence
            if fht in _ORDERED_TABLE_TYPES:
                if find != mind:
                    errmsg = 'Out of order argument, {} in {}'
                    errors_found = add_error(errors_found,
                                             errmsg.format(lname, title))
                    continue
                # end if
            # end if
            if arrayref:
                # Array reference, do not look for this in Fortran table
                continue
            # end if
            errs = var_comp('local_name', mvar, fvar, title)
            if errs:
                errors_found = add_error(errors_found, errs)
            else:
                errs = var_comp('type', mvar, fvar, title)
                if errs:
                    errors_found = add_error(errors_found, errs)
                # end if
                errs = var_comp('kind', mvar, fvar, title)
                if errs:
                    errors_found = add_error(errors_found, errs)
                # end if
                if meta_header.header_type == SCHEME_HEADER_TYPE:
                    errs = var_comp('intent', mvar, fvar, title)
                    if errs:
                        errors_found = add_error(errors_found, errs)
                    # end if
                # end if
                # Compare dimensions
                errs = dims_comp(meta_header, mvar, fvar, title, logger)
                if errs:
                    errors_found = add_error(errors_found, errs)
                # end if
            # end if
        # end for
    # end if
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
    for mheader in meta_headers:
        fheader = None
        mtitle = mheader.title
        for findex in range(len(fort_headers)): #pylint: disable=consider-using-enumerate
            if fort_headers[findex].title == mtitle:
                fheader = fort_headers.pop(findex)
                break
            # end if
        # end for
        if fheader is None:
            tlist = '\n    '.join([x.title for x in fort_headers])
            logger.debug("CCPP routines in {}:{}".format(ffilename, tlist))
            errmsg = "No matching Fortran routine found for {} in {}"
            raise CCPPError(errmsg.format(mtitle, ffilename))
        # end if
        header_dict[mheader] = fheader
        # end if
    # end while
    if fort_headers:
        errmsg = ""
        sep = ""
        for fheader in fort_headers:
            if fheader.has_variables:
                errmsg += sep + "No matching metadata header found for {} in {}"
                errmsg = errmsg.format(fheader.title, mfilename)
                sep = "\n"
            # end if
        # end for
        if errmsg:
            raise CCPPError(errmsg)
        # end if
    # end if
    # We have a one-to-one set, compare headers
    errors_found = ''
    for mheader in header_dict:
        fheader = header_dict[mheader]
        errors_found += compare_fheader_to_mheader(mheader, fheader, logger)
    # end for
    if errors_found:
        num_errors = len(re.findall(r'\n', errors_found)) + 1
        errmsg = "{}\n{} error{} found comparing {} to {}"
        raise CCPPError(errmsg.format(errors_found, num_errors,
                                      's' if num_errors > 1 else '',
                                      mfilename, ffilename))
    # end if
    # No return, an exception is raised on error

###############################################################################
def duplicate_item_error(title, filename, itype, orig_item):
###############################################################################
    """Raise an error indicating a duplicate item of type, <itype>"""
    errmsg = "Duplicate {typ}, {title}, found in {file}"
    edict = {'title':title, 'file':filename, 'typ':itype}
    ofile = orig_item.context.filename
    if ofile is not None:
        errmsg = errmsg + ", original found in {ofile}"
        edict['ofile'] = ofile
    # end if
    raise CCPPError(errmsg.format(**edict))

###############################################################################
def parse_host_model_files(host_filenames, host_name, run_env):
###############################################################################
    """
    Gather information from host files (e.g., DDTs, registry) and
    return a host model object with the information.
    """
    header_dict = {}
    table_dict = {}
    known_ddts = list()
    logger = run_env.logger
    for filename in host_filenames:
        logger.info('Reading host model data from {}'.format(filename))
        # parse metadata file
        mtables = parse_metadata_file(filename, known_ddts, run_env)
        fort_file = find_associated_fortran_file(filename)
        ftables = parse_fortran_file(fort_file, run_env)
        # Check Fortran against metadata (will raise an exception on error)
        mheaders = list()
        for sect in [x.sections() for x in mtables]:
            mheaders.extend(sect)
        # end for
        fheaders = list()
        for sect in [x.sections() for x in ftables]:
            fheaders.extend(sect)
        # end for
        check_fortran_against_metadata(mheaders, fheaders,
                                       filename, fort_file, logger)
        # Check for duplicate tables, then add to dict
        for table in mtables:
            if table.table_name in table_dict:
                duplicate_item_error(table.table_name, filename,
                                     table.table_type, table_dict[header.title])
            else:
                table_dict[table.table_name] = table
            # end if
        # end for
        # Check for duplicate headers, then add to dict
        for header in mheaders:
            if header.title in header_dict:
                duplicate_item_error(header.title, filename,
                                     header.header_type,
                                     header_dict[header.title])
            else:
                header_dict[header.title] = header
                if header.header_type == 'ddt':
                    known_ddts.append(header.title)
            # end if
        # end for
    # end for
    if not host_name:
        host_name = None
    # end if
    host_model = HostModel(table_dict, host_name, run_env)
    return host_model

###############################################################################
def parse_scheme_files(scheme_filenames, run_env):
###############################################################################
    """
    Gather information from scheme files (e.g., init, run, and finalize
    methods) and return resulting dictionary.
    """
    table_dict = {} # Duplicate check and for dependencies processing
    header_dict = {} # To check for duplicates
    known_ddts = list()
    logger = run_env.logger
    for filename in scheme_filenames:
        logger.info('Reading CCPP schemes from {}'.format(filename))
        # parse metadata file
        mtables = parse_metadata_file(filename, known_ddts, run_env)
        fort_file = find_associated_fortran_file(filename)
        ftables = parse_fortran_file(fort_file, run_env)
        # Check Fortran against metadata (will raise an exception on error)
        mheaders = list()
        for sect in [x.sections() for x in mtables]:
            mheaders.extend(sect)
        # end for
        fheaders = list()
        for sect in [x.sections() for x in ftables]:
            fheaders.extend(sect)
        # end for
        check_fortran_against_metadata(mheaders, fheaders,
                                       filename, fort_file, logger)
        # Check for duplicate tables, then add to dict
        for table in mtables:
            if table.table_name in table_dict:
                duplicate_item_error(table.table_name, filename,
                                     table.table_type, table_dict[header.title])
            else:
                table_dict[table.table_name] = table
            # end if
        # end for
        # Check for duplicate headers, then add to dict
        for header in mheaders:
            if header.title in header_dict:
                duplicate_item_error(header.title, filename, header.header_type,
                                     header_dict[header.title])
            else:
                header_dict[header.title] = header
                if header.header_type == 'ddt':
                    known_ddts.append(header.title)
                # end if
            # end if
        # end for
    # end for
    return header_dict.values(), table_dict

###############################################################################
def clean_capgen(cap_output_file, logger):
###############################################################################
    """Attempt to remove the files created by the last invocation of capgen"""
    log_level = logger.getEffectiveLevel()
    set_log_level(logger, logging.INFO)
    if os.path.exists(cap_output_file):
        logger.info("Cleaning capgen files from {}".format(cap_output_file))
        delete_pathnames_from_file(cap_output_file, logger)
    else:
        emsg = "Unable to run clean, {} not found"
        logger.error(emsg.format(cap_output_file))
    # end if
    set_log_level(logger, log_level)

###############################################################################
def capgen(run_env, return_db=False):
###############################################################################
    """Parse indicated host, scheme, and suite files.
    Generate code to allow host model to run indicated CCPP suites."""
    ## A few sanity checks
    ## Make sure output directory is legit
    if os.path.exists(run_env.output_dir):
        if not os.path.isdir(run_env.output_dir):
            errmsg = "output-root, '{}', is not a directory"
            raise CCPPError(errmsg.format(run_env.output_root))
        # end if
        if not os.access(run_env.output_dir, os.W_OK):
            errmsg = "Cannot write files to output-root ({})"
            raise CCPPError(errmsg.format(run_env.output_root))
        # end if (output_dir is okay)
    else:
        # Try to create output_dir (let it crash if it fails)
        os.makedirs(run_env.output_dir)
    # end if
    # Pre-register base CCPP DDT types:
    for ddt_name in _CCPP_FRAMEWORK_DDT_TYPES:
        register_fortran_ddt_name(ddt_name)
    # end for
    src_dir = os.path.join(_FRAMEWORK_ROOT, "src")
    host_files = run_env.host_files
    host_name = run_env.host_name
    scheme_files = run_env.scheme_files
    # We need to create three lists of files, hosts, schemes, and SDFs
    host_files = create_file_list(run_env.host_files, ['meta'], 'Host',
                                  run_env.logger)
    # The host model needs to know about the constituents module
    const_mod = os.path.join(_SRC_ROOT, "ccpp_constituent_prop_mod.meta")
    if const_mod not in host_files:
        host_files.append(const_mod)
    # end if
    scheme_files = create_file_list(run_env.scheme_files, ['meta'],
                                    'Scheme', run_env.logger)
    sdfs = create_file_list(run_env.suites, ['xml'], 'Suite', run_env.logger)
    check_for_writeable_file(run_env.datatable_file, "Cap output datatable")
    ##XXgoldyXX: Temporary warning
    if run_env.generate_docfiles:
        raise CCPPError("--generate-docfiles not yet supported")
    # end if
    # First up, handle the host files
    host_model = parse_host_model_files(host_files, host_name, run_env)
    # We always need to parse the ccpp_constituent_prop_ptr_t DDT
    const_prop_mod = os.path.join(src_dir, "ccpp_constituent_prop_mod.meta")
    if const_prop_mod not in scheme_files:
        scheme_files = [const_prop_mod] + scheme_files
    # end if
    # Next, parse the scheme files
    scheme_headers, scheme_tdict = parse_scheme_files(scheme_files, run_env)
    if run_env.verbose:
        ddts = host_model.ddt_lib.keys()
        if ddts:
            run_env.logger.debug("DDT definitions = {}".format(ddts))
        # end if
    # end if
    plist = host_model.prop_list('local_name')
    if run_env.verbose:
        run_env.logger.debug("{} variables = {}".format(host_model.name, plist))
        run_env.logger.debug("schemes = {}".format([x.title
                                                    for x in scheme_headers]))
    # Finally, we can get on with writing suites
    # Make sure to write to temporary location if files exist in <output_dir>
    if not os.path.exists(run_env.output_dir):
        # Try to create output_dir (let it crash if it fails)
        os.makedirs(run_env.output_dir)
        # Nothing here, use it for output
        outtemp_dir = run_env.output_dir
    elif not os.listdir(run_env.output_dir):
        # Nothing here, use it for output
        outtemp_dir = run_env.output_dir
    else:
        # We need to create a temporary staging area, create it here
        outtemp_name = "ccpp_temp_scratch_dir"
        outtemp_dir = os.path.join(run_env.output_dir, outtemp_name)
        if os.path.exists(outtemp_dir):
            remove_dir(outtemp_dir, force=True)
        # end if
        os.makedirs(outtemp_dir)
    # end if
    ccpp_api = API(sdfs, host_model, scheme_headers, run_env)
    cap_filenames = ccpp_api.write(outtemp_dir, run_env)
    if run_env.generate_host_cap:
        # Create a cap file
        cap_module = host_model.ccpp_cap_name()
        host_files = [write_host_cap(host_model, ccpp_api, cap_module,
                                     outtemp_dir, run_env)]
    else:
        host_files = list()
    # end if
    # Create the kinds file
    kinds_file = create_kinds_file(run_env, outtemp_dir)
    # Move any changed files to output_dir and remove outtemp_dir
    move_modified_files(outtemp_dir, run_env.output_dir,
                        overwrite=run_env.force_overwrite, remove_src=True)
    # We have to rename the files we created
    if outtemp_dir != run_env.output_dir:
        replace_paths(cap_filenames, outtemp_dir, run_env.output_dir)
        replace_paths(host_files, outtemp_dir, run_env.output_dir)
        kinds_file = kinds_file.replace(outtemp_dir, run_env.output_dir)
    # end if
    # Finally, create the database of generated files and caps
    # This can be directly in output_dir because it will not affect dependencies
    generate_ccpp_datatable(run_env, host_model, ccpp_api,
                            scheme_headers, scheme_tdict, host_files,
                            cap_filenames, kinds_file, src_dir)
    if return_db:
        return CCPPDatabaseObj(run_env, host_model=host_model, api=ccpp_api)
    # end if
    return None

###############################################################################
def _main_func():
###############################################################################
    """Parse command line, then parse indicated host, scheme, and suite files.
    Finally, generate code to allow host model to run indicated CCPP suites."""
    framework_env = parse_command_line(sys.argv[1:], __doc__, logger=_LOGGER)
    if framework_env.verbosity > 1:
        set_log_level(framework_env.logger, logging.DEBUG)
    elif framework_env.verbosity > 0:
        set_log_level(framework_env.logger, logging.INFO)
    # end if
    if framework_env.clean:
        clean_capgen(framework_env.datatable_file, framework_env.logger)
    else:
        _ = capgen(framework_env)
    # end if (clean)

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
