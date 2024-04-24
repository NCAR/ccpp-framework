#!/usr/bin/env python3

"""
Recursively compare all fortran and metadata files in user-supplied directory, and report any problems
USAGE:
    ./offline_check_fortran_vs_metadata.py --directory <full path to directory with scheme files> (--debug)
"""


import sys
import os
import glob
import logging
import argparse
import site
# Enable imports from parent directory
site.addsitedir(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

# CCPP framework imports
from framework_env import CCPPFrameworkEnv
from fortran_tools import parse_fortran_file
from metadata_table import parse_metadata_file
from ccpp_capgen import find_associated_fortran_file
from ccpp_capgen import check_fortran_against_metadata, parse_scheme_files
from parse_tools import init_log, set_log_level
from parse_tools import register_fortran_ddt_name
from parse_tools import CCPPError, ParseInternalError

_LOGGER = init_log(os.path.basename(__file__))
_DUMMY_RUN_ENV = CCPPFrameworkEnv(_LOGGER, ndict={'host_files':'',
                                                    'scheme_files':'',
                                                    'suites':''})
_CCPP_FRAMEWORK_DDT_TYPES = ["ccpp_hash_table_t",
                             "ccpp_hashable_t",
                             "ccpp_hashable_char_t",
                             "ccpp_constituent_prop_ptr_t"]



def find_files_to_compare(directory):
    metadata_files = []
    for file in glob.glob(os.path.join(directory,'**','*.meta'), recursive=True):
        metadata_files.append(file)
    # end for
    return metadata_files

def compare_fortran_and_metadata(scheme_directory, run_env):
    ## Check for files
    metadata_files = find_files_to_compare(scheme_directory)
    # Pre-register base CCPP DDT types:
    for ddt_name in _CCPP_FRAMEWORK_DDT_TYPES:
        register_fortran_ddt_name(ddt_name)
    # end for
    # Perform checks
    parse_scheme_files(metadata_files, run_env, known_ddts=['ccpp_constituent_prop_ptr_t'])

def parse_command_line(arguments, description):
    """Parse command-line arguments"""
    parser = argparse.ArgumentParser(description=description,
                                     formatter_class=argparse.RawTextHelpFormatter)
    parser.add_argument("--directory", type=str, required=True,
                        metavar='top-level directory to analyze - REQUIRED',
                        help="""Full path to scheme directory""")
    parser.add_argument("--debug", action='store_true', default=False,
                        help="""turn on debug mode for additional verbosity""")
    pargs = parser.parse_args(arguments)
    return pargs

def _main_func():
    """Parse command line, then parse indicated host, scheme, and suite files.
    Finally, generate code to allow host model to run indicated CCPP suites."""
    pargs = parse_command_line(sys.argv[1:], __doc__)
    logger = _LOGGER
    if pargs.debug:
        set_log_level(logger, logging.DEBUG)
    else:
        set_log_level(logger, logging.INFO)
    # end if
    compare_fortran_and_metadata(pargs.directory, _DUMMY_RUN_ENV)
    print('All checks passed!')

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

