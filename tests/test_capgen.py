#! /usr/bin/env python
"""
Test script to check ability to parse and generate caps.
"""

# Python library imports
import os.path
if __name__ == '__main__' and __package__ is None:
    import sys
    import os
    tdir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
    sdir = os.path.join(tdir, "scripts")
    if not os.path.exists(sdir):
        raise ImportError("Cannot find scripts directory")
    # End if
    sys.path.append(sdir)
# End if
import re
import logging
# CCPP framework imports
import convert_metadata
from parse_tools import register_fortran_ddt_name, init_log, set_log_level
from metadata_table import MetadataHeader

arg_table_re = re.compile(r"(?i)[\s]*!.*section.*arg_table_")

## Init this now so that all Exceptions can be trapped
logger = init_log('ccpp_capgen')
set_log_level(logger, logging.INFO)

########################################################################

if __name__ == "__main__":
    if len(sys.argv) > 2:
        pdir = sys.argv[2]
    else:
        pdir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
        pdir = os.path.join(pdir, 'tests', 'cap_tests')
        if not os.path.exists(pdir):
            pdir = os.getcwd()
        # End if
  # End if
    if len(sys.argv) > 1:
        sdir = sys.argv[1]
    else:
        sdir = os.path.join(os.path.dirname(pdir), 'ccpp-physics')
    # End if
    print("Converting physics files from {} with output to {}".format(sdir, pdir))
# XXgoldyXX: v debug only
    # Temporary DDT registration
    register_fortran_ddt_name('GFS_control_type')
    register_fortran_ddt_name('GFS_statein_type')
    register_fortran_ddt_name('GFS_stateout_type')
    register_fortran_ddt_name('GFS_sfcprop_type')
    register_fortran_ddt_name('GFS_coupling_type')
    register_fortran_ddt_name('GFS_grid_type')
    register_fortran_ddt_name('GFS_tbd_type')
    register_fortran_ddt_name('GFS_cldprop_type')
    register_fortran_ddt_name('GFS_radtend_type')
    register_fortran_ddt_name('GFS_diag_type')
    register_fortran_ddt_name('GFS_interstitial_type')
    register_fortran_ddt_name('GFS_data_type')
    register_fortran_ddt_name('cmpfsw_type')
    register_fortran_ddt_name('topflw_type')
    register_fortran_ddt_name('sfcflw_type')
    register_fortran_ddt_name('proflw_type')
    register_fortran_ddt_name('topfsw_type')
    register_fortran_ddt_name('sfcfsw_type')
    register_fortran_ddt_name('profsw_type')
    register_fortran_ddt_name('CCPP_interstitial_type')
# XXgoldyXX: ^ debug only
    tfilenames = list()
    # Find files with arg tables
    for dir in ['physics', 'stochastic_physics', 'GFS_layer']:
        if not os.path.exists(os.path.join(sdir, dir)):
            continue
        # End if
        for file in os.listdir(os.path.join(sdir, dir)):
            has_arg_table = False
            pathname = os.path.join(sdir, dir, file)
            if os.path.isfile(pathname):
                with open(pathname, 'r') as infile:
                    preamble = True
                    for line in infile:
                        if preamble:
                            if line.strip().lower() == 'contains':
                                preamble = False
                            # End if
                        # End if
                        if arg_table_re.match(line) is not None:
                            has_arg_table = True
                            break
                        # End if
                    # End for
                # End with
            # End if
            if has_arg_table:
                tfilenames.append(pathname)
            # End if
        # End for
    # End for
    print("Found {} files with arg tables".format(len(tfilenames)))
    total_headers = 0
    for tfile in tfilenames:
        try:
            tbase = os.path.basename(tfile)
            file = os.path.join(pdir, tbase)
            mbase = "{}.md".format('.'.join(tbase.split('.')[:-1]))
            mdfile = os.path.join(pdir, mbase)
            if not os.path.exists(file):
                infile = tfile
                if not os.path.exists(infile):
                    print("WARNING: Cannot find '{}'".format(infile))
                else:
                    convert_metadata.convert_file(infile, file, mdfile, logger)
                # End if
            # End if
            if os.path.exists(mdfile):
                mh = MetadataHeader.parse_metadata_file(mdfile)
                print("{} metadata headers parsed in {}".format(len(mh), mdfile))
                total_headers = total_headers + len(mh)
            else:
                print("{} not found!".format(mdfile))
            # End if
        except ValueError as ve:
            print("{}: {}".format(infile, ve))
        # End except
    # End for
    print("Found {} total metadata headers".format(total_headers))
# End if __main__
