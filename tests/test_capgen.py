#! /usr/bin/env python
"""
Test script to check ability to parse and generate caps.
"""

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
import convert_metadata
from fortran_tools import parse_fortran_file
from parse_tools import register_fortran_ddt_name

arg_table_re = re.compile(r"[\s]*!.*section.*arg_table_")

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
    for dir in ['physics', 'stochastic_physics']:
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
            file = os.path.join(pdir, os.path.basename(tfile))
            if not os.path.exists(file):
                infile = tfile
                if not os.path.exists(infile):
                    print("WARNING: Cannot find '{}'".format(infile))
                else:
                    convert_metadata.convert_file(infile, file)
                # End if
            # End if
            if os.path.exists(file):
                mh = parse_fortran_file(file)
                print("{} metadata headers parsed in {}".format(len(mh), file))
                total_headers = total_headers + len(mh)
            # End if
        except ValueError as ve:
            print("{}: {}".format(infile, ve))
        # End except
    # End for
    print("Found {} total metadata headers".format(total_headers))
# End if __main__
