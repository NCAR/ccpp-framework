#! /usr/bin/env python
"""
Test script to check ability to parse and generate caps.
"""

import os.path
if __name__ == '__main__' and __package__ is None:
    import sys
    tdir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
    sdir = os.path.join(tdir, "scripts")
    if not os.path.exists(sdir):
        raise ImportError("Cannot find scripts directory")
    # End if
    sys.path.append(sdir)
# End if
import convert_metadata
from fortran_parser import parse_fortran_file

########################################################################

if __name__ == "__main__":
    pdir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
    sdir = os.path.join(os.path.dirname(pdir), 'ccpp-physics')
    tfilenames = [os.path.join('physics', 'GFS_DCNV_generic.F90'),
    os.path.join('physics', 'GFS_MP_generic.F90'),
    os.path.join('physics', 'GFS_PBL_generic.F90'),
    os.path.join('physics', 'GFS_SCNV_generic.F90'),
    os.path.join('physics', 'GFS_debug.F90'),
    os.path.join('physics', 'GFS_phys_time_vary.fv3.F90'),
    os.path.join('physics', 'GFS_rad_time_vary.fv3.F90'),
    os.path.join('physics', 'GFS_rrtmg_post.F90'),
    os.path.join('physics', 'GFS_rrtmg_pre.F90'),
    os.path.join('physics', 'GFS_rrtmg_setup.F90'),
    os.path.join('physics', 'GFS_stochastics.F90'),
    os.path.join('physics', 'GFS_suite_interstitial.F90'),
    os.path.join('physics', 'GFS_surface_generic.F90'),
    os.path.join('physics', 'GFS_surface_loop_control.F90'),
    os.path.join('physics', 'GFS_time_vary_pre.fv3.F90'),
    os.path.join('physics', 'cnvc90.f'),
    os.path.join('physics', 'cs_conv.F90'),
    os.path.join('physics', 'dcyc2.f'),
    os.path.join('physics', 'gcm_shoc.F90'),
    os.path.join('physics', 'get_prs_fv3.F90'),
    os.path.join('physics', 'gfdl_cloud_microphys.F90'),
    os.path.join('physics', 'gfdl_fv_sat_adj.F90'),
    os.path.join('physics', 'gscond.f'),
    os.path.join('physics', 'gwdc.f'),
    os.path.join('physics', 'gwdps.f'),
    os.path.join('physics', 'h2ophys.f'),
    os.path.join('physics', 'samfdeepcnv.f'),
    os.path.join('physics', 'samfshalcnv.f'),
    os.path.join('physics', 'cu_gf_driver_pre.F90'),
    os.path.join('physics', 'cu_gf_driver.F90'),
    os.path.join('physics', 'cu_gf_driver_post.F90'),
    os.path.join('physics', 'moninedmf.f'),
    os.path.join('physics', 'moninshoc.f'),
    os.path.join('physics', 'satmedmfvdif.F90'),
    os.path.join('physics', 'mp_thompson_hrrr_pre.F90'),
    os.path.join('physics', 'mp_thompson_hrrr.F90'),
    os.path.join('physics', 'mp_thompson_hrrr_post.F90'),
    os.path.join('physics', 'ozphys.f'),
    os.path.join('physics', 'ozphys_2015.f'),
    os.path.join('physics', 'precpd.f'),
    os.path.join('physics', 'radlw_main.f'),
    os.path.join('physics', 'radsw_main.f'),
    os.path.join('physics', 'rayleigh_damp.f'),
    os.path.join('physics', 'rrtmg_lw_post.F90'),
    os.path.join('physics', 'rrtmg_lw_pre.F90'),
    os.path.join('physics', 'rrtmg_sw_post.F90'),
    os.path.join('physics', 'rrtmg_sw_pre.F90'),
    os.path.join('physics', 'sfc_diag.f'),
    os.path.join('physics', 'sfc_diag_post.F90'),
    os.path.join('physics', 'sfc_drv_ruc.F90'),
    os.path.join('physics', 'sfc_diff.f'),
    os.path.join('physics', 'sfc_drv.f'),
    os.path.join('physics', 'sfc_nst.f'),
    os.path.join('physics', 'sfc_sice.f'),
    os.path.join('stochastic_physics', 'stochastic_physics.F90'),
    os.path.join('physics', 'memcheck.F90'),
    os.path.join('physics', 'GFS_suite_init_finalize_test.F90')]
    for tfile in tfilenames:
        file = os.path.join(pdir, 'tests', 'cap_tests', os.path.basename(tfile))
        if not os.path.exists(file):
            infile = os.path.join(sdir, tfile)
            if not os.path.exists(infile):
                print("WARNING: Cannot find '{}'".format(infile))
            else:
                convert_metadata.convert_file(infile, file)
            # End if
        # End if
        if os.path.exists(file):
            mh = parse_fortran_file(file)
            print("metadata headers from {}".format(file))
            print mh
        # End if
    # End for
# End if __main__
