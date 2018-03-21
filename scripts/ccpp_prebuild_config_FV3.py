#!/usr/bin/env python

# CCPP prebuild config for GFDL Finite-Volume Cubed-Sphere Model (FV3) v0.0


###############################################################################
# Definitions                                                                 #
###############################################################################

# Relative to basedir defined in ccpp_prebuild.py
VARIABLE_DEFINITION_FILES = [
    'FV3/gfsphysics/GFS_layer/GFS_typedefs.F90',
    'FV3/gfsphysics/physics/physcons.f90',
    ]

# Location of scheme_files relative to basedir defined in ccpp_prebuild.py
SCHEME_FILES = [
    'FV3/gfsphysics/physics/GFS_DCNV_generic.f90',
    'FV3/gfsphysics/physics/GFS_MP_generic_post.f90',
    'FV3/gfsphysics/physics/GFS_MP_generic_pre.f90',
    'FV3/gfsphysics/physics/GFS_PBL_generic.f90',
    'FV3/gfsphysics/physics/GFS_SCNV_generic.f90',
    'FV3/gfsphysics/physics/GFS_calpreciptype.f90',
    'FV3/gfsphysics/physics/GFS_debug.f90',
    'FV3/gfsphysics/physics/GFS_phys_time_vary.f90',
    'FV3/gfsphysics/physics/GFS_rad_time_vary.f90',
    'FV3/gfsphysics/physics/GFS_rrtmg_post.F90',
    'FV3/gfsphysics/physics/GFS_rrtmg_pre.F90',
    'FV3/gfsphysics/physics/GFS_stochastics.f90',
    'FV3/gfsphysics/physics/GFS_suite_interstitial.ccpp.f90',
    'FV3/gfsphysics/physics/GFS_surface_generic.f90',
    'FV3/gfsphysics/physics/GFS_surface_loop_control.f',
    'FV3/gfsphysics/physics/GFS_zhao_carr_pre.f90',
    'FV3/gfsphysics/physics/cnvc90.f',
    'FV3/gfsphysics/physics/dcyc2.f',
    'FV3/gfsphysics/physics/get_prs_fv3.f90',
    'FV3/gfsphysics/physics/gscond.f',
    'FV3/gfsphysics/physics/gwdc.f',
    'FV3/gfsphysics/physics/gwdps.f',
    'FV3/gfsphysics/physics/mfdeepcnv.f',
    'FV3/gfsphysics/physics/mfshalcnv.f',
    'FV3/gfsphysics/physics/moninedmf.f',
    'FV3/gfsphysics/physics/ozphys.f',
    'FV3/gfsphysics/physics/precpd.f',
    'FV3/gfsphysics/physics/radlw_main.f',
    'FV3/gfsphysics/physics/radsw_main.f',
    'FV3/gfsphysics/physics/rayleigh_damp.f',
    'FV3/gfsphysics/physics/rrtmg_lw_post.F90',
    'FV3/gfsphysics/physics/rrtmg_lw_pre.F90',
    'FV3/gfsphysics/physics/rrtmg_sw_post.F90',
    'FV3/gfsphysics/physics/rrtmg_sw_pre.F90',
    'FV3/gfsphysics/physics/sfc_diag.f',
    'FV3/gfsphysics/physics/sfc_diff.f',
    'FV3/gfsphysics/physics/sfc_drv.f',
    'FV3/gfsphysics/physics/sfc_nst.f',
    'FV3/gfsphysics/physics/sfc_sice.f',
    ]

# Relative to basedir defined in ccpp_prebuild.py
SCHEMES_MAKEFILE = 'FV3/gfsphysics/CCPP_SCHEMES.mk'

# Relative to basedir defined in ccpp_prebuild.py
TARGET_FILES = [
    'FV3/gfsphysics/IPD_layer/IPD_CCPP_Driver.F90',
    ]

# Relative to basedir
CAPS_MAKEFILE = 'FV3/gfsphysics/CCPP_CAPS.mk'
CAPS_DIR = 'FV3/gfsphysics/physics'

# Optional arguments - only required for schemes that use optional arguments. This script will throw
# an exception if it encounters a scheme subroutine with optional arguments if no entry is made in
# the following dictionary. Valid values are 'all', 'none' or a list of arguments: [ 'var1', 'var3' ].
OPTIONAL_ARGUMENTS = {
    'rrtmg_sw' : {
        'rrtmg_sw_run' : [
            'tendency_of_air_temperature_due_to_shortwave_heating_assuming_clear_sky_on_radiation_time_step',
            'components_of_surface_downward_shortwave_fluxes',
            'cloud_liquid_water_path',
            'mean_effective_radius_for_liquid_cloud',
            'cloud_ice_water_path',
            'mean_effective_radius_for_ice_cloud',
            'cloud_rain_water_path',
            'mean_effective_radius_for_rain_drop',
            'cloud_snow_water_path',
            'mean_effective_radius_for_snow_flake',
            ],
        },
    'rrtmg_lw' : {
        'rrtmg_lw_run' : [
            'tendency_of_air_temperature_due_to_longwave_heating_assuming_clear_sky_on_radiation_time_step',
            'cloud_liquid_water_path',
            'mean_effective_radius_for_liquid_cloud',
            'cloud_ice_water_path',
            'mean_effective_radius_for_ice_cloud',
            'cloud_rain_water_path',
            'mean_effective_radius_for_rain_drop',
            'cloud_snow_water_path',
            'mean_effective_radius_for_snow_flake',
            ],
        },
    #'subroutine_name_1' : 'all',
    #'subroutine_name_2' : 'none',
    #'subroutine_name_2' : [ 'var1', 'var3'],
    }

# No path needed - will be created in directory of target files defined above
MODULE_INCLUDE_FILE = 'ccpp_modules.inc'
FIELDS_INCLUDE_FILE = 'ccpp_fields.inc'


###############################################################################
# Template code to generate include files                                     #
###############################################################################

# Modules to load for auto-generated ccpp_field_add code (e.g. error handling)
MODULE_USE_TEMPLATE_HOST_CAP = \
'''
use ccpp_errors, only: ccpp_error
'''

# Name of the CCPP data structure in the host model cap
CCPP_DATA_STRUCTURE = 'cdata_block(nb,nt)'

# Modules to load for auto-generated ccpp_field_add code (e.g. error handling)
MODULE_USE_TEMPLATE_SCHEME_CAP = \
'''
       use machine, only: kind_phys
       use module_radlw_parameters, only: sfcflw_type, topflw_type
       use module_radsw_parameters, only: cmpfsw_type, sfcfsw_type, topfsw_type
       use GFS_typedefs, only: GFS_statein_type,  GFS_stateout_type,    &
                               GFS_sfcprop_type,  GFS_sfccycle_type,    &
                               GFS_coupling_type, GFS_control_type,     &
                               GFS_grid_type,     GFS_tbd_type,         &
                               GFS_cldprop_type,  GFS_radtend_type,     &
                               GFS_diag_type,     GFS_interstitial_type
'''
