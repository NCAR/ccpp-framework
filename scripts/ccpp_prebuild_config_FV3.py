#!/usr/bin/env python

# CCPP prebuild config for GFDL Finite-Volume Cubed-Sphere Model (FV3) v1


###############################################################################
# Definitions                                                                 #
###############################################################################

# Add all files with metadata tables on the host model side,
# relative to basedir = top-level directory of host model
VARIABLE_DEFINITION_FILES = [
    'FV3/atmos_cubed_sphere/model/fv_arrays.F90',
    'FV3/gfsphysics/CCPP_layer/CCPP_typedefs.F90',
    'FV3/gfsphysics/GFS_layer/GFS_typedefs.F90',
    ]

# Add all physics scheme dependencies relative to basedir - note that these are all violations
# of the CCPP requirement to not use any external modules except Fortran standard modules!
SCHEME_FILES_DEPENDENCIES = [
    'ccpp/physics/physics/GFDL_parse_tracers.F90',
    'ccpp/physics/physics/calpreciptype.f90',
    'ccpp/physics/physics/date_def.f',
    'ccpp/physics/physics/funcphys.f90',
    'ccpp/physics/physics/gcycle.F90',
    'ccpp/physics/physics/gfs_phy_tracer_config.f',
    'ccpp/physics/physics/gocart_tracer_config_stub.f',
    'ccpp/physics/physics/h2o_def.f',
    'ccpp/physics/physics/h2ointerp.f90',
    'ccpp/physics/physics/iounitdef.f',
    'ccpp/physics/physics/machine.F', # DH should this be in FV3/gfsphysics/physics/physparam.f ?
    'ccpp/physics/physics/mersenne_twister.f',
    'ccpp/physics/physics/mfpbl.f',
    'ccpp/physics/physics/module_bfmicrophysics.f',
    'ccpp/physics/physics/module_nst_model.f90',
    'ccpp/physics/physics/module_nst_parameters.f90',
    'ccpp/physics/physics/module_nst_water_prop.f90',
#    'ccpp/physics/physics/module_mp_thompson_hrrr_radar.F90',
#    'ccpp/physics/physics/module_mp_thompson_hrrr.F90',
    'ccpp/physics/physics/namelist_soilveg.f',
    'ccpp/physics/physics/num_parthds.F',
    'ccpp/physics/physics/ozne_def.f',
    'ccpp/physics/physics/ozinterp.f90',
    'ccpp/physics/physics/physcons.F90',
    'ccpp/physics/physics/physparam.f', # DH should this be in FV3/gfsphysics/physics/physparam.f ?
    'ccpp/physics/physics/radcons.f90',
    'ccpp/physics/physics/radiation_aerosols.f',
    'ccpp/physics/physics/radiation_astronomy.f',
    'ccpp/physics/physics/radiation_clouds.f',
    'ccpp/physics/physics/radiation_gases.f',
    'ccpp/physics/physics/radiation_surface.f',
    'ccpp/physics/physics/radlw_datatb.f',
    'ccpp/physics/physics/radlw_param.f',
    'ccpp/physics/physics/radsw_datatb.f',
    'ccpp/physics/physics/radsw_param.f',
    'ccpp/physics/physics/rascnvv2.f',
    'ccpp/physics/physics/sfcsub.F',
    'ccpp/physics/physics/sflx.f',
    'ccpp/physics/physics/set_soilveg.f',
    'ccpp/physics/physics/surface_perturbation.F90',
    'ccpp/physics/physics/tridi.f',
    'ccpp/physics/physics/wam_f107_kp_mod.f90',
    # stochastic physics
    'ccpp/physics/stochastic_physics/spectral_layout.f',
    'ccpp/physics/stochastic_physics/stochy_gg_def.f',
    'ccpp/physics/stochastic_physics/stochy_resol_def.f',
    'ccpp/physics/stochastic_physics/stochy_layout_lag.f',
    'ccpp/physics/stochastic_physics/four_to_grid_stochy.F',
    'ccpp/physics/stochastic_physics/glats_stochy.f',
    'ccpp/physics/stochastic_physics/sumfln_stochy.f',
    'ccpp/physics/stochastic_physics/gozrineo_stochy.f',
    'ccpp/physics/stochastic_physics/get_ls_node_stochy.f',
    'ccpp/physics/stochastic_physics/get_lats_node_a_stochy.f',
    'ccpp/physics/stochastic_physics/setlats_a_stochy.f',
    'ccpp/physics/stochastic_physics/setlats_lag_stochy.f',
    'ccpp/physics/stochastic_physics/epslon_stochy.f',
    'ccpp/physics/stochastic_physics/getcon_lag_stochy.f',
    'ccpp/physics/stochastic_physics/pln2eo_stochy.f',
    'ccpp/physics/stochastic_physics/dozeuv_stochy.f',
    'ccpp/physics/stochastic_physics/dezouv_stochy.f',
    'ccpp/physics/stochastic_physics/getcon_spectral.F90',
    'ccpp/physics/stochastic_physics/stochy_namelist_def.F90',
    'ccpp/physics/stochastic_physics/compns_stochy.F90',
    'ccpp/physics/stochastic_physics/stochy_internal_state_mod.F90',
    'ccpp/physics/stochastic_physics/stochy_patterngenerator.F90',
    'ccpp/physics/stochastic_physics/stochy_data_mod.F90',
    'ccpp/physics/stochastic_physics/get_stochy_pattern.F90',
    'ccpp/physics/stochastic_physics/initialize_spectral_mod.F90',
    'ccpp/physics/stochastic_physics/stochy_ccpp.F90',
    # derived data type definitions
    'FV3/gfsphysics/GFS_layer/GFS_typedefs.F90',
    'FV3/gfsphysics/CCPP_layer/CCPP_typedefs.F90',
    ]

# Add all physics scheme files relative to basedir
SCHEME_FILES = {
    # Relative path to source (from where ccpp_prebuild.py is called) : [ list of categories in which scheme may be called ]
    'ccpp/physics/physics/GFS_DCNV_generic.f90'              : [ 'slow_physics' ],
#    'ccpp/physics/physics/GFS_MP_generic_pre.f90'            : [ 'slow_physics' ],
    'ccpp/physics/physics/GFS_MP_generic_post.F90'           : [ 'slow_physics' ],
    'ccpp/physics/physics/GFS_PBL_generic.f90'               : [ 'slow_physics' ],
    'ccpp/physics/physics/GFS_SCNV_generic.f90'              : [ 'slow_physics' ],
    'ccpp/physics/physics/GFS_debug.F90'                     : [ 'slow_physics' ],
    'ccpp/physics/physics/GFS_phys_time_vary.fv3.f90'        : [ 'slow_physics' ],
    'ccpp/physics/physics/GFS_rad_time_vary.fv3.f90'         : [ 'slow_physics' ],
    'ccpp/physics/physics/GFS_rrtmg_post.F90'                : [ 'slow_physics' ],
    'ccpp/physics/physics/GFS_rrtmg_pre.F90'                 : [ 'slow_physics' ],
    'ccpp/physics/physics/GFS_rrtmg_setup.F90'               : [ 'slow_physics' ],
    'ccpp/physics/physics/GFS_stochastics.F90'               : [ 'slow_physics' ],
    'ccpp/physics/physics/GFS_suite_interstitial.F90'        : [ 'slow_physics' ],
    'ccpp/physics/physics/GFS_surface_generic.f90'           : [ 'slow_physics' ],
    'ccpp/physics/physics/GFS_surface_loop_control.F90'      : [ 'slow_physics' ],
    'ccpp/physics/physics/GFS_time_vary_pre.fv3.f90'         : [ 'slow_physics' ],
    'ccpp/physics/physics/GFS_zhao_carr_pre.f90'             : [ 'slow_physics' ],
    'ccpp/physics/physics/GFS_gfdlmp_pre.f90'                : [ 'slow_physics' ],
    'ccpp/physics/physics/cnvc90.f'                          : [ 'slow_physics' ],
    'ccpp/physics/physics/dcyc2.f'                           : [ 'slow_physics' ],
    'ccpp/physics/physics/get_prs_fv3.f90'                   : [ 'slow_physics' ],
    'ccpp/physics/physics/gfdl_cloud_microphys.F90'          : [ 'slow_physics' ],
    'ccpp/physics/physics/gfdl_cloud_microphys_pre_post.F90' : [ 'slow_physics' ],
    'ccpp/physics/physics/gfdl_fv_sat_adj.F90'               : [ 'fast_physics' ],
    'ccpp/physics/physics/gfdl_fv_sat_adj_pre.F90'           : [ 'fast_physics' ],
    'ccpp/physics/physics/gscond.f'                          : [ 'slow_physics' ],
    'ccpp/physics/physics/gwdc.f'                            : [ 'slow_physics' ],
    'ccpp/physics/physics/gwdps.f'                           : [ 'slow_physics' ],
    'ccpp/physics/physics/h2ophys.f'                         : [ 'slow_physics' ],
    'ccpp/physics/physics/samfdeepcnv.f'                     : [ 'slow_physics' ],
    'ccpp/physics/physics/samfshalcnv.f'                     : [ 'slow_physics' ],
    'ccpp/physics/physics/moninedmf.f'                       : [ 'slow_physics' ],
#    'ccpp/physics/physics/mp_thompson_hrrr.F90'              : [ 'slow_physics' ],
    'ccpp/physics/physics/ozphys.f'                          : [ 'slow_physics' ],
    'ccpp/physics/physics/precpd.f'                          : [ 'slow_physics' ],
    'ccpp/physics/physics/radlw_main.f'                      : [ 'slow_physics' ],
    'ccpp/physics/physics/radsw_main.f'                      : [ 'slow_physics' ],
    'ccpp/physics/physics/rayleigh_damp.f'                   : [ 'slow_physics' ],
    'ccpp/physics/physics/rrtmg_lw_post.F90'                 : [ 'slow_physics' ],
    'ccpp/physics/physics/rrtmg_lw_pre.F90'                  : [ 'slow_physics' ],
    'ccpp/physics/physics/rrtmg_sw_post.F90'                 : [ 'slow_physics' ],
    'ccpp/physics/physics/rrtmg_sw_pre.F90'                  : [ 'slow_physics' ],
    'ccpp/physics/physics/sfc_diag.f'                        : [ 'slow_physics' ],
    'ccpp/physics/physics/sfc_diff.f'                        : [ 'slow_physics' ],
    'ccpp/physics/physics/sfc_drv.f'                         : [ 'slow_physics' ],
    'ccpp/physics/physics/sfc_nst.f'                         : [ 'slow_physics' ],
    'ccpp/physics/physics/sfc_sice.f'                        : [ 'slow_physics' ],
    # stochastic physics
    'ccpp/physics/stochastic_physics/stochastic_physics.F90' : [ 'slow_physics' ],
    }

# Auto-generated makefile/cmakefile snippets that contain all schemes
SCHEMES_MAKEFILE = 'ccpp/physics/CCPP_SCHEMES.mk'
SCHEMES_CMAKEFILE = 'ccpp/physics/CCPP_SCHEMES.cmake'

# CCPP host cap in which to insert the ccpp_field_add statements;
# determines the directory to place ccpp_{modules,fields}.inc
TARGET_FILES = [
    'FV3/atmos_cubed_sphere/driver/fvGFS/atmosphere.F90',
    'FV3/ipd/IPD_CCPP_Driver.F90',
    ]

# Auto-generated makefile/cmakefile snippets that contain all caps
CAPS_MAKEFILE = 'ccpp/physics/CCPP_CAPS.mk'
CAPS_CMAKEFILE = 'ccpp/physics/CCPP_CAPS.cmake'

# Directory where to put all auto-generated physics caps
CAPS_DIR = 'ccpp/physics/physics'

# Optional arguments - only required for schemes that use
# optional arguments. ccpp_prebuild.py will throw an exception
# if it encounters a scheme subroutine with optional arguments
# if no entry is made here. Possible values are: 'all', 'none',
# or a list of standard_names: [ 'var1', 'var3' ].
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
            'cloud_optical_depth_weighted',
            'cloud_optical_depth_layers_678',
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
            'cloud_optical_depth_weighted',
            'cloud_optical_depth_layers_678',
            ],
        },
    'mp_thompson_hrrr' : {
        'mp_thompson_hrrr_init' : [
            'tendency_of_water_friendly_surface_aerosols_at_surface',
            'water_friendly_aerosol_number_concentration',
            'ice_friendly_aerosol_number_concentration',
            ],
        'mp_thompson_hrrr_run' : [
            'cloud_droplet_number_concentration_updated_by_physics',
            'water_friendly_aerosol_number_concentration_updated_by_physics',
            'ice_friendly_aerosol_number_concentration_updated_by_physics',
            'tendency_of_water_friendly_surface_aerosols_at_surface',
            'mean_effective_radius_for_liquid_cloud',
            'mean_effective_radius_for_ice_cloud',
            'mean_effective_radius_for_snow_flake',
            ],
        },
    #'subroutine_name_1' : 'all',
    #'subroutine_name_2' : 'none',
    #'subroutine_name_2' : [ 'var1', 'var3'],
    }

# Names of Fortran include files in the host model cap (do not change);
# both files will be written to the directory of each target file
MODULE_INCLUDE_FILE = 'ccpp_modules_{category}.inc'
FIELDS_INCLUDE_FILE = 'ccpp_fields_{category}.inc'

# HTML document containing the model-defined CCPP variables
HTML_VARTABLE_FILE = 'ccpp/physics/CCPP_VARIABLES_FV3.html'

# LaTeX document containing the provided vs requested CCPP variables
LATEX_VARTABLE_FILE = 'ccpp/framework/doc/DevelopersGuide/CCPP_VARIABLES_FV3.tex'


###############################################################################
# Template code to generate include files                                     #
###############################################################################

# Name of the CCPP data structure in the host model cap;
# in the case of FV3, this is a 2-dimensional array with
# the number of blocks as the first and the number of
# OpenMP threads as the second dimension; nb is the loop
# index for the current block, nt for the current thread
CCPP_DATA_STRUCTURE = 'cdata'

# Modules to load for auto-generated ccpp_field_add code
# in the host model cap (e.g. error handling)
MODULE_USE_TEMPLATE_HOST_CAP = \
'''
use ccpp_api, only: ccpp_error
'''

# Modules to load for auto-generated ccpp_field_get code
# in the physics scheme cap (e.g. derived data types)
MODULE_USE_TEMPLATE_SCHEME_CAP = \
'''
       use machine, only: kind_grid, kind_phys
       use module_radlw_parameters, only: sfcflw_type, topflw_type
       use module_radsw_parameters, only: cmpfsw_type, sfcfsw_type, topfsw_type
       use CCPP_typedefs, only: CCPP_interstitial_type
       use GFS_typedefs, only: GFS_statein_type,  GFS_stateout_type,    &
                               GFS_sfcprop_type,  GFS_coupling_type,    &
                               GFS_control_type,  GFS_grid_type,        &
                               GFS_tbd_type,      GFS_cldprop_type,     &
                               GFS_radtend_type,  GFS_diag_type,        &
                               GFS_data_type,     GFS_interstitial_type
'''
