#!/usr/bin/env python

# Standard modules
import os
import sys

# DH*
# CONSISTENCY CHECK BETWEEN OPTIONAL ARGUMENTS IN THE METADATA TABLE AND IN
# THE ACTUAL ARGUMENT LIST?
# FURTHER CONSISTENCY CHECKS OF RANKS, TYPE, INTENT
# *DH

# Local modules
from common import execute
from metadata_parser import merge_metadata_dicts, parse_scheme_tables, parse_variable_tables

###############################################################################
# Definitions                                                                 #
###############################################################################

variable_definition_files = [
    'FV3/gfsphysics/GFS_layer/GFS_typedefs.F90',
    'FV3/gfsphysics/physics/physcons.f90',
    ]

# DH* NEED THIS ONE?
physicsdirs = [
    'FV3/gfsphysics/physics',
    ]

# DH* separator work physics after big merge and after fixing longname conflicts
scheme_files = [
    '/Users/dom.heinzeller/work/gmtb-fv3_bigmerge/gmtb-fv3-longnamefix/FV3/gfsphysics/physics/GFS_DCNV_generic.f90',
    '/Users/dom.heinzeller/work/gmtb-fv3_bigmerge/gmtb-fv3-longnamefix/FV3/gfsphysics/physics/GFS_MP_generic_post.f90',
    '/Users/dom.heinzeller/work/gmtb-fv3_bigmerge/gmtb-fv3-longnamefix/FV3/gfsphysics/physics/GFS_MP_generic_pre.f90',
    '/Users/dom.heinzeller/work/gmtb-fv3_bigmerge/gmtb-fv3-longnamefix/FV3/gfsphysics/physics/GFS_PBL_generic.f90',
    '/Users/dom.heinzeller/work/gmtb-fv3_bigmerge/gmtb-fv3-longnamefix/FV3/gfsphysics/physics/GFS_RRTMG_post.f90',
    '/Users/dom.heinzeller/work/gmtb-fv3_bigmerge/gmtb-fv3-longnamefix/FV3/gfsphysics/physics/GFS_RRTMG_pre.f90',
    '/Users/dom.heinzeller/work/gmtb-fv3_bigmerge/gmtb-fv3-longnamefix/FV3/gfsphysics/physics/GFS_SCNV_generic.f90',
    '/Users/dom.heinzeller/work/gmtb-fv3_bigmerge/gmtb-fv3-longnamefix/FV3/gfsphysics/physics/GFS_calpreciptype.f90',
    '/Users/dom.heinzeller/work/gmtb-fv3_bigmerge/gmtb-fv3-longnamefix/FV3/gfsphysics/physics/GFS_rad_time_vary.f90',
    '/Users/dom.heinzeller/work/gmtb-fv3_bigmerge/gmtb-fv3-longnamefix/FV3/gfsphysics/physics/GFS_radlw_post.f90',
    '/Users/dom.heinzeller/work/gmtb-fv3_bigmerge/gmtb-fv3-longnamefix/FV3/gfsphysics/physics/GFS_radlw_pre.f90',
    '/Users/dom.heinzeller/work/gmtb-fv3_bigmerge/gmtb-fv3-longnamefix/FV3/gfsphysics/physics/GFS_radsw_post.f90',
    '/Users/dom.heinzeller/work/gmtb-fv3_bigmerge/gmtb-fv3-longnamefix/FV3/gfsphysics/physics/GFS_radsw_pre.f90',
    '/Users/dom.heinzeller/work/gmtb-fv3_bigmerge/gmtb-fv3-longnamefix/FV3/gfsphysics/physics/GFS_suite_interstitial.f90',
    '/Users/dom.heinzeller/work/gmtb-fv3_bigmerge/gmtb-fv3-longnamefix/FV3/gfsphysics/physics/GFS_suite_setup.f90',
    '/Users/dom.heinzeller/work/gmtb-fv3_bigmerge/gmtb-fv3-longnamefix/FV3/gfsphysics/physics/GFS_surface_generic.f90',
    '/Users/dom.heinzeller/work/gmtb-fv3_bigmerge/gmtb-fv3-longnamefix/FV3/gfsphysics/physics/GFS_surface_loop_control.f',
    '/Users/dom.heinzeller/work/gmtb-fv3_bigmerge/gmtb-fv3-longnamefix/FV3/gfsphysics/physics/GFS_zhao_carr_pre.f90',
    '/Users/dom.heinzeller/work/gmtb-fv3_bigmerge/gmtb-fv3-longnamefix/FV3/gfsphysics/physics/cnvc90.f',
    '/Users/dom.heinzeller/work/gmtb-fv3_bigmerge/gmtb-fv3-longnamefix/FV3/gfsphysics/physics/dcyc2.f',
    '/Users/dom.heinzeller/work/gmtb-fv3_bigmerge/gmtb-fv3-longnamefix/FV3/gfsphysics/physics/get_prs_fv3.f90',
    '/Users/dom.heinzeller/work/gmtb-fv3_bigmerge/gmtb-fv3-longnamefix/FV3/gfsphysics/physics/gscond.f',
    '/Users/dom.heinzeller/work/gmtb-fv3_bigmerge/gmtb-fv3-longnamefix/FV3/gfsphysics/physics/gwdc.f',
    '/Users/dom.heinzeller/work/gmtb-fv3_bigmerge/gmtb-fv3-longnamefix/FV3/gfsphysics/physics/gwdps.f',
    '/Users/dom.heinzeller/work/gmtb-fv3_bigmerge/gmtb-fv3-longnamefix/FV3/gfsphysics/physics/mfdeepcnv.f',
    '/Users/dom.heinzeller/work/gmtb-fv3_bigmerge/gmtb-fv3-longnamefix/FV3/gfsphysics/physics/mfshalcnv.f',
    '/Users/dom.heinzeller/work/gmtb-fv3_bigmerge/gmtb-fv3-longnamefix/FV3/gfsphysics/physics/moninedmf.f',
    '/Users/dom.heinzeller/work/gmtb-fv3_bigmerge/gmtb-fv3-longnamefix/FV3/gfsphysics/physics/ozphys.f',
    '/Users/dom.heinzeller/work/gmtb-fv3_bigmerge/gmtb-fv3-longnamefix/FV3/gfsphysics/physics/precpd.f',
    '/Users/dom.heinzeller/work/gmtb-fv3_bigmerge/gmtb-fv3-longnamefix/FV3/gfsphysics/physics/radlw_main.f',
    '/Users/dom.heinzeller/work/gmtb-fv3_bigmerge/gmtb-fv3-longnamefix/FV3/gfsphysics/physics/radsw_main.f',
    '/Users/dom.heinzeller/work/gmtb-fv3_bigmerge/gmtb-fv3-longnamefix/FV3/gfsphysics/physics/rayleigh_damp.f',
    '/Users/dom.heinzeller/work/gmtb-fv3_bigmerge/gmtb-fv3-longnamefix/FV3/gfsphysics/physics/sfc_diag.f',
    '/Users/dom.heinzeller/work/gmtb-fv3_bigmerge/gmtb-fv3-longnamefix/FV3/gfsphysics/physics/sfc_diff.f',
    '/Users/dom.heinzeller/work/gmtb-fv3_bigmerge/gmtb-fv3-longnamefix/FV3/gfsphysics/physics/sfc_drv.f',
    '/Users/dom.heinzeller/work/gmtb-fv3_bigmerge/gmtb-fv3-longnamefix/FV3/gfsphysics/physics/sfc_nst.f',
    '/Users/dom.heinzeller/work/gmtb-fv3_bigmerge/gmtb-fv3-longnamefix/FV3/gfsphysics/physics/sfc_sice.f',
    ]

target_files = [
    'FV3/gfsphysics/IPD_layer/IPD_CCPP_Driver.F90',
    ]

# Optional arguments - only required for schemes that use optional arguments. This script will throw
# an exception if it encounters a scheme subroutine with optional arguments if no entry is made in
# the following dictionary. Valid values are 'all', 'none' or a list of arguments: [ 'var1', 'var3' ].
optional_arguments = {
    'module_radsw_main' : {
        'swrad_run' : [
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
    'module_radlw_main' : {
        'lwrad_run' : [
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

module_include_file = 'ccpp_modules.inc'

fields_include_file = 'ccpp_fields.inc'

###############################################################################
# Template code to generate include files                                     #
###############################################################################

# Modules to load for auto-generated ccpp_fields_add code (e.g. error handling)
MODULE_USE_TEMPLATE = \
"""
use mpi
use ccpp_errors, only: ccpp_error
"""

# call ccpp_fields_add(cdata, 'eastward_wind', c_loc(u), size(u), shape(u), ierr, 'm s-1')
# call ccpp_fields_add(cdata, 'IPD_Diag', '', c_loc(IPD_Diag), size(IPD_Diag), shape(IPD_Diag), ierr=ierr)
# {0} : standard_name
# {1} : local_name
# {2} : units
CCPP_FIELDS_ADD_TEMPLATE_STANDARD_VARIABLE_TYPE = \
"""
call ccpp_fields_add(cdata_block(nb), '{0}', {1}, ierr, '{2}')
if (ierr /= 0) then
    call ccpp_error('Unable to add field "{0}" to CCPP data structure')
    call MPI_FINALIZE(ierr)
    stop
end if
"""

CCPP_FIELDS_ADD_TEMPLATE_DERIVED_VARIABLE_TYPE_SCALAR = \
"""
call ccpp_fields_add(cdata_block(nb), '{0}', '', c_loc({1}), ierr)
if (ierr /= 0) then
    call ccpp_error('Unable to add field "{0}" to CCPP data structure')
    call MPI_FINALIZE(ierr)
    stop
end if
"""

CCPP_FIELDS_ADD_TEMPLATE_DERIVED_VARIABLE_TYPE_ARRAY = \
"""
call ccpp_fields_add(cdata_block(nb), '{0}', '', c_loc({1}), rank=size({1}), dims=shape({1}), ierr=ierr)
if (ierr /= 0) then
    call ccpp_error('Unable to add field "{0}" to CCPP data structure')
    call MPI_FINALIZE(ierr)
    stop
end if
"""

STANDARD_VARIABLE_TYPES = [ 'charater', 'integer', 'logical', 'real' ]

###############################################################################
# Functions and subroutines                                                   #
###############################################################################

def gather_variable_definitions():
    metadata_define = {}
    for variable_definition_file in variable_definition_files:
        metadata = parse_variable_tables(variable_definition_file)
        metadata_define = merge_metadata_dicts(metadata_define, metadata)
    return metadata_define

def collect_physics_subroutines():
    """Scan all Fortran source files in the physics directory for subroutines with argument tables."""
    metadata_request = {}
    for scheme_file in scheme_files:
        metadata = parse_scheme_tables(scheme_file)
        metadata_request = merge_metadata_dicts(metadata_request, metadata)
    return metadata_request

def check_optional_arguments(metadata_request):
    """Check if for each subroutine with optional arguments, an entry exists in the
    optional_arguments dictionary. This is required to generate the caps correctly
    and to assess whether the variable is required from the host model. Optional
    arguments that are not requested by the model as specified in the dictionary
    optional_arguments are deleted from the list of requested data individually
    for each subroutine."""

    success = True
    for var_name in sorted(metadata_request.keys()):
        # The notation metadata_request[var_name][:] is a convenient way to make a copy
        # of the metadata_request[var_name] list, which allows removing items as we go
        for var in metadata_request[var_name][:]:
            if var.optional in ['t', 'T']:
                for item in var.container.split(' '):
                    subitems = item.split('_')
                    if subitems[0] == 'MODULE':
                        module_name = '_'.join(subitems[1:])
                    elif subitems[0] == 'SUBROUTINE':
                        subroutine_name = '_'.join(subitems[1:])
                    else:
                        success = False
                        print 'Error, invalid identifier {0} in container value {1} of requested variable {2}'.format(
                                                                                 subitems[0], var.container, var_name)
                if not module_name in optional_arguments.keys() or not \
                        subroutine_name in optional_arguments[module_name].keys():
                    success = False
                    print 'Error, No entry found in optional_arguments dictionary for optional argument ' + \
                           '{0} to subroutine {1} in module {2}'.format(var_name, subroutine_name, module_name)
                if type(optional_arguments[module_name][subroutine_name]) is list:
                    if var_name in optional_arguments[module_name][subroutine_name]:
                        print "Optional argument {0} to subroutine {1} in module {2} is required, keep in list".format(
                                                                                var_name, subroutine_name, module_name)
                    else:
                        print "Optional argument {0} to subroutine {1} in module {2} is not required, remove from list".format(
                                                                                        var_name, subroutine_name, module_name)
                        metadata_request[var_name].remove(var)
                elif optional_arguments[module_name][subroutine_name] == 'all':
                    print "Optional argument {0} to subroutine {1} in module {2} is required, keep in list".format(
                                                                            var_name, subroutine_name, module_name)

        # If metadata_request[var_name] is now empty, i.e. the variable is not
        # requested at all by the model, remove the entry from metadata_request
        if not metadata_request[var_name]:
            del metadata_request[var_name]

    return (success, metadata_request)

def compare_metadata(metadata_define, metadata_request):
    """Compare the requested metadata to the defined one. For each requested entry, a
    single (i.e. non-ambiguous entry) must be present in the defined entries. Also ..."""
    success = True
    missing = 0
    modules = []
    metadata = {}
    for var_name in sorted(metadata_request.keys()):
        # Check if all requests for this variable are optional
        optional = True
        for var in metadata_request[var_name]:
            if var.optional in ['f', 'F']:
                optional = False
        # Check that variable is provided by the model
        if not var_name in metadata_define.keys():
            requested_by = ' & '.join(var.container for var in metadata_request[var_name])
            if optional:
                print 'WARNSFX, {0}, {1}'.format(var_name, requested_by)
                print 'Warning, optional variable {0} requested by {1} not provided by the model'.format(var_name, requested_by)
            else:
                success = False
                missing += 1
                print 'ERRSFX, {0}, {1}'.format(var_name, requested_by)
                print 'Error, variable {0} requested by {1} not provided by the model'.format(var_name, requested_by)
            continue
        # Check that an unambiguous target exists for this variable
        if len(metadata_define[var_name]) > 1:
            success = False
            requested_by = ' & '.join(var.container for var in metadata_request[var_name])
            provided_by = ' & '.join(var.container for var in metadata_define[var_name])
            print 'Error, variable {0} requested by {1} cannot be identified unambiguously. Multiple definitions in {2}'.format(var_name, requested_by, provided_by)
            continue
        # DH* MISSING - compliancy check
        if not metadata_request[var_name][0].compatible(metadata_define[var_name][0]):
            raise Exception('Incompatible entries in metadata for variable {0}:\n'.format(var_name) +\
                            'provided:  {0}\n'.format(metadata_define[var_name][0].print_debug()) +\
                            'requested: {0}'.format(metadata_request[var_name][0].print_debug()))

        # Construct the actual target variable and list of modules to use from the information in 'container'
        var = metadata_define[var_name][0]
        target = ''
        for item in var.container.split(' '):
            subitems = item.split('_')
            if subitems[0] == 'MODULE':
                modules.append('_'.join(subitems[1:]))
            elif subitems[0] == 'TYPE':
                # DH*
                pass
                #target += '{0}%'.format('_'.join(subitems[1:]))
                # *DH
            else:
                print 'Error, unknown identifier {0} in container value of defined variable {1}'.format(subitems[0], var_name)
        target += var.local_name
        metadata[var_name] = metadata_request[var_name]
        #if not metadata_define[var_name][0].type in ['real']:
        #    print var_name, metadata_define[var_name][0].print_debug()
        #    raise Exception
        #print var_name, metadata_define[var_name][0].type
        for var in metadata[var_name]:
            var.target = target
            if optional:
                print 'Optional variable {0} in {1} matched to target {2} in module {3}'.format(var_name, var.container, target, modules[-1])
            else:
                print 'Requested variable {0} in {1} matched to target {2} in module {3}'.format(var_name, var.container, target, modules[-1])

    # Remove duplicated from list of modules
    modules = sorted(list(set(modules)))
    if missing > 0:
        print '---'
        print 'In total, {0} variables missing'.format(missing)
        print '---'
    return (success, modules, metadata)

def create_module_use_statements(modules):
    # MODULE_USE_TEMPLATE must include the required modules
    # for error handling of the ccpp_fields_add statments
    module_use_statements = MODULE_USE_TEMPLATE
    cnt = 1
    for module in modules:
        module_use_statements += 'use {0}\n'.format(module)
        cnt += 1
    print "Generated module use statements for {0} module(s)".format(cnt)
    return module_use_statements

def create_ccpp_fields_add_statements(metadata):
    # The metadata container may contain multiple entries
    # for the same variable standard_name, but for different
    # "callers" (i.e. subroutines using it) with potentially
    # different local_name. We only need to add it once to
    # the add_field statement, since the target (i.e. the
    # original variable defined by the model) is the same.
    ccpp_fields_add_statements = ''
    cnt = 0
    for var_name in sorted(metadata.keys()):
        # Add variable with var_name = standard_name once
        var = metadata[var_name][0]
        standard_name = var.standard_name
        units = var.units
        target = var.target
        # DH*
        #if not var_name == 'air_pressure':
        #    print "SKIPPING VARIABLE {0} != air_pressure for testing".format(var_name)
        #    continue
        if var.type in STANDARD_VARIABLE_TYPES:
            ccpp_fields_add_template = CCPP_FIELDS_ADD_TEMPLATE_STANDARD_VARIABLE_TYPE
        elif var.rank == '0':
            ccpp_fields_add_template = CCPP_FIELDS_ADD_TEMPLATE_DERIVED_VARIABLE_TYPE_SCALAR
        else:
            ccpp_fields_add_template = CCPP_FIELDS_ADD_TEMPLATE_DERIVED_VARIABLE_TYPE_ARRAY
        # *DH
        ccpp_fields_add_statements += ccpp_fields_add_template.format(standard_name, target, units)
        cnt += 1
    print "Generated ccpp_fields_add statements for {0} variable(s)".format(cnt)
    return ccpp_fields_add_statements

def generate_include_files(module_use_statements, ccpp_fields_add_statements):
    target_dirs = []
    for target_file in target_files:
        target_dirs.append(os.path.split(target_file)[0])
    target_dirs = sorted(list(set(target_dirs)))
    for target_dir in target_dirs:
        # module use statements
        includefile = os.path.join(target_dir, module_include_file)
        print "Generating module-use include file {0}".format(includefile)
        with open(includefile, "w") as f:
            f.write(module_use_statements)
        # ccpp_fields_add statements
        includefile = os.path.join(target_dir, fields_include_file)
        print "Generating fields-add include file {0}".format(includefile)
        with open(includefile, "w") as f:
            f.write(ccpp_fields_add_statements)
    return True

def main():
    metadata_define = gather_variable_definitions()
    metadata_request = collect_physics_subroutines()
    (success, metadata_request) = check_optional_arguments(metadata_request)
    if not success:
        raise Exception('Call to check_optional_arguments failed.')
    (success, modules, metadata) = compare_metadata(metadata_define, metadata_request)
    if not success:
        #print 'ERROR, Call to compare_metadata failed. Ignore and proceed for testing purposes ...'
        raise Exception('Call to compare_metadata failed.')
    print
    module_use_statements = create_module_use_statements(modules)
    print "module_use_statements:"
    print module_use_statements
    ccpp_fields_add_statements = create_ccpp_fields_add_statements(metadata)
    print "ccpp_fields_add_statements:"
    print ccpp_fields_add_statements
    success = generate_include_files(module_use_statements, ccpp_fields_add_statements)
    # DH* Force exiting without error
    sys.exit(0)

if __name__ == '__main__':
    main()