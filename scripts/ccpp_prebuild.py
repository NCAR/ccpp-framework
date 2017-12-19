#!/usr/bin/env python

# Standard modules
import os
import sys

# Local modules
from common import execute
from metadata_parser import merge_metadata_dicts, parse_scheme_tables, parse_variable_tables

###############################################################################
# Definitions                                                                 #
###############################################################################

variable_definition_files = [
    'FV3/gfsphysics/GFS_layer/GFS_typedefs.F90',
    ]

#driver_files = [
#    'FV3/GFS_physics_driver.F90',
#    'FV3/GFS_radiation_driver.F90',
#    ]

physicsdirs = [
    'FV3/gfsphysics/physics',
    ]

scheme_files = [
    'FV3/gfsphysics/physics/moninedmf.f.tmp.ccpp-compliant-from-grant-for-testing',
    ]
# DH* use output from longnames_autocheck.py to test ccpp_prebuild.py for completeness of variables
scheme_files = [
    'manual_fixes_for_testing_ccpp_prebuild/christopherwharrop_harrop_gfs_gmtb-gfsphysics/physics/sfc_nst.f',
    'manual_fixes_for_testing_ccpp_prebuild/davegill_gfs_separator_SASS_gmtb-gfsphysics/physics/mfdeepcnv.f',
    'manual_fixes_for_testing_ccpp_prebuild/grantfirl_EDMF_table_gmtb-gfsphysics/physics/moninedmf.f',
    'manual_fixes_for_testing_ccpp_prebuild/grantfirl_EDMF_table_gmtb-gfsphysics/physics/GFS_PBL_generic.f90',
    'manual_fixes_for_testing_ccpp_prebuild/grantfirl_EDMF_table_gmtb-gfsphysics/physics/GFS_DCNV_generic.f90',
    'manual_fixes_for_testing_ccpp_prebuild/grantfirl_EDMF_table_gmtb-gfsphysics/physics/GFS_suite_interstitial.f90',
    'manual_fixes_for_testing_ccpp_prebuild/grantfirl_EDMF_table_gmtb-gfsphysics/physics/GFS_SCNV_generic.f90',
    'manual_fixes_for_testing_ccpp_prebuild/gsketefian_gsk_separator_work_gfs_gmtb-gfsphysics/physics/gwdc.f',
    'manual_fixes_for_testing_ccpp_prebuild/gsketefian_gsk_separator_work_gfs_gmtb-gfsphysics/physics/gwdps.f',
    'manual_fixes_for_testing_ccpp_prebuild/gsketefian_gsk_separator_work_gfs_gmtb-gfsphysics/physics/rayleigh_damp.f',
    'manual_fixes_for_testing_ccpp_prebuild/gsketefian_gsk_separator_work_gfs_gmtb-gfsphysics/physics/get_prs_fv3.f90',
    'manual_fixes_for_testing_ccpp_prebuild/gsketefian_gsk_separator_work_gfs_gmtb-gfsphysics/physics/cnvc90.f',
    'manual_fixes_for_testing_ccpp_prebuild/gsketefian_gsk_separator_work_gfs_gmtb-gfsphysics/physics/dcyc2.f',
    'manual_fixes_for_testing_ccpp_prebuild/gsketefian_gsk_separator_work_gfs_gmtb-gfsphysics/physics/ozphys.f',
    'manual_fixes_for_testing_ccpp_prebuild/JulieSchramm_gfs_separator_shalcnv_gmtb-gfsphysics/physics/mfshalcnv.f',
    # DH* 'manual_fixes_for_testing_ccpp_prebuild/kellylittleblackdog_seaice_separator_work_gfs_gmtb-gfsphysics/physics/sfc_sice.f',
    'manual_fixes_for_testing_ccpp_prebuild/lulinxue_GFS_separate_Noah_LSM_master_gfsphysics_gmtb-gfsphysics/physics/sfc_drv.f',
    'manual_fixes_for_testing_ccpp_prebuild/lulinxue_GFS_separate_Noah_LSM_master_gfsphysics_gmtb-gfsphysics/physics/sfc_diff.f',
    'manual_fixes_for_testing_ccpp_prebuild/lulinxue_GFS_separate_Noah_LSM_master_gfsphysics_gmtb-gfsphysics/physics/sfc_diag.f',
    'manual_fixes_for_testing_ccpp_prebuild/lulinxue_GFS_separate_Noah_LSM_master_gfsphysics_gmtb-gfsphysics/physics/GFS_surface_loop_control.f',
    'manual_fixes_for_testing_ccpp_prebuild/mzhangw_masep_gmtb-gfsphysics/physics/gscond.f',
    'manual_fixes_for_testing_ccpp_prebuild/mzhangw_masep_gmtb-gfsphysics/physics/precpd.f',
    'manual_fixes_for_testing_ccpp_prebuild/mzhangw_masep_gmtb-gfsphysics/physics/GFS_calpreciptype.f90',
    'manual_fixes_for_testing_ccpp_prebuild/mzhangw_masep_gmtb-gfsphysics/physics/GFS_MP_generic_pre.f90',
    'manual_fixes_for_testing_ccpp_prebuild/mzhangw_masep_gmtb-gfsphysics/physics/GFS_MP_generic_post.f90',
    'manual_fixes_for_testing_ccpp_prebuild/mzhangw_masep_gmtb-gfsphysics/physics/GFS_zhao_carr_pre.f90',
    'manual_fixes_for_testing_ccpp_prebuild/pedro-jm_radiation2_gmtb-gfsphysics/physics/radsw_main.f',
    'manual_fixes_for_testing_ccpp_prebuild/pedro-jm_radiation2_gmtb-gfsphysics/physics/radlw_main.f',
    ]

target_files = [
    'FV3/atmos_model.F90',
    'FV3/gfsphysics/IPD_layer/IPD_Driver.F90',
    ]

module_include_file = 'ccpp_modules.inc'

fields_include_file = 'ccpp_fields.inc'

###############################################################################
# Template code to generate include files                                     #
###############################################################################

# Modules to load for auto-generated ccpp_fields_add code (e.g. error handling)
MODULE_USE_TEMPLATE = \
"""use fms_mod, only: error_mesg, FATAL
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
    call error_mesg('ccpp', 'error calling ccpp_fields_add for field "{0}"', FATAL)
end if
"""

CCPP_FIELDS_ADD_TEMPLATE_DERIVED_VARIABLE_TYPE_SCALAR = \
"""
call ccpp_fields_add(cdata_block(nb), '{0}', '', c_loc({1}), ierr)
!call ccpp_fields_add(cdata_block(nb), '{0}', '', {1}, ierr=ierr)
if (ierr /= 0) then
    call error_mesg('ccpp', 'error calling ccpp_fields_add for field "{0}"', FATAL)
end if
"""

CCPP_FIELDS_ADD_TEMPLATE_DERIVED_VARIABLE_TYPE_ARRAY = \
"""
call ccpp_fields_add(cdata_block(nb), '{0}', '', c_loc({1}), rank=size({1}), dims=shape({1}), ierr=ierr)
if (ierr /= 0) then
    call error_mesg('ccpp', 'error calling ccpp_fields_add for field "{0}"', FATAL)
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
    """Scan all Fortran source files in the physics directory for subroutines with argument tables"""
    metadata_request = {}
    for scheme_file in scheme_files:
        metadata = parse_scheme_tables(scheme_file)
        metadata_request = merge_metadata_dicts(metadata_request, metadata)
    return metadata_request

def compare_metadata(metadata_define, metadata_request):
    """Compare the requested metadata to the defined one. For each requested entry, a
    single (i.e. non-ambiguous entry) must be present in the defined entries. Also ..."""
    success = True
    missing = 0
    modules = []
    metadata = {}
    for var_name in sorted(metadata_request.keys()):
        # Check that variable is provided by the model
        if not var_name in metadata_define.keys():
            success = False
            missing += 1
            print 'Error, requested variable {0} not provided by the model'.format(var_name)
            continue
        # Check that an unambiguous target exists for this variable
        if len(metadata_define[var_name]) > 1:
            success = False
            print 'Error, requested variable {0} cannot be identified unambiguously.' +\
                    ' multiple definitions in {1}'.format(' & '.join(var.container for var in metadata_define[var_name]))
            continue
        # DH* MISSING - compliancy check
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
    print metadata_define
    metadata_request = collect_physics_subroutines()
    print metadata_request
    (success, modules, metadata) = compare_metadata(metadata_define, metadata_request)
    print metadata
    if not success:
        print 'ERROR, Call to compare_metadata failed. Ignore and proceed for testing purposes ...'
        #raise Exception('Call to compare_metadata failed.')
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