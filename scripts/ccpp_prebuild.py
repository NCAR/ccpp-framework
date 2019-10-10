#!/usr/bin/env python

# Standard modules
import argparse
import collections
import itertools
import logging
import os
import re
import sys

# Local modules
from common import encode_container, decode_container, decode_container_as_dict, execute
from common import CCPP_INTERNAL_VARIABLES, CCPP_STATIC_API_MODULE
from common import split_var_name_and_array_reference
from metadata_parser import merge_dictionaries, parse_scheme_tables, parse_variable_tables
from mkcap import Cap, CapsMakefile, CapsCMakefile, CapsSourcefile, \
                  SchemesMakefile, SchemesCMakefile, SchemesSourcefile
from mkdoc import metadata_to_html, metadata_to_latex
from mkstatic import API, Suite, Group

###############################################################################
# Set up the command line argument parser and other global variables          #
###############################################################################

parser = argparse.ArgumentParser()
parser.add_argument('--config',     action='store', help='path to CCPP prebuild configuration file', required=True)
parser.add_argument('--clean',      action='store_true', help='remove files created by this script, then exit', default=False)
parser.add_argument('--debug',      action='store_true', help='enable debugging output', default=False)
parser.add_argument('--static',     action='store_true', help='enable a static build for a given suite definition file', default=False)
parser.add_argument('--suites',     action='store', help='suite definition files to use (comma-separated, for static build only, without path)', default='')
parser.add_argument('--builddir',   action='store', help='relative path to CCPP build directory', required=False, default='.')

# BASEDIR is the current directory where this script is executed
BASEDIR = os.getcwd()

# SCRIPTDIR is the directory where the ccpp_prebuild.py and its Python modules are located
SCRIPTDIR = os.path.abspath(os.path.split(__file__)[0])

# SRCDIR is the directory where the CCPP framework source code (C, Fortran) is located
SRCDIR = os.path.abspath(os.path.join(SCRIPTDIR, '..', 'src'))

# Definition of variables (metadata tables) that are provided by CCPP
CCPP_INTERNAL_VARIABLE_DEFINITON_FILE = os.path.join(SRCDIR, 'ccpp_types.F90')

###############################################################################
# Functions and subroutines                                                   #
###############################################################################

def parse_arguments():
    """Parse command line arguments."""
    success = True
    args = parser.parse_args()
    configfile = args.config
    clean = args.clean
    debug = args.debug
    static = args.static
    if static and not args.suites:
        parser.print_help()
        sys.exit(-1)
    sdfs = [ 'suite_{0}.xml'.format(x) for x in args.suites.split(',')]
    builddir = args.builddir
    return (success, configfile, clean, debug, static, sdfs, builddir)

def import_config(configfile, builddir):
    """Import the configuration from a given configuration file"""
    success = True
    config = {}

    if not os.path.isfile(configfile):
        logging.error("Configuration file {0} not found".format(configfile))
        success = False
        return

    # Import the host-model specific CCPP prebuild config;
    # split into path and module name for import
    configpath = os.path.abspath(os.path.split(configfile)[0])
    configmodule = os.path.split(configfile)[1].rstrip('.py')
    sys.path.append(configpath)
    ccpp_prebuild_config = __import__(configmodule)

    # Definitions in host-model dependent CCPP prebuild config script
    config['variable_definition_files'] = ccpp_prebuild_config.VARIABLE_DEFINITION_FILES
    config['scheme_files']              = ccpp_prebuild_config.SCHEME_FILES
    config['scheme_files_dependencies'] = ccpp_prebuild_config.SCHEME_FILES_DEPENDENCIES
    config['schemes_makefile']          = ccpp_prebuild_config.SCHEMES_MAKEFILE.format(build_dir=builddir)
    config['schemes_cmakefile']         = ccpp_prebuild_config.SCHEMES_CMAKEFILE.format(build_dir=builddir)
    config['schemes_sourcefile']        = ccpp_prebuild_config.SCHEMES_SOURCEFILE.format(build_dir=builddir)
    config['target_files']              = ccpp_prebuild_config.TARGET_FILES
    config['caps_makefile']             = ccpp_prebuild_config.CAPS_MAKEFILE.format(build_dir=builddir)
    config['caps_cmakefile']            = ccpp_prebuild_config.CAPS_CMAKEFILE.format(build_dir=builddir)
    config['caps_sourcefile']           = ccpp_prebuild_config.CAPS_SOURCEFILE.format(build_dir=builddir)
    config['caps_dir']                  = ccpp_prebuild_config.CAPS_DIR.format(build_dir=builddir)
    config['suites_dir']                = ccpp_prebuild_config.SUITES_DIR
    config['optional_arguments']        = ccpp_prebuild_config.OPTIONAL_ARGUMENTS
    config['module_include_file']       = ccpp_prebuild_config.MODULE_INCLUDE_FILE
    config['fields_include_file']       = ccpp_prebuild_config.FIELDS_INCLUDE_FILE
    config['host_model']                = ccpp_prebuild_config.HOST_MODEL_IDENTIFIER
    config['html_vartable_file']        = ccpp_prebuild_config.HTML_VARTABLE_FILE.format(build_dir=builddir)
    config['latex_vartable_file']       = ccpp_prebuild_config.LATEX_VARTABLE_FILE.format(build_dir=builddir)
    # For static build: location of static API file, and shell script to source
    config['static_api_dir']            = ccpp_prebuild_config.STATIC_API_DIR.format(build_dir=builddir)
    config['static_api_srcfile']        = ccpp_prebuild_config.STATIC_API_SRCFILE.format(build_dir=builddir)
    # Template code in host-model dependent CCPP prebuild config script
    config['ccpp_data_structure']            = ccpp_prebuild_config.CCPP_DATA_STRUCTURE

    # Add model-intependent, CCPP-internal variable definition files
    config['variable_definition_files'].append(CCPP_INTERNAL_VARIABLE_DEFINITON_FILE)

    # To handle new metadata: import DDT references (if exist)
    try:
        config['typedefs_new_metadata'] = ccpp_prebuild_config.TYPEDEFS_NEW_METADATA
        logging.info("Found TYPEDEFS_NEW_METADATA dictionary in config, assume at least some data is in new metadata formet")
    except AttributeError:
        config['typedefs_new_metadata'] = None
        logging.info("Could not find TYPEDEFS_NEW_METADATA dictionary in config, assume all data is in old metadata formet")

    return(success, config)

def setup_logging(debug):
    """Sets up the logging module and logging level."""
    success = True
    if debug:
        level = logging.DEBUG
    else:
        level = logging.INFO
    logging.basicConfig(format='%(levelname)s: %(message)s', level=level)
    if debug:
        logging.info('Logging level set to DEBUG')
    else:
        logging.info('Logging level set to INFO')
    return success

def clean_files(config, static):
    """Clean files created by ccpp_prebuild.py"""
    success = True
    logging.info('Performing clean ....')
    # Create list of files to remove, use wildcards where necessary
    files_to_remove = [
        config['schemes_makefile'],
        config['schemes_cmakefile'],
        config['schemes_sourcefile'],
        config['caps_makefile'],
        config['caps_cmakefile'],
        config['caps_sourcefile'],
        config['html_vartable_file'],
        config['latex_vartable_file'],
        ]
    if static:
        files_to_remove.append(os.path.join(config['caps_dir'], 'ccpp_*_cap.F90'))
        files_to_remove.append(os.path.join(config['static_api_dir'], '{api}.F90'.format(api=CCPP_STATIC_API_MODULE)))
    else:
        files_to_remove.append(os.path.join(config['caps_dir'], '*_cap.F90'))
        for target_file in config['target_files']:
            target_file_path = os.path.split(target_file)[0]
            files_to_remove.append(os.path.join(target_file_path, config['module_include_file'].format(set='*')))
            files_to_remove.append(os.path.join(target_file_path, config['fields_include_file'].format(set='*')))
    # Not very pythonic, but the easiest way w/o importing another Python module
    cmd = 'rm -vf {0}'.format(' '.join(files_to_remove))
    execute(cmd)
    return success

def parse_suites(suites_dir, sdfs):
    """Parse suite definition files for static build"""
    logging.info('Parsing suite definition files ...')
    suites = []
    for sdf in sdfs:
        logging.info('Parsing suite definition file {0} ...'.format(os.path.join(suites_dir, sdf)))
        suite = Suite(sdf_name=os.path.join(suites_dir, sdf))
        success = suite.parse()
        if not success:
            logging.error('Parsing suite definition file {0} failed.'.format(sdf))
            break
        suites.append(suite)
    return (success, suites)

def check_unique_pset_per_scheme(scheme_files):
    """Check that each scheme belongs to one and only one physics set"""
    success = True
    for scheme_file in scheme_files.keys():
        if len(scheme_files[scheme_file])>1:
            logging.error("Scheme file {0} belongs to multiple physics sets: {1}".format(scheme_file, ','.join(scheme_files[scheme_file])))
            success = False
    return success

def convert_local_name_from_new_metadata(metadata, standard_name, typedefs_new_metadata, converted_variables):
    """Convert local names in new metadata format (no old-style DDT references, array references as
    standard names) to old metadata format (with old-style DDT references, array references as local names)."""
    success = True
    var = metadata[standard_name][0]
    # Check if this variable has already been converted
    if standard_name in converted_variables:
        logging.debug('Variable {0} was in old metadata format and has already been converted'.format(standard_name))
        return (success, var.local_name, converted_variables)
    # Decode container into a dictionary
    container = decode_container_as_dict(var.container)
    # Check if variable is in old or new metadata format
    module_name = container['MODULE']
    if not module_name in typedefs_new_metadata.keys():
        logging.debug('Variable {0} is in old metadata format, no conversion necessary'.format(standard_name))
        return (success, var.local_name, converted_variables)
    # For module variables set type_name to module_name
    if not 'TYPE' in container.keys():
        type_name = module_name
    else:
        type_name = container['TYPE']
    # Check that this module/type is configured (modules will have empty prefices)
    if not type_name in typedefs_new_metadata[module_name].keys():
        logging.error("Module {0} uses the new metadata format, but module/type {1} is not configured".format(module_name, type_name))
        success = False
        return (success, None, converted_variables)

    # The local name (incl. the array reference) is in new metadata format
    local_name = var.local_name
    logging.info("Converting local name {0} of variable {1} from new to old metadata".format(local_name, standard_name))
    if "(" in local_name:
        (actual_var_name, array_reference) = split_var_name_and_array_reference(local_name)
        indices = array_reference.lstrip('(').rstrip(')').split(',')
        indices_local_names = []
        for index_range in indices:
            # Leave colons-only dimension alone
            if index_range == ':':
                indices_local_names.append(index_range)
                continue
            # Split by colons to get a pair of dimensions
            dimensions = index_range.split(':')
            dimensions_local_names = []
            for dimension in dimensions:
                # Leave literals alone
                try:
                    int(dimension)
                    dimensions_local_names.append(dimension)
                    continue
                except ValueError:
                    pass
                # Convert the local name of the dimension to old metadata standard, if necessary (recursive call)
                (success, local_name_dim, converted_variables) = convert_local_name_from_new_metadata(
                                      metadata, dimension, typedefs_new_metadata, converted_variables)
                if not success:
                    return (success, None, converted_variables)
                # Update the local name of the dimension, if necessary
                if not metadata[dimension][0].local_name == local_name_dim:
                    logging.debug("Updating local name of variable {0} from {1} to {2}".format(dimension,
                                                      metadata[dimension][0].local_name, local_name_dim))
                    metadata[dimension][0].local_name = local_name_dim
                dimensions_local_names.append(local_name_dim)
            indices_local_names.append(':'.join(dimensions_local_names))
        # Put back together the array reference with local names in old metadata format
        array_reference_local_names = '(' + ','.join(indices_local_names) + ')'
        # Compose local name (still without any DDT reference prefix)
        local_name = actual_var_name + array_reference_local_names

    # Prefix the local name with the reference if not empty
    if typedefs_new_metadata[module_name][type_name]:
        local_name = typedefs_new_metadata[module_name][type_name] + '%' + local_name
    if success:
        converted_variables.append(standard_name)

    return (success, local_name, converted_variables)

def gather_variable_definitions(variable_definition_files, typedefs_new_metadata):
    """Scan all Fortran source files with variable definitions on the host model side.
    If typedefs_new_metadata is not None, search all metadata entries and convert new metadata
    (local names) into old metadata by prepending the DDT references."""
    #
    logging.info('Parsing metadata tables for variables provided by host model ...')
    success = True
    metadata_define = {}
    for variable_definition_file in variable_definition_files:
        (filedir, filename) = os.path.split(variable_definition_file)
        # Change to directory of variable_definition_file and parse it
        os.chdir(os.path.join(BASEDIR,filedir))
        metadata = parse_variable_tables(filename)
        metadata_define = merge_dictionaries(metadata_define, metadata)
        # Return to BASEDIR
        os.chdir(BASEDIR)
    #
    if typedefs_new_metadata:
        logging.info('Convert local names from new metadata format into old metadata format ...')
        # Keep track of which variables have already been converted
        converted_variables = []
        for key in metadata_define.keys():
            # Double-check that variable definitions are unique
            if len(metadata_define[key])>1:
                logging.error("Multiple definitions of standard_name {0} in type/variable defintions".format(key))
                success = False
                return
            (success, local_name, converted_variables) = convert_local_name_from_new_metadata(
                             metadata_define, key, typedefs_new_metadata, converted_variables)
            if not success:
                logging.error("An error occurred during the conversion of variable {0} from new to old metadata format".format(key))
                return (success, metadata_define)
            # Update the local name of the variable, if necessary
            if not metadata_define[key][0].local_name == local_name:
                logging.debug("Updating local name of variable {0} from {1} to {2}".format(key,
                                               metadata_define[key][0].local_name, local_name))
                metadata_define[key][0].local_name = local_name
    #
    return (success, metadata_define)

def collect_physics_subroutines(scheme_files):
    """Scan all Fortran source files in scheme_files for subroutines with argument tables."""
    logging.info('Parsing metadata tables in physics scheme files ...')
    success = True
    # Parse all scheme files
    metadata_request = {}
    arguments_request = {}
    pset_request = {}
    pset_schemes = {}
    for scheme_file in scheme_files.keys():
        (scheme_filepath, scheme_filename) = os.path.split(os.path.abspath(scheme_file))
        # Change to directory where scheme_file lives
        os.chdir(scheme_filepath)
        (metadata, arguments) = parse_scheme_tables(scheme_filename)
        # The different psets for the variables used by schemes in scheme_file
        pset = { var_name : scheme_files[scheme_file] for var_name in metadata.keys() }
        # The different psets for the schemes in scheme_file
        for scheme_name in arguments.keys():
            pset_schemes[scheme_name] = scheme_files[scheme_file]
        # Merge metadata and pset, append to arguments
        metadata_request = merge_dictionaries(metadata_request, metadata)
        pset_request = merge_dictionaries(pset_request, pset)
        arguments_request.update(arguments)
        os.chdir(BASEDIR)
    # Return to BASEDIR
    os.chdir(BASEDIR)
    return (success, metadata_request, pset_request, arguments_request, pset_schemes)

def filter_metadata(metadata, pset, arguments, suites):
    """Remove all variables from metadata that are not used in the given suite"""
    success = True
    # Output: filtered dictionaries
    metadata_filtered = {}
    pset_filtered = {}
    arguments_filtered = {}
    # Loop through all variables and check if the calling subroutine is in list of subroutines
    for var_name in sorted(metadata.keys()):
        keep = False
        for var in metadata[var_name][:]:
            container_string = decode_container(var.container)
            subroutine = container_string[container_string.find('SUBROUTINE')+len('SUBROUTINE')+1:]
            for suite in suites:
                if subroutine in suite.all_subroutines_called:
                    keep = True
                    break
            if keep:
                break
        if keep:
            metadata_filtered[var_name] = metadata[var_name]
            pset_filtered[var_name] = pset[var_name]
        else:
            logging.info("filtering out variable {0}".format(var_name))
    for scheme in arguments.keys():
        for suite in suites:
            if scheme in suite.all_schemes_called:
                arguments_filtered[scheme] = arguments[scheme]
                break

    return (success, metadata_filtered, pset_filtered, arguments_filtered)

def check_optional_arguments(metadata, arguments, optional_arguments):
    """Check if for each subroutine with optional arguments, an entry exists in the
    optional_arguments dictionary. This is required to generate the caps correctly
    and to assess whether the variable is required from the host model. Optional
    arguments that are not requested by the model as specified in the dictionary
    optional_arguments are deleted from the list of requested data individually
    for each subroutine."""
    logging.info('Checking optional arguments in physics schemes ...')
    success = True
    for var_name in sorted(metadata.keys()):
        # The notation metadata[var_name][:] is a convenient way to make a copy
        # of the metadata[var_name] list, which allows removing items as we go
        for var in metadata[var_name][:]:
            if var.optional in ['t', 'T']:
                for item in var.container.split(' '):
                    subitems = item.split('_')
                    if subitems[0] == 'MODULE':
                        module_name = '_'.join(subitems[1:])
                    elif subitems[0] == 'SCHEME':
                        scheme_name = '_'.join(subitems[1:])
                    elif subitems[0] == 'SUBROUTINE':
                        subroutine_name = '_'.join(subitems[1:])
                    else:
                        success = False
                        logging.error('Invalid identifier {0} in container value {1} of requested variable {2}'.format(
                                                                                 subitems[0], var.container, var_name))
                if not module_name in optional_arguments.keys() or not \
                        subroutine_name in optional_arguments[module_name].keys():
                    success = False
                    logging.error('No entry found in optional_arguments dictionary for optional argument ' + \
                                  '{0} to subroutine {1} in module {2}'.format(var_name, subroutine_name, module_name))
                if type(optional_arguments[module_name][subroutine_name]) is list:
                    if var_name in optional_arguments[module_name][subroutine_name]:
                        logging.debug('Optional argument {0} to subroutine {1} in module {2} is required, keep in list'.format(
                                                                                       var_name, subroutine_name, module_name))
                    else:
                        logging.debug('Optional argument {0} to subroutine {1} in module {2} is not required, remove from list'.format(
                                                                                               var_name, subroutine_name, module_name))
                        # Remove this var instance from list of var instances for this var_name
                        metadata[var_name].remove(var)
                        # Remove var_name from list of calling arguments for that subroutine
                        # (unless that module has been filtered out for the static build)
                        if module_name in arguments.keys():
                            arguments[module_name][scheme_name][subroutine_name].remove(var_name)
                elif optional_arguments[module_name][subroutine_name] == 'all':
                    logging.debug('optional argument {0} to subroutine {1} in module {2} is required, keep in list'.format(
                                                                                   var_name, subroutine_name, module_name))

        # If metadata[var_name] is now empty, i.e. the variable is not
        # requested at all by the model, remove the entry from metadata
        if not metadata[var_name]:
            del metadata[var_name]

    return (success, metadata, arguments)

def compare_metadata(metadata_define, metadata_request, pset_request, psets_merged):
    """Compare the requested metadata to the defined one. For each requested entry, a
    single (i.e. non-ambiguous entry) must be present in the defined entries. All optional
    arguments that are still in the list of required variables for a scheme are needed,
    since they were checked in the routine check_optional_arguments beforehand."""

    logging.info('Comparing metadata for requested and provided variables ...')
    success = True
    modules = { x : [] for x in psets_merged }
    metadata = {}
    for var_name in sorted(metadata_request.keys()):
        # Check that variable is provided by the model
        if not var_name in metadata_define.keys():
            requested_by = ' & '.join(var.container for var in metadata_request[var_name])
            success = False
            logging.error('Variable {0} requested by {1} not provided by the model'.format(var_name, requested_by))
            continue
        # Check that an unambiguous target exists for this variable
        if len(metadata_define[var_name]) > 1:
            success = False
            requested_by = ' & '.join(var.container for var in metadata_request[var_name])
            provided_by = ' & '.join(var.container for var in metadata_define[var_name])
            error_message = '  error, variable {0} requested by {1} cannot be identified unambiguously.'.format(var_name, requested_by) +\
                            ' Multiple definitions in {0}'.format(provided_by)
            logging.error(error_message)
            continue
        # Check that the variable properties are compatible between the model and the schemes;
        # because we know that all variables in the metadata_request[var_name] list are compatible,
        # it is sufficient to test the first entry against (the unique) metadata_define[var_name][0].
        if not metadata_request[var_name][0].compatible(metadata_define[var_name][0]):
            success = False
            error_message = '  incompatible entries in metadata for variable {0}:\n'.format(var_name) +\
                            '    provided:  {0}\n'.format(metadata_define[var_name][0].print_debug()) +\
                            '    requested: {0}'.format(metadata_request[var_name][0].print_debug())
            logging.error(error_message)
            continue
        # Check for and register unit conversions if necessary. This must be done for each registered
        # variable in the metadata_request[var_name] list (i.e. for each subroutine that is using it).
        # Because var is an instance of the variable specific to the subroutine that uses it, and since
        # each variable can be passed to a subroutine only once, there can be no overlapping/conflicting
        # unit conversions.
        for var in metadata_request[var_name]:
            # Compare units
            if var.units == metadata_define[var_name][0].units:
                continue
            # Register conversion, depending on the intent for this subroutine.
            if var.intent=='inout':
                var.convert_from(metadata_define[var_name][0].units)
                var.convert_to(metadata_define[var_name][0].units)
            elif var.intent=='in':
                var.convert_from(metadata_define[var_name][0].units)
            elif var.intent=='out':
                var.convert_to(metadata_define[var_name][0].units)
        # Construct the actual target variable and list of modules to use from the information in 'container'
        var = metadata_define[var_name][0]
        target = ''
        for item in var.container.split(' '):
            subitems = item.split('_')
            if subitems[0] == 'MODULE':
                # Add to list of required modules for each pset the requested variable falls under
                for pset in pset_request[var_name]:
                    modules[pset].append('_'.join(subitems[1:]))
            elif subitems[0] == 'TYPE':
                pass
            else:
                logging.error('Unknown identifier {0} in container value of defined variable {1}'.format(subitems[0], var_name))
        target += var.local_name
        # Copy the length kind from the variable definition to update len=* in the variable requests
        if var.type == 'character':
            kind = var.kind
        metadata[var_name] = metadata_request[var_name]
        # Set target and kind (if applicable)
        for var in metadata[var_name]:
            var.target = target
            logging.debug('Requested variable {0} in {1} matched to target {2} in module {3}'.format(
                          var_name, var.container, target, modules[pset_request[var_name][0]][-1]))
            # Update len=* for character variables
            if var.type == 'character' and var.kind == 'len=*':
                logging.debug('Update kind information for requested variable {0} in {1} from {2} to {3}'.format(var_name,
                                                                                           var.container, var.kind, kind))
                var.kind = kind

    # Remove duplicated from list of modules
    for pset in psets_merged:
        modules[pset] = sorted(list(set(modules[pset])))
    return (success, modules, metadata)

def create_module_use_statements(modules, pset):
    """Create Fortran module use statements to be included in the host cap."""
    logging.info('Generating module use statements for physics set {0} ...'.format(pset))
    success = True
    module_use_statements = ''
    cnt = 1
    for module in modules:
        module_use_statements += 'use {0}\n'.format(module)
        cnt += 1
    logging.info('Generated module use statements for {0} module(s)'.format(cnt))
    return (success, module_use_statements)

def create_ccpp_field_add_statements(metadata, pset, ccpp_data_structure):
    """Create Fortran code to add host model variables to the cdata
    structure. The metadata container may contain multiple entries
    of a variable with the same standard_name, but for different
    "callers" (i.e. subroutines using it) with identical or
    different local_name. We only need to add it once to
    the add_field statement, since the target (i.e. the
    original variable defined by the model) is the same."""
    logging.info('Generating ccpp_field_add statements for physics set {0} ...'.format(pset))
    success = True
    ccpp_field_add_statements = ''
    cnt = 0
    # Record the index for each variable added to cdata via ccpp_add_field()
    ccpp_field_map = {}
    # Important - adding the variables sorted is key to using hard-coded
    # indices for faster retrieval of variables from cdata via ccpp_field_get
    for var_name in sorted(metadata.keys()):
        # Skip CCPP internal variables, these are treated differently
        if var_name in CCPP_INTERNAL_VARIABLES.keys():
            continue
        # Add variable with var_name = standard_name once
        logging.debug('Generating ccpp_field_add statement for variable {0}'.format(var_name))
        var = metadata[var_name][0]
        # Use print add with specified index number and register the index in ccpp_field_map;
        # note: Python counters run from 0 to X, Fortran counters from 1 to X+1
        ccpp_field_add_statements += var.print_add(ccpp_data_structure, cnt+1)
        ccpp_field_map[var_name] = cnt+1
        cnt += 1
    logging.info('Generated ccpp_field_add statements for {0} variable(s)'.format(cnt))
    return (success, ccpp_field_add_statements, ccpp_field_map)

def generate_include_files(module_use_statements, ccpp_field_add_statements,
                           target_files, module_include_file, fields_include_file):
    """Generate include files for modules and field-add statements for host model cap."""
    logging.info('Generating include files for host model caps {0} ...'.format(', '.join(target_files)))
    success = True
    target_dirs = []
    for target_file in target_files:
        target_dirs.append(os.path.split(target_file)[0])
    target_dirs = sorted(list(set(target_dirs)))
    for target_dir in target_dirs:
        # module use statements
        includefile = os.path.join(target_dir, module_include_file)
        logging.info('Generated module-use include file {0}'.format(includefile))
        with open(includefile, "w") as f:
            f.write(module_use_statements)
        # ccpp_field_add statements
        includefile = os.path.join(target_dir, fields_include_file)
        logging.info('Generated fields-add include file {0}'.format(includefile))
        with open(includefile, "w") as f:
            f.write(ccpp_field_add_statements)
    return success

def generate_scheme_caps(metadata_define, metadata_request, arguments, pset_schemes, ccpp_field_maps, caps_dir):
    """Generate scheme caps for all schemes parsed."""
    success = True
    # Change to caps directory
    os.chdir(caps_dir)
    # List of filenames of scheme caps
    scheme_caps = []
    for module_name in arguments.keys():
        for scheme_name in arguments[module_name].keys():
            for subroutine_name in arguments[module_name][scheme_name].keys():
                # Skip subroutines without argument table or with empty argument table
                if not arguments[module_name][scheme_name][subroutine_name]:
                    continue
            # Create cap
            cap = Cap()
            cap.filename = "{0}_cap.F90".format(scheme_name)
            scheme_caps.append(cap.filename)
            # Parse all subroutines and their arguments to generate the cap
            capdata = collections.OrderedDict()
            for subroutine_name in arguments[module_name][scheme_name].keys():
                capdata[subroutine_name] = []
                for var_name in arguments[module_name][scheme_name][subroutine_name]:
                    container = encode_container(module_name, scheme_name, subroutine_name)
                    for var in metadata_request[var_name]:
                        if var.container == container:
                            capdata[subroutine_name].append(var)
                            break
            # Write cap using the unique physics set for the scheme
            pset = pset_schemes[scheme_name][0]
            cap.write(module_name, capdata, ccpp_field_maps[pset], metadata_define)
    #
    os.chdir(BASEDIR)
    return (success, scheme_caps)

def generate_suite_and_group_caps(suites, metadata_request, metadata_define, arguments, caps_dir):
    """Generate for the suite and for all groups parsed."""
    logging.info("Generating suite and group caps ...")
    suite_and_group_caps = []
    # Change to caps directory
    os.chdir(caps_dir)
    for suite in suites:
        logging.debug("Generating suite and group caps for suite {0}...".format(suite.name))
        # Write caps for suite and groups in suite
        suite.write(metadata_request, metadata_define, arguments)
        suite_and_group_caps += suite.caps
    os.chdir(BASEDIR)
    if suite_and_group_caps:
        success = True
    else:
        success = False
    return (success, suite_and_group_caps)

def generate_static_api(suites, static_api_dir):
    """Generate API for static build for a given suite"""
    success = True
    # Change to caps directory, create if necessary
    if not os.path.isdir(static_api_dir):
        os.makedirs(static_api_dir)
    os.chdir(static_api_dir)
    api = API(suites=suites, directory=static_api_dir)
    logging.info('Generating static API {0} in {1} ...'.format(api.filename, static_api_dir))
    api.write()
    os.chdir(BASEDIR)
    return (success, api)

def generate_schemes_makefile(schemes, schemes_makefile, schemes_cmakefile, schemes_sourcefile):
    """Generate makefile/cmakefile snippets for all schemes."""
    logging.info('Generating schemes makefile/cmakefile snippet ...')
    success = True
    makefile = SchemesMakefile()
    makefile.filename = schemes_makefile
    cmakefile = SchemesCMakefile()
    cmakefile.filename = schemes_cmakefile
    sourcefile = SchemesSourcefile()
    sourcefile.filename = schemes_sourcefile
    # Adjust relative file path to schemes from caps makefile
    schemes_with_path = []
    schemes_with_abspath = []
    schemes_makefile_dir = os.path.split(os.path.abspath(schemes_makefile))[0]
    for scheme in schemes:
        (scheme_filepath, scheme_filename) = os.path.split(os.path.abspath(scheme))
        relative_path = './{0}'.format(os.path.relpath(scheme_filepath, schemes_makefile_dir))
        schemes_with_path.append(os.path.join(relative_path, scheme_filename))
        schemes_with_abspath.append(os.path.abspath(scheme))
    makefile.write(schemes_with_path)
    cmakefile.write(schemes_with_path)
    sourcefile.write(schemes_with_abspath)
    logging.info('Added {0} schemes to {1}, {2}, {3}'.format(
           len(schemes_with_path), makefile.filename, cmakefile.filename, sourcefile.filename))
    return success

def generate_caps_makefile(caps, caps_makefile, caps_cmakefile, caps_sourcefile, caps_dir):
    """Generate makefile/cmakefile snippets for all caps."""
    logging.info('Generating caps makefile/cmakefile snippet ...')
    success = True
    makefile = CapsMakefile()
    makefile.filename = caps_makefile
    cmakefile = CapsCMakefile()
    cmakefile.filename = caps_cmakefile
    sourcefile = CapsSourcefile()
    sourcefile.filename = caps_sourcefile
    # Adjust relative file path to schemes from caps makefile
    caps_makefile_dir = os.path.split(os.path.abspath(caps_makefile))[0]
    relative_path = './{0}'.format(os.path.relpath(caps_dir, caps_makefile_dir))
    caps_with_path = [ os.path.join(relative_path, cap) for cap in caps]
    caps_with_abspath = [ os.path.abspath(os.path.join(caps_dir, cap)) for cap in caps]
    makefile.write(caps_with_path)
    cmakefile.write(caps_with_path)
    sourcefile.write(caps_with_abspath)
    logging.info('Added {0} auto-generated caps to {1} and {2}'.format(
                          len(caps_with_path), makefile.filename, cmakefile.filename))
    return success

def main():
    """Main routine that handles the CCPP prebuild for different host models."""
    # Parse command line arguments
    (success, configfile, clean, debug, static, sdfs, builddir) = parse_arguments()
    if not success:
        raise Exception('Call to parse_arguments failed.')

    success = setup_logging(debug)
    if not success:
        raise Exception('Call to setup_logging failed.')

    (success, config) = import_config(configfile, builddir)
    if not success:
        raise Exception('Call to import_config failed.')

    # Perform clean if requested, then exit
    if clean:
        success = clean_files(config, static)
        logging.info('CCPP prebuild clean completed successfully, exiting.')
        sys.exit(0)

    # Parse suite definition files for static build
    if static:
        (success, suites) = parse_suites(config['suites_dir'], sdfs)
        if not success:
            raise Exception('Parsing suite definition files failed.')

    # Check that each scheme only belongs to one set of physics
    # this is required for using the optimized version of ccpp_field_get
    # that supplies the build-time derived index in the array
    success = check_unique_pset_per_scheme(config['scheme_files'])
    if not success:
        raise Exception('Call to check_unique_pset_per_scheme failed.')

    # Variables defined by the host model
    (success, metadata_define) = gather_variable_definitions(config['variable_definition_files'], config['typedefs_new_metadata'])
    if not success:
        raise Exception('Call to gather_variable_definitions failed.')

    # Create an HTML table with all variables provided by the model
    success = metadata_to_html(metadata_define, config['host_model'], config['html_vartable_file'])
    if not success:
        raise Exception('Call to metadata_to_html failed.')

    # Variables requested by the CCPP physics schemes
    (success, metadata_request, pset_request, arguments_request, pset_schemes) = collect_physics_subroutines(config['scheme_files'])
    if not success:
        raise Exception('Call to collect_physics_subroutines failed.')

    # Filter metadata/pset/arguments for static build - remove whatever is not included in suite definition file
    if static:
        (success, metadata_request, pset_request, arguments_request) = filter_metadata(metadata_request, pset_request,
                                                                                            arguments_request, suites)
        if not success:
            raise Exception('Call to filter_metadata failed.')

    # Process optional arguments based on configuration in above dictionary optional_arguments
    (success, metadata_request, arguments_request) = check_optional_arguments(metadata_request,arguments_request,
                                                                              config['optional_arguments'])
    if not success:
        raise Exception('Call to check_optional_arguments failed.')

    # Create a LaTeX table with all variables requested by the pool of physics and/or provided by the host model
    success = metadata_to_latex(metadata_define, metadata_request, pset_request, config['host_model'], config['latex_vartable_file'])
    if not success:
        raise Exception('Call to metadata_to_latex failed.')

    # Flatten list of list of psets for all variables
    psets_merged = list(set(itertools.chain(*pset_request.values())))

    # Check requested against defined arguments to generate metadata (list/dict of variables for CCPP)
    (success, modules, metadata) = compare_metadata(metadata_define, metadata_request, pset_request, psets_merged)
    if not success:
        raise Exception('Call to compare_metadata failed.')

    if not static:
        # Dictionary of indices of variables in the cdata structure, per pset
        ccpp_field_maps = {}
        for pset in psets_merged:
            # Create module use statements to inject into the host model cap
            (success, module_use_statements) = create_module_use_statements(modules[pset], pset)
            if not success:
                raise Exception('Call to create_module_use_statements failed.')

            # Only process variables that fall into this pset
            metadata_filtered = { key : value for (key, value) in metadata.items() if pset in pset_request[key] }

            # Create ccpp_fiels_add statements to inject into the host model cap;
            # this returns a ccpp_field_map that contains indices of variables in
            # the cdata structure for the given pset
            (success, ccpp_field_add_statements, ccpp_field_map) = create_ccpp_field_add_statements(metadata_filtered,
                                                                                  pset, config['ccpp_data_structure'])
            if not success:
                raise Exception('Call to create_ccpp_field_add_statements failed.')
            ccpp_field_maps[pset] = ccpp_field_map

            # Generate include files for module_use_statements and ccpp_field_add_statements
            success = generate_include_files(module_use_statements, ccpp_field_add_statements, config['target_files'],
                                                                       config['module_include_file'].format(set=pset),
                                                                       config['fields_include_file'].format(set=pset))
            if not success:
                raise Exception('Call to generate_include_files failed.')

    # Add filenames of schemes to makefile - add dependencies for schemes
    success = generate_schemes_makefile(config['scheme_files_dependencies'] + config['scheme_files'].keys(),
                                        config['schemes_makefile'], config['schemes_cmakefile'],
                                        config['schemes_sourcefile'])
    if not success:
        raise Exception('Call to generate_schemes_makefile failed.')

    if static:
        # Static build: generate caps for entire suite and groups in the specified suite; generate API
        (success, suite_and_group_caps) = generate_suite_and_group_caps(suites, metadata_request, metadata_define,
                                                                        arguments_request, config['caps_dir'])
        if not success:
            raise Exception('Call to generate_suite_and_group_caps failed.')

        (success, api) = generate_static_api(suites, config['static_api_dir'])
        if not success: 
            raise Exception('Call to generate_static_api failed.')

        success = api.write_sourcefile(config['static_api_srcfile'])
        if not success: 
            raise Exception("Writing API sourcefile {sourcefile} failed".format(sourcefile=config['static_api_srcfile']))
    else:
        # Generate scheme caps for each individual scheme
        (success, scheme_caps) = generate_scheme_caps(metadata_define, metadata_request, arguments_request,
                                                      pset_schemes, ccpp_field_maps, config['caps_dir'])
        if not success:
            raise Exception('Call to generate_scheme_caps failed.')

    # Add filenames of caps to makefile
    if static:
        all_caps = suite_and_group_caps
    else:
        all_caps = scheme_caps
    success = generate_caps_makefile(all_caps, config['caps_makefile'], config['caps_cmakefile'],
                                     config['caps_sourcefile'], config['caps_dir'])
    if not success:
        raise Exception('Call to generate_caps_makefile failed.')

    logging.info('CCPP prebuild step completed successfully.')

if __name__ == '__main__':
    main()
