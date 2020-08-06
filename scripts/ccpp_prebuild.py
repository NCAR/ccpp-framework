#!/usr/bin/env python

# Standard modules
import argparse
import collections
import filecmp
import importlib
import itertools
import logging
import os
import re
import sys

# CCPP framework imports
from common import encode_container, decode_container, decode_container_as_dict, execute
from common import CCPP_INTERNAL_VARIABLES, CCPP_STATIC_API_MODULE, CCPP_INTERNAL_VARIABLE_DEFINITON_FILE
from common import STANDARD_VARIABLE_TYPES, STANDARD_INTEGER_TYPE, CCPP_TYPE
from common import SUITE_DEFINITION_FILENAME_PATTERN
from common import split_var_name_and_array_reference
from metadata_parser import merge_dictionaries, parse_scheme_tables, parse_variable_tables
from mkcap import CapsMakefile, CapsCMakefile, CapsSourcefile, \
                  SchemesMakefile, SchemesCMakefile, SchemesSourcefile, \
                  TypedefsMakefile, TypedefsCMakefile, TypedefsSourcefile
from mkdoc import metadata_to_html, metadata_to_latex
from mkstatic import API, Suite, Group

###############################################################################
# Set up the command line argument parser and other global variables          #
###############################################################################

parser = argparse.ArgumentParser()
parser.add_argument('--config',     action='store', help='path to CCPP prebuild configuration file', required=True)
parser.add_argument('--clean',      action='store_true', help='remove files created by this script, then exit', default=False)
parser.add_argument('--debug',      action='store_true', help='enable debugging output', default=False)
parser.add_argument('--suites',     action='store', help='suite definition files to use (comma-separated, without path)', default='')
parser.add_argument('--builddir',   action='store', help='relative path to CCPP build directory', required=False, default=None)

# BASEDIR is the current directory where this script is executed
BASEDIR = os.getcwd()

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
    if args.suites:
        sdfs = [ 'suite_{0}.xml'.format(x) for x in args.suites.split(',')]
    else:
        sdfs = None
    builddir = args.builddir
    return (success, configfile, clean, debug, sdfs, builddir)

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
    configpath = os.path.abspath(os.path.dirname(configfile))
    configmodule = os.path.splitext(os.path.basename(configfile))[0]
    sys.path.append(configpath)
    ccpp_prebuild_config = importlib.import_module(configmodule)

    # If the build directory for running ccpp_prebuild.py is not
    # specified as command line argument, use value from config
    if not builddir:
        builddir = os.path.join(BASEDIR, ccpp_prebuild_config.DEFAULT_BUILD_DIR)
        logging.info('Build directory not specified on command line, ' + \
                     'use "{}" from CCPP prebuild config'.format(ccpp_prebuild_config.DEFAULT_BUILD_DIR))

    # Definitions in host-model dependent CCPP prebuild config script
    config['variable_definition_files'] = ccpp_prebuild_config.VARIABLE_DEFINITION_FILES
    config['typedefs_makefile']         = ccpp_prebuild_config.TYPEDEFS_MAKEFILE.format(build_dir=builddir)
    config['typedefs_cmakefile']        = ccpp_prebuild_config.TYPEDEFS_CMAKEFILE.format(build_dir=builddir)
    config['typedefs_sourcefile']       = ccpp_prebuild_config.TYPEDEFS_SOURCEFILE.format(build_dir=builddir)
    config['scheme_files']              = ccpp_prebuild_config.SCHEME_FILES
    config['schemes_makefile']          = ccpp_prebuild_config.SCHEMES_MAKEFILE.format(build_dir=builddir)
    config['schemes_cmakefile']         = ccpp_prebuild_config.SCHEMES_CMAKEFILE.format(build_dir=builddir)
    config['schemes_sourcefile']        = ccpp_prebuild_config.SCHEMES_SOURCEFILE.format(build_dir=builddir)
    config['caps_makefile']             = ccpp_prebuild_config.CAPS_MAKEFILE.format(build_dir=builddir)
    config['caps_cmakefile']            = ccpp_prebuild_config.CAPS_CMAKEFILE.format(build_dir=builddir)
    config['caps_sourcefile']           = ccpp_prebuild_config.CAPS_SOURCEFILE.format(build_dir=builddir)
    config['caps_dir']                  = ccpp_prebuild_config.CAPS_DIR.format(build_dir=builddir)
    config['suites_dir']                = ccpp_prebuild_config.SUITES_DIR
    config['optional_arguments']        = ccpp_prebuild_config.OPTIONAL_ARGUMENTS
    config['host_model']                = ccpp_prebuild_config.HOST_MODEL_IDENTIFIER
    config['html_vartable_file']        = ccpp_prebuild_config.HTML_VARTABLE_FILE.format(build_dir=builddir)
    config['latex_vartable_file']       = ccpp_prebuild_config.LATEX_VARTABLE_FILE.format(build_dir=builddir)
    # Location of static API file, and shell script to source
    config['static_api_dir']            = ccpp_prebuild_config.STATIC_API_DIR.format(build_dir=builddir)
    config['static_api_srcfile']        = ccpp_prebuild_config.STATIC_API_SRCFILE.format(build_dir=builddir)

    # Add model-independent, CCPP-internal variable definition files
    config['variable_definition_files'].append(CCPP_INTERNAL_VARIABLE_DEFINITON_FILE)

    # To handle new metadata: import DDT references (if exist)
    try:
        config['typedefs_new_metadata'] = ccpp_prebuild_config.TYPEDEFS_NEW_METADATA
        logging.info("Found TYPEDEFS_NEW_METADATA dictionary in config, assume at least some data is in new metadata format")
    except AttributeError:
        config['typedefs_new_metadata'] = None
        logging.info("Could not find TYPEDEFS_NEW_METADATA dictionary in config, assume all data is in old metadata format")

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

def clean_files(config):
    """Clean files created by ccpp_prebuild.py"""
    success = True
    logging.info('Performing clean ....')
    # Create list of files to remove, use wildcards where necessary
    files_to_remove = [
        config['typedefs_makefile'],
        config['typedefs_cmakefile'],
        config['typedefs_sourcefile'],
        config['schemes_makefile'],
        config['schemes_cmakefile'],
        config['schemes_sourcefile'],
        config['caps_makefile'],
        config['caps_cmakefile'],
        config['caps_sourcefile'],
        config['html_vartable_file'],
        config['latex_vartable_file'],
        os.path.join(config['caps_dir'], 'ccpp_*_cap.F90'),
        os.path.join(config['static_api_dir'], '{api}.F90'.format(api=CCPP_STATIC_API_MODULE)),
        config['static_api_srcfile'],
        ]
    # Not very pythonic, but the easiest way w/o importing another Python module
    cmd = 'rm -vf {0}'.format(' '.join(files_to_remove))
    execute(cmd)
    return success

def get_all_suites(suites_dir):
    success = False
    logging.info("No suites were given, compiling a list of all suites")
    sdfs = []
    for f in os.listdir(suites_dir):
        match = SUITE_DEFINITION_FILENAME_PATTERN.match(f)
        if match:
            logging.info('Adding suite definition file {}'.format(f))
            sdfs.append(f)
    if sdfs:
        success = True
    return (success, sdfs)

def parse_suites(suites_dir, sdfs):
    """Parse suite definition files for prebuild"""
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
    # Parse all scheme files: record metadata, argument list, dependencies, and which scheme is in which file
    metadata_request = {}
    arguments_request = {}
    dependencies_request = {}
    schemes_in_files = {}
    for scheme_file in scheme_files:
        scheme_file_with_abs_path = os.path.abspath(scheme_file)
        (scheme_filepath, scheme_filename) = os.path.split(scheme_file_with_abs_path)
        # Change to directory where scheme_file lives
        os.chdir(scheme_filepath)
        (metadata, arguments, dependencies) = parse_scheme_tables(scheme_filepath, scheme_filename)
        # Record which scheme is in which file
        for scheme in arguments.keys():
            schemes_in_files[scheme] = scheme_file_with_abs_path
        # Merge metadata, append to arguments and dependencies
        metadata_request = merge_dictionaries(metadata_request, metadata)
        arguments_request.update(arguments)
        dependencies_request.update(dependencies)
        os.chdir(BASEDIR)
    # Return to BASEDIR
    os.chdir(BASEDIR)
    return (success, metadata_request, arguments_request, dependencies_request, schemes_in_files)

def filter_metadata(metadata, arguments, dependencies, schemes_in_files, suites):
    """Remove all variables from metadata that are not used in the given suite;
    also remove information on argument lists, dependencies and schemes in files"""
    success = True
    # Output: filtered dictionaries
    metadata_filtered = {}
    arguments_filtered = {}
    dependencies_filtered = {}
    schemes_in_files_filtered = {}
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
        else:
            logging.info("filtering out variable {0}".format(var_name))
    # Filter argument lists
    for scheme in arguments.keys():
        for suite in suites:
            if scheme in suite.all_schemes_called:
                arguments_filtered[scheme] = arguments[scheme]
                break
    # Filter dependencies
    for scheme in dependencies.keys():
        for suite in suites:
            if scheme in suite.all_schemes_called:
                dependencies_filtered[scheme] = dependencies[scheme]
                break
    # Filter schemes_in_files
    for scheme in schemes_in_files.keys():
        for suite in suites:
            if scheme in suite.all_schemes_called:
                schemes_in_files_filtered[scheme] = schemes_in_files[scheme]
    return (success, metadata_filtered, arguments_filtered, dependencies_filtered, schemes_in_files_filtered)

def generate_list_of_schemes_and_dependencies_to_compile(schemes_in_files, dependencies):
    """Generate a flat list of schemes and dependencies to compile"""
    success = True
    # schemes_in_files is a dictionary with key scheme_name and value scheme_file
    # dependencies is a dictionary with key scheme_name and value "list of dependencies"
    schemes_and_dependencies_to_compile = schemes_in_files.values() + \
            [dependency for dependency_list in dependencies.values() for dependency in dependency_list]
    # Remove duplicates
    return (success, list(set(schemes_and_dependencies_to_compile)))

def check_optional_arguments(metadata, arguments, optional_arguments):
    """Check if for each subroutine with optional arguments, an entry exists in the
    optional_arguments dictionary. This is required to generate the caps correctly
    and to assess whether the variable is required from the host model. Optional
    arguments that are not requested by the model as specified in the dictionary
    optional_arguments are deleted from the list of requested data individually
    for each subroutine."""
    logging.info('Checking optional arguments in physics schemes ...')
    success = True

    # First make sure that the CCPP prebuild config entry doesn't contain any variables that are unknown
    # (by standard name), or that it lists variables as optional arguments that aren't declared as optional
    for scheme_name in optional_arguments.keys():
        # Skip modules that have been filtered out (because they are not used by the selected suites, for example)
        if scheme_name in arguments.keys():
            for subroutine_name in optional_arguments[scheme_name].keys():
                # If optional arguments are listed individually, check each of them
                if isinstance(optional_arguments[scheme_name][subroutine_name], list):
                    for var_name in optional_arguments[scheme_name][subroutine_name]:
                        if not var_name in arguments[scheme_name][subroutine_name]:
                            raise Exception("Explicitly requested optional argument '{}' not known to {}/{}".format(
                                                                            var_name, module_name, subroutine_name))
                        else:
                            for var in metadata[var_name][:]:
                                for item in var.container.split(' '):
                                    subitems = item.split('_')
                                    if subitems[0] == 'MODULE':
                                        module_name_test = '_'.join(subitems[1:])
                                    elif subitems[0] == 'SCHEME':
                                        scheme_name_test = '_'.join(subitems[1:])
                                    elif subitems[0] == 'SUBROUTINE':
                                        subroutine_name_test = '_'.join(subitems[1:])
                                    else:
                                        success = False
                                        logging.error('Invalid identifier {0} in container value {1} of requested variable {2}'.format(
                                                                                                 subitems[0], var.container, var_name))
                                if scheme_name_test == scheme_name and subroutine_name_test == subroutine_name and not var.optional in ['t', 'T']:
                                    raise Exception("Variable {} in {} / {}".format(var_name, scheme_name, subroutine_name) + \
                                                " is not an optional argument, but listed as such in the CCPP prebuild config")

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
                        raise Exception('Invalid identifier {0} in container value {1} of requested variable {2}'.format(
                                                                                   subitems[0], var.container, var_name))
                if not scheme_name in optional_arguments.keys() or not \
                        subroutine_name in optional_arguments[scheme_name].keys():
                    raise Exception('No entry found in optional_arguments dictionary for optional argument ' + \
                                    '{0} to subroutine {1} in module {2}'.format(var_name, subroutine_name, scheme_name))
                if type(optional_arguments[scheme_name][subroutine_name]) is list:
                    if var_name in optional_arguments[scheme_name][subroutine_name]:
                        logging.debug('Optional argument {0} to subroutine {1} in module {2} is required, keep in list'.format(
                                                                                       var_name, subroutine_name, scheme_name))
                    else:
                        logging.debug('Optional argument {0} to subroutine {1} in module {2} is not required, remove from list'.format(
                                                                                               var_name, subroutine_name, scheme_name))
                        # Remove this var instance from list of var instances for this var_name
                        metadata[var_name].remove(var)
                        # Remove var_name from list of calling arguments for that subroutine
                        # (unless that module has been filtered out because none of the suites uses it)
                        if scheme_name in arguments.keys():
                            arguments[scheme_name][subroutine_name].remove(var_name)
                elif optional_arguments[scheme_name][subroutine_name] == 'all':
                    logging.debug('optional argument {0} to subroutine {1} in module {2} is required, keep in list'.format(
                                                                                   var_name, subroutine_name, scheme_name))

        # If metadata[var_name] is now empty, i.e. the variable is not
        # requested at all by the model, remove the entry from metadata
        if not metadata[var_name]:
            del metadata[var_name]

    return (success, metadata, arguments)

def compare_metadata(metadata_define, metadata_request):
    """Compare the requested metadata to the defined one. For each requested entry, a
    single (i.e. non-ambiguous entry) must be present in the defined entries. All optional
    arguments that are still in the list of required variables for a scheme are needed,
    since they were checked in the routine check_optional_arguments beforehand."""

    logging.info('Comparing metadata for requested and provided variables ...')
    success = True
    modules = []
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
                # Add to list of required modules
                modules.append('_'.join(subitems[1:]))
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
                          var_name, var.container, target, modules[-1]))
            # Update len=* for character variables
            if var.type == 'character' and var.kind == 'len=*':
                logging.debug('Update kind information for requested variable {0} in {1} from {2} to {3}'.format(var_name,
                                                                                           var.container, var.kind, kind))
                var.kind = kind

    # Remove duplicates from list of modules
    modules = sorted(list(set(modules)))
    return (success, modules, metadata)

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
    """Generate static API for given suite(s)"""
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

def generate_typedefs_makefile(metadata_define, typedefs_makefile, typedefs_cmakefile, typedefs_sourcefile):
    """Generate list of Fortran modules containing CCPP type/kind definitions,
       and create makefile/cmakefile snippets for host model build system"""
    logging.info('Generating list of Fortran modules containing CCPP type definitions ...')
    success = True
    #
    typedefs = []
    # (1) Search for type definitions in the metadata, defined by:
    #    (a) the type not being a standard type, and
    #    (b) the type not being the CCPP framework internal type
    #    (c) the standard_name being identical to the type name
    # (2) Search for kind definitions in the metadata, defined by:
    #    (a) the standard_name starting with "kind_"
    #    (b) the type being integer and the units being none
    for key in metadata_define.keys():
        # derived data types
        if not metadata_define[key][0].type in STANDARD_VARIABLE_TYPES and \
                not metadata_define[key][0].type == CCPP_TYPE and \
                metadata_define[key][0].type == metadata_define[key][0].standard_name:
            container = decode_container_as_dict(metadata_define[key][0].container)
            if not 'MODULE' in container.keys():
                logging.error("Invalid type definition for type {}: {}".format(metadata_define[key][0].type, metadata_define[key][0].print_debug()))
                success = False
                continue
            # Fortran modules are lowercase and have the ending ".mod"
            typedef_fortran_module = "{}.mod".format(container['MODULE']).lower()
            if not typedef_fortran_module in typedefs:
                typedefs.append(typedef_fortran_module)
        # kind definitions
        elif metadata_define[key][0].standard_name.startswith("kind_") and \
                metadata_define[key][0].type == STANDARD_INTEGER_TYPE and \
                metadata_define[key][0].units == 'none':
            container = decode_container_as_dict(metadata_define[key][0].container)
            if not 'MODULE' in container.keys():
                logging.error("Invalid kind definition for kind {}: {}".format(metadata_define[key][0].type, metadata_define[key][0].print_debug()))
                success = False
                continue
            # Fortran modules are lowercase and have the ending ".mod"
            typedef_fortran_module = "{}.mod".format(container['MODULE']).lower()
            if not typedef_fortran_module in typedefs:
                typedefs.append(typedef_fortran_module)

    logging.info('Generating typedefs makefile/cmakefile snippet ...')
    # Write the Fortran modules without path - the build system knows where they are
    makefile = TypedefsMakefile()
    makefile.filename = typedefs_makefile + '.tmp'
    cmakefile = TypedefsCMakefile()
    cmakefile.filename = typedefs_cmakefile + '.tmp'
    sourcefile = TypedefsSourcefile()
    sourcefile.filename = typedefs_sourcefile + '.tmp'
    makefile.write(typedefs)
    cmakefile.write(typedefs)
    sourcefile.write(typedefs)
    if os.path.isfile(typedefs_makefile) and \
            filecmp.cmp(typedefs_makefile, makefile.filename):
        os.remove(makefile.filename)
        os.remove(cmakefile.filename)
        os.remove(sourcefile.filename)
    else:
        if os.path.isfile(typedefs_makefile):
            os.remove(typedefs_makefile)
        if os.path.isfile(typedefs_cmakefile):
            os.remove(typedefs_cmakefile)
        if os.path.isfile(typedefs_sourcefile):
            os.remove(typedefs_sourcefile)
        os.rename(makefile.filename, typedefs_makefile)
        os.rename(cmakefile.filename, typedefs_cmakefile)
        os.rename(sourcefile.filename, typedefs_sourcefile)
    #
    logging.info('Added {0} typedefs to {1}, {2}, {3}'.format(
           len(typedefs), typedefs_makefile, typedefs_cmakefile, typedefs_sourcefile))
    return success

def generate_schemes_makefile(schemes, schemes_makefile, schemes_cmakefile, schemes_sourcefile):
    """Generate makefile/cmakefile snippets for all schemes."""
    logging.info('Generating schemes makefile/cmakefile snippet ...')
    success = True
    makefile = SchemesMakefile()
    makefile.filename = schemes_makefile + '.tmp'
    cmakefile = SchemesCMakefile()
    cmakefile.filename = schemes_cmakefile + '.tmp'
    sourcefile = SchemesSourcefile()
    sourcefile.filename = schemes_sourcefile + '.tmp'
    # Generate list of schemes with absolute path
    schemes_with_abspath = [ os.path.abspath(scheme) for scheme in schemes ]
    makefile.write(schemes_with_abspath)
    cmakefile.write(schemes_with_abspath)
    sourcefile.write(schemes_with_abspath)
    if os.path.isfile(schemes_makefile) and \
            filecmp.cmp(schemes_makefile, makefile.filename):
        os.remove(makefile.filename)
        os.remove(cmakefile.filename)
        os.remove(sourcefile.filename)
    else:
        if os.path.isfile(schemes_makefile):
            os.remove(schemes_makefile)
        if os.path.isfile(schemes_cmakefile):
            os.remove(schemes_cmakefile)
        if os.path.isfile(schemes_sourcefile):
            os.remove(schemes_sourcefile)
        os.rename(makefile.filename, schemes_makefile)
        os.rename(cmakefile.filename, schemes_cmakefile)
        os.rename(sourcefile.filename, schemes_sourcefile)
    #
    logging.info('Added {0} schemes to {1}, {2}, {3}'.format(
           len(schemes_with_abspath), schemes_makefile, schemes_cmakefile, schemes_sourcefile))
    return success

def generate_caps_makefile(caps, caps_makefile, caps_cmakefile, caps_sourcefile, caps_dir):
    """Generate makefile/cmakefile snippets for all caps."""
    logging.info('Generating caps makefile/cmakefile snippet ...')
    success = True
    makefile = CapsMakefile()
    makefile.filename = caps_makefile + '.tmp'
    cmakefile = CapsCMakefile()
    cmakefile.filename = caps_cmakefile + '.tmp'
    sourcefile = CapsSourcefile()
    sourcefile.filename = caps_sourcefile + '.tmp'
    # Generate list of caps with absolute path
    caps_with_abspath = [ os.path.abspath(os.path.join(caps_dir, cap)) for cap in caps ]
    makefile.write(caps_with_abspath)
    cmakefile.write(caps_with_abspath)
    sourcefile.write(caps_with_abspath)
    if os.path.isfile(caps_makefile) and \
            filecmp.cmp(caps_makefile, makefile.filename):
        os.remove(makefile.filename)
        os.remove(cmakefile.filename)
        os.remove(sourcefile.filename)
    else:
        if os.path.isfile(caps_makefile):
            os.remove(caps_makefile)
        if os.path.isfile(caps_cmakefile):
            os.remove(caps_cmakefile)
        if os.path.isfile(caps_sourcefile):
            os.remove(caps_sourcefile)
        os.rename(makefile.filename, caps_makefile)
        os.rename(cmakefile.filename, caps_cmakefile)
        os.rename(sourcefile.filename, caps_sourcefile)
    #
    logging.info('Added {0} auto-generated caps to {1} and {2}, {3}'.format(
           len(caps_with_abspath), caps_makefile, caps_cmakefile, caps_sourcefile))
    return success

def main():
    """Main routine that handles the CCPP prebuild for different host models."""
    # Parse command line arguments
    (success, configfile, clean, debug, sdfs, builddir) = parse_arguments()
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
        success = clean_files(config)
        logging.info('CCPP prebuild clean completed successfully, exiting.')
        sys.exit(0)

    # If no suite definition files were given, get all of them
    if not sdfs:
        (success, sdfs) = get_all_suites(config['suites_dir'])
        if not success:
            raise Exception('Call to get_all_sdfs failed.')

    # Parse suite definition files for prebuild
    (success, suites) = parse_suites(config['suites_dir'], sdfs)
    if not success:
        raise Exception('Parsing suite definition files failed.')

    # Variables defined by the host model
    (success, metadata_define) = gather_variable_definitions(config['variable_definition_files'], config['typedefs_new_metadata'])
    if not success:
        raise Exception('Call to gather_variable_definitions failed.')

    # Create an HTML table with all variables provided by the model
    success = metadata_to_html(metadata_define, config['host_model'], config['html_vartable_file'])
    if not success:
        raise Exception('Call to metadata_to_html failed.')

    # Variables requested by the CCPP physics schemes
    (success, metadata_request, arguments_request, dependencies_request, schemes_in_files) = collect_physics_subroutines(config['scheme_files'])
    if not success:
        raise Exception('Call to collect_physics_subroutines failed.')

    # Filter metadata/arguments - remove whatever is not included in suite definition files
    (success, metadata_request, arguments_request, dependencies_request, schemes_in_files) = filter_metadata(
                         metadata_request, arguments_request, dependencies_request, schemes_in_files, suites)
    if not success:
        raise Exception('Call to filter_metadata failed.')

    (success, schemes_and_dependencies_to_compile) = generate_list_of_schemes_and_dependencies_to_compile(schemes_in_files, dependencies_request)
    if not success:
        raise Exception('Call to generate_list_of_schemes_and_dependencies_to_compile failed.')

    # Process optional arguments based on configuration in above dictionary optional_arguments
    (success, metadata_request, arguments_request) = check_optional_arguments(metadata_request,arguments_request,
                                                                              config['optional_arguments'])
    if not success:
        raise Exception('Call to check_optional_arguments failed.')

    # Create a LaTeX table with all variables requested by the pool of physics and/or provided by the host model
    success = metadata_to_latex(metadata_define, metadata_request, config['host_model'], config['latex_vartable_file'])
    if not success:
        raise Exception('Call to metadata_to_latex failed.')

    # Check requested against defined arguments to generate metadata (list/dict of variables for CCPP)
    (success, modules, metadata) = compare_metadata(metadata_define, metadata_request)
    if not success:
        raise Exception('Call to compare_metadata failed.')

    # Add Fortran module files of typedefs to makefile/cmakefile/shell script
    success = generate_typedefs_makefile(metadata_define, config['typedefs_makefile'],
                                         config['typedefs_cmakefile'], config['typedefs_sourcefile'])
    if not success:
        raise Exception('Call to generate_typedefs_makefile failed.')

    # Add filenames of schemes and variable definition files (types) to makefile/cmakefile/shell script
    success = generate_schemes_makefile(schemes_and_dependencies_to_compile + config['variable_definition_files'],
                                        config['schemes_makefile'], config['schemes_cmakefile'],
                                        config['schemes_sourcefile'])
    if not success:
        raise Exception('Call to generate_schemes_makefile failed.')

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

    # Add filenames of caps to makefile/cmakefile/shell script
    all_caps = suite_and_group_caps

    success = generate_caps_makefile(all_caps, config['caps_makefile'], config['caps_cmakefile'],
                                     config['caps_sourcefile'], config['caps_dir'])
    if not success:
        raise Exception('Call to generate_caps_makefile failed.')

    logging.info('CCPP prebuild step completed successfully.')

if __name__ == '__main__':
    main()
