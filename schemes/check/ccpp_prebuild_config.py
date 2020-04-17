#!/usr/bin/env python

# CCPP prebuild config for unit tests


###############################################################################
# Definitions                                                                 #
###############################################################################

HOST_MODEL_IDENTIFIER = 'TEST'

# Add all files with metadata tables on the host model side,
# relative to basedir = top-level directory of host model
VARIABLE_DEFINITION_FILES = [
    '../../../../../src/tests/test_check.f90',
    ]

# Add all physics scheme dependencies relative to basedir - note that these are all violations
# of the CCPP requirement to not use any external modules except Fortran standard modules!
SCHEME_FILES_DEPENDENCIES = [
    ]

# Add all physics scheme files relative to basedir
SCHEME_FILES = {
    # Relative path to source (from where ccpp_prebuild.py is called) : [ list of physics sets in which scheme may be called ];
    # current restrictions are that each scheme can only belong to one physics set, and all schemes within one group in the
    # suite definition file have to belong to the same physics set
    '../../../../../schemes/check/check_test.f90' : [ 'test' ],
    }

# Default build dir, relative to current working directory,
# if not specified as command-line argument
DEFAULT_BUILD_DIR = '.'

# Auto-generated makefile/cmakefile snippets that contain all type definitions
TYPEDEFS_MAKEFILE   = 'CCPP_TYPEDEFS.mk'
TYPEDEFS_CMAKEFILE  = 'CCPP_TYPEDEFS.cmake'
TYPEDEFS_SOURCEFILE = 'CCPP_TYPEDEFS.sh'

# Auto-generated makefile/cmakefile snippets that contain all schemes
SCHEMES_MAKEFILE = 'CCPP_SCHEMES.mk'
SCHEMES_CMAKEFILE = 'CCPP_SCHEMES.cmake'
SCHEMES_SOURCEFILE = 'CCPP_SCHEMES.sh'

# CCPP host cap in which to insert the ccpp_field_add statements;
# determines the directory to place ccpp_{modules,fields}.inc
TARGET_FILES = [
    '../../../../../src/tests/test_check.f90',
    ]

# Auto-generated makefile/cmakefile snippets that contain all caps
CAPS_MAKEFILE = 'CCPP_CAPS.mk'
CAPS_CMAKEFILE = 'CCPP_CAPS.cmake'
CAPS_SOURCEFILE = 'CCPP_CAPS.sh'

# Directory where to put all auto-generated physics caps
CAPS_DIR = '.'

# Directory where the suite definition files are stored
SUITES_DIR = '../../../../../src/tests'

# Optional arguments - only required for schemes that use
# optional arguments. ccpp_prebuild.py will throw an exception
# if it encounters a scheme subroutine with optional arguments
# if no entry is made here. Possible values are: 'all', 'none',
# or a list of standard_names: [ 'var1', 'var3' ].
OPTIONAL_ARGUMENTS = {
    'test' : {
        'test_run' : [ 'surface_skin_temperature' ],
        },
    #'subroutine_name_1' : 'all',
    #'subroutine_name_2' : 'none',
    #'subroutine_name_2' : [ 'var1', 'var3'],
    }

# Names of Fortran include files in the host model cap (do not change);
# both files will be written to the directory of each target file
MODULE_INCLUDE_FILE = 'ccpp_modules_{set}.inc'
FIELDS_INCLUDE_FILE = 'ccpp_fields_{set}.inc'

# Directory where to write static API to
STATIC_API_DIR = '.'
STATIC_API_SRCFILE = './CCPP_STATIC_API.sh'

# HTML document containing the model-defined CCPP variables
HTML_VARTABLE_FILE = 'CCPP_VARIABLES_FV3.html'

# LaTeX document containing the provided vs requested CCPP variables
LATEX_VARTABLE_FILE = 'CCPP_VARIABLES_FV3.tex'


###############################################################################
# Template code to generate include files                                     #
###############################################################################

# Name of the CCPP data structure in the host model cap
CCPP_DATA_STRUCTURE = 'cdata'
