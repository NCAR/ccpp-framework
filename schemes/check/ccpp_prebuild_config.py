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

# Auto-generated makefile/cmakefile snippets that contain all schemes
SCHEMES_MAKEFILE = '/dev/null'
SCHEMES_CMAKEFILE = '/dev/null'

# CCPP host cap in which to insert the ccpp_field_add statements;
# determines the directory to place ccpp_{modules,fields}.inc
TARGET_FILES = [
    '../../../../../src/tests/test_check.f90',
    ]

# Auto-generated makefile/cmakefile snippets that contain all caps
CAPS_MAKEFILE = '/dev/null'
CAPS_CMAKEFILE = '/dev/null'

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

# HTML document containing the model-defined CCPP variables
HTML_VARTABLE_FILE = 'CCPP_VARIABLES_FV3.html'

# LaTeX document containing the provided vs requested CCPP variables
LATEX_VARTABLE_FILE = 'CCPP_VARIABLES_FV3.tex'


###############################################################################
# Template code to generate include files                                     #
###############################################################################

# Name of the CCPP data structure in the host model cap;
# in the case of FV3, this is a 2-dimensional array with
# the number of blocks as the first and the number of
# OpenMP threads as the second dimension; nb is the loop
# index for the current block, nt for the current thread
CCPP_DATA_STRUCTURE = 'cdata'
