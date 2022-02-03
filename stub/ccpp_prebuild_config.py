#!/usr/bin/env python

# CCPP prebuild config for GFDL Finite-Volume Cubed-Sphere Model (FV3)


###############################################################################
# Definitions                                                                 #
###############################################################################

HOST_MODEL_IDENTIFIER = "FV3"

# Add all files with metadata tables on the host model side and in CCPP,
# relative to basedir = top-level directory of host model. This includes
# kind and type definitions used in CCPP physics. Also add any internal
# dependencies of these files to the list.
VARIABLE_DEFINITION_FILES = [
    # actual variable definition files
    '../src/ccpp_types.F90',
    'data.F90',
    ]

TYPEDEFS_NEW_METADATA = {
    'ccpp_types' : {
        'ccpp_types' : '',
        'ccpp_t' : 'ccpp_data',
        },
    }

# Add all physics scheme files relative to basedir
SCHEME_FILES = [
    'stub.F90',
    ]

# Default build dir, relative to current working directory,
# if not specified as command-line argument
DEFAULT_BUILD_DIR = '.'

# Auto-generated makefile/cmakefile snippets that contain all type definitions
TYPEDEFS_MAKEFILE   = '{build_dir}/CCPP_TYPEDEFS.mk'
TYPEDEFS_CMAKEFILE  = '{build_dir}/CCPP_TYPEDEFS.cmake'
TYPEDEFS_SOURCEFILE = '{build_dir}/CCPP_TYPEDEFS.sh'

# Auto-generated makefile/cmakefile snippets that contain all schemes
SCHEMES_MAKEFILE   = '{build_dir}/CCPP_SCHEMES.mk'
SCHEMES_CMAKEFILE  = '{build_dir}/CCPP_SCHEMES.cmake'
SCHEMES_SOURCEFILE = '{build_dir}/CCPP_SCHEMES.sh'

# Auto-generated makefile/cmakefile snippets that contain all caps
CAPS_MAKEFILE   = '{build_dir}/CCPP_CAPS.mk'
CAPS_CMAKEFILE  = '{build_dir}/CCPP_CAPS.cmake'
CAPS_SOURCEFILE = '{build_dir}/CCPP_CAPS.sh'

# Directory where to put all auto-generated physics caps
CAPS_DIR = '{build_dir}'

# Directory where the suite definition files are stored
SUITES_DIR = '{build_dir}'

# Optional arguments - only required for schemes that use
# optional arguments. ccpp_prebuild.py will throw an exception
# if it encounters a scheme subroutine with optional arguments
# if no entry is made here. Possible values are: 'all', 'none',
# or a list of standard_names: [ 'var1', 'var3' ].
OPTIONAL_ARGUMENTS = {}

# Directory where to write static API to
STATIC_API_DIR = '{build_dir}'
STATIC_API_CMAKEFILE  = '{build_dir}/CCPP_API.cmake'
STATIC_API_SOURCEFILE = '{build_dir}/CCPP_API.sh'

# Directory for writing HTML pages generated from metadata files
METADATA_HTML_OUTPUT_DIR = '{build_dir}'

# HTML document containing the model-defined CCPP variables
HTML_VARTABLE_FILE = '{build_dir}/CCPP_VARIABLES_STUB.html'

# LaTeX document containing the provided vs requested CCPP variables
LATEX_VARTABLE_FILE = '{build_dir}/CCPP_VARIABLES_STUB.tex'
