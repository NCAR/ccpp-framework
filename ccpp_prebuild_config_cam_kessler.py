#!/usr/bin/env python

# CCPP prebuild config for simple CAM physics 


###############################################################################
# Definitions                                                                 #
###############################################################################

# Add all files with metadata tables on the host model side,
# relative to basedir = top-level directory of host model
VARIABLE_DEFINITION_FILES = [
    'cam_driver/src/cam_var_defs.f90',
    ]

# Can be empty, since all physics schemes and their
# dependencies are hardcoded in CMakeLists in
# ccpp-physics - to fix, c.f. FV3 v1
SCHEME_FILES_DEPENDENCIES = []

# Add all physics scheme files relative to basedir
SCHEME_FILES = [
    'simple/src/kessler.F90',
    ]

# Auto-generated makefile/cmakefile snippets that contain all schemes
SCHEMES_MAKEFILE = 'simple/CCPP_SCHEMES.mk'
SCHEMES_CMAKEFILE = 'simple/CCPP_SCHEMES.cmake'

# CCPP host cap in which to insert the ccpp_field_add statements;
# determines the directory to place ccpp_{modules,fields}.inc
TARGET_FILES = [
    'cam_driver/src/cam_kessler.f90',
    ]

# Auto-generated makefile/cmakefile snippets that contain all caps
CAPS_MAKEFILE = 'simple/CCPP_CAPS.mk'
CAPS_CMAKEFILE = 'simple/CCPP_CAPS.cmake'

# Directory where to put all auto-generated physics caps
CAPS_DIR = 'simple/physics'

# Optional arguments - only required for schemes that use
# optional arguments. ccpp_prebuild.py will throw an exception
# if it encounters a scheme subroutine with optional arguments
# if no entry is made here. Possible values are: 'all', 'none',
# or a list of standard_names: [ 'var1', 'var3' ].
OPTIONAL_ARGUMENTS = {
    }

# Names of Fortran include files in the host model cap (do not change);
# both files will be written to the directory of each target file
MODULE_INCLUDE_FILE = 'ccpp_modules.inc'
FIELDS_INCLUDE_FILE = 'ccpp_fields.inc'

# HTML document containing the model-defined CCPP variables
HTML_VARTABLE_FILE = 'simple/CCPP_VARIABLES_simple.html'

# LaTeX document containing the provided vs requested CCPP variables
LATEX_VARTABLE_FILE = 'ccpp-framework/doc/DevelopersGuide/CCPP_VARIABLES_simple.tex'


###############################################################################
# Template code to generate include files                                     #
###############################################################################

# Name of the CCPP data structure in the host model cap;
# in the case of simple CAM physics, this is a vector with loop index i
CCPP_DATA_STRUCTURE = 'cdata(i)'

# Modules to load for auto-generated ccpp_field_add code
# in the host model cap (e.g. error handling)
MODULE_USE_TEMPLATE_HOST_CAP = \
'''
use ccpp_errors, only: ccpp_error
'''

# Modules to load for auto-generated ccpp_field_get code
# in the physics scheme cap (e.g. derived data types)
MODULE_USE_TEMPLATE_SCHEME_CAP = \
'''
   use machine, only: kind_phys
'''
