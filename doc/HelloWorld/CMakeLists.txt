CMAKE_MINIMUM_REQUIRED(VERSION 2.8)
PROJECT(HelloWorld)
ENABLE_LANGUAGE(Fortran)

include(CMakeForceCompiler)

SET(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${CMAKE_SOURCE_DIR}/cmake/modules)

#-----------------------------------------------------------------------------
#
# Create list of SCHEME_FILES, HOST_FILES, and SUITE_FILES
# Paths should be relative to CMAKE_SOURCE_DIR (this file's directory)
#
#-----------------------------------------------------------------------------
SET(SCHEME_FILES "hello_scheme_files.txt")
FILE(STRINGS ${SCHEME_FILES} SCHEME_FILENAMES)
LIST(APPEND HOST_FILES  "hello_world_mod")
LIST(APPEND SUITE_FILES "hello_world_suite.xml")
# HOST is the name of the executable we will build.
SET(HOST "${CMAKE_PROJECT_NAME}")
# HOST_PROGRAM is the name of the program source files (Fortran and metadata)
SET(HOST_PROGRAM "hello_world_host")

#-----------------------------------------------------------------------------
#
# Set where the CCPP Framework lives
#
#-----------------------------------------------------------------------------
get_filename_component(HELLO_ROOT "${CMAKE_SOURCE_DIR}" DIRECTORY)
get_filename_component(CCPP_ROOT "${HELLO_ROOT}" DIRECTORY)

#-----------------------------------------------------------------------------
##############################################################################
#
# End of project-specific input
#
##############################################################################
#-----------------------------------------------------------------------------

# By default, no verbose output
SET(VERBOSITY 0 CACHE STRING "Verbosity level of output (default: 0)")
# By default, generated caps go in ccpp subdir
SET(CCPP_CAP_FILES "${CMAKE_BINARY_DIR}/ccpp" CACHE
  STRING "Location of CCPP-generated cap files")

SET(CCPP_FRAMEWORK ${CCPP_ROOT}/scripts)

# Use rpaths on MacOSX
set(CMAKE_MACOSX_RPATH 1)

#-----------------------------------------------------------------------------
# Set a default build type if none was specified
if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
    #message(STATUS "Setting build type to 'Debug' as none was specified.")
    #set(CMAKE_BUILD_TYPE Debug CACHE STRING "Choose the type of build." FORCE)
    message(STATUS "Setting build type to 'Release' as none was specified.")
    set(CMAKE_BUILD_TYPE Release CACHE STRING "Choose the type of build." FORCE)

    # Set the possible values of build type for cmake-gui
    set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release"
                 "MinSizeRel" "RelWithDebInfo")
endif()

ADD_COMPILE_OPTIONS(-O0)

# CMake 3 version: ADD_EXECUTABLE(${HOST} ${HOST_PROGRAM}.F90)
if (${CMAKE_Fortran_COMPILER_ID} MATCHES "GNU")
#  gfortran
#  MESSAGE("gfortran being used.")
  ADD_COMPILE_OPTIONS(-fcheck=all)
  ADD_COMPILE_OPTIONS(-fbacktrace)
  ADD_COMPILE_OPTIONS(-ffpe-trap=zero)
  ADD_COMPILE_OPTIONS(-finit-real=nan)
  ADD_COMPILE_OPTIONS(-ggdb)
  ADD_COMPILE_OPTIONS(-ffree-line-length-none)
  ADD_COMPILE_OPTIONS(-cpp)
elseif (${CMAKE_Fortran_COMPILER_ID} MATCHES "Intel")
#  ifort
#  MESSAGE("ifort being used.")
  #ADD_COMPILE_OPTIONS(-check all)
  ADD_COMPILE_OPTIONS(-fpe0)
  ADD_COMPILE_OPTIONS(-warn)
  ADD_COMPILE_OPTIONS(-traceback)
  ADD_COMPILE_OPTIONS(-debug extended)
  ADD_COMPILE_OPTIONS(-fpp)
elseif (${CMAKE_Fortran_COMPILER_ID} MATCHES "PGI")
#  pgf90
#  MESSAGE("pgf90 being used.")
  ADD_COMPILE_OPTIONS(-g)
  ADD_COMPILE_OPTIONS(-Mipa=noconst)
  ADD_COMPILE_OPTIONS(-traceback)
  ADD_COMPILE_OPTIONS(-Mfree)
  ADD_COMPILE_OPTIONS(-Mfptrap)
  ADD_COMPILE_OPTIONS(-Mpreprocess)
else (${CMAKE_Fortran_COMPILER_ID} MATCHES "GNU")
  message (FATAL_ERROR "This program has only been compiled with gfortran, pgf90 and ifort. If another compiler is needed, the appropriate flags must be added in ${CMAKE_SOURCE_DIR}/CMakeLists.txt")
endif (${CMAKE_Fortran_COMPILER_ID} MATCHES "GNU")

#-----------------------------------------------------------------------------
# CMake Modules
# Set the CMake module path
list(APPEND CMAKE_MODULE_PATH "${CCPP_FRAMEWORK}/cmake")
#-----------------------------------------------------------------------------
# Set OpenMP flags for C/C++/Fortran
if (OPENMP)
  include(detect_openmp)
  detect_openmp()
  set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${OpenMP_C_FLAGS}")
  set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${OpenMP_CXX_FLAGS}")
  set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${OpenMP_Fortran_FLAGS}")
  message(STATUS "Enable OpenMP support for C/C++/Fortran compiler")
else(OPENMP)
  message (STATUS "Disable OpenMP support for C/C++/Fortran compiler")
endif()

# Create metadata and source file lists
FOREACH(FILE ${SCHEME_FILENAMES})
  # add absolute pathnames to library  list
  string(REPLACE ".meta" ".F90" TEMP "${FILE}")
  get_filename_component(ABS_PATH "${TEMP}" ABSOLUTE)
  list(APPEND LIBRARY_LIST ${ABS_PATH})
  # CMake 3 version: target_sources(${HOST} PUBLIC ${ABS_PATH})
ENDFOREACH(FILE)

FOREACH(FILE ${HOST_FILES})
  LIST(APPEND HOST_METADATA "${FILE}.meta")
  # add absolute pathnames to library list
  get_filename_component(ABS_PATH "${FILE}.F90" ABSOLUTE)
  LIST(APPEND HOST_SOURCE   "${ABS_PATH}")
ENDFOREACH(FILE)
list(APPEND LIBRARY_LIST ${HOST_SOURCE})
# CMake 3 version: target_sources(${HOST} PUBLIC ${HOST_SOURCE})
string(REPLACE ";" ".meta," HOST_METADATA "${HOST_FILES}")
set(HOST_METADATA "${HOST_PROGRAM}.meta,${HOST_METADATA}.meta")

# Run ccpp_capgen
set(CAPGEN_CMD "${CCPP_FRAMEWORK}/ccpp_capgen.py")
list(APPEND CAPGEN_CMD "--host-files")
list(APPEND CAPGEN_CMD "${HOST_METADATA}")
list(APPEND CAPGEN_CMD "--scheme-files")
list(APPEND CAPGEN_CMD "${SCHEME_FILES}")
list(APPEND CAPGEN_CMD "--suites")
list(APPEND CAPGEN_CMD "${SUITE_FILES}")
list(APPEND CAPGEN_CMD "--output-root")
list(APPEND CAPGEN_CMD "${CCPP_CAP_FILES}")
list(APPEND CAPGEN_CMD "--host-name")
list(APPEND CAPGEN_CMD "${HOST}")
while (VERBOSITY GREATER 0)
  list(APPEND CAPGEN_CMD "--verbose")
  MATH(EXPR VERBOSITY "${VERBOSITY} - 1")
endwhile ()
string(REPLACE ";" " " CAPGEN_STRING "${CAPGEN_CMD}")
MESSAGE(STATUS "Running: ${CAPGEN_STRING}")
EXECUTE_PROCESS(COMMAND ${CAPGEN_CMD} WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
  OUTPUT_VARIABLE CAPGEN_OUT ERROR_VARIABLE CAPGEN_OUT RESULT_VARIABLE RES)
MESSAGE(STATUS "${CAPGEN_OUT}")
if (RES EQUAL 0)
  MESSAGE(STATUS "CCPP cap generation completed")
else(RES EQUAL 0)
  MESSAGE(FATAL_ERROR "CCPP cap generation FAILED: result = ${RES}")
endif(RES EQUAL 0)

# Retrieve the list of files from datatable.xml and set to CCPP_CAPS
set(DTABLE_CMD "${CCPP_FRAMEWORK}/ccpp_datafile.py")
list(APPEND DTABLE_CMD "${CCPP_CAP_FILES}/datatable.xml")
list(APPEND DTABLE_CMD "--ccpp-files")
list(APPEND DTABLE_CMD "--separator=\\;")
string(REPLACE ";" " " DTABLE_STRING "${DTABLE_CMD}")
MESSAGE(STATUS "Running: ${DTABLE_STRING}")
EXECUTE_PROCESS(COMMAND ${DTABLE_CMD} OUTPUT_VARIABLE CCPP_CAPS
                RESULT_VARIABLE RES
                OUTPUT_STRIP_TRAILING_WHITESPACE ERROR_STRIP_TRAILING_WHITESPACE)
if (RES EQUAL 0)
  MESSAGE(STATUS "CCPP cap files retrieved")
else(RES EQUAL 0)
  MESSAGE(FATAL_ERROR "CCPP cap file retrieval FAILED: result = ${RES}")
endif(RES EQUAL 0)
FOREACH(FILE ${CCPP_CAPS})
  list(APPEND LIBRARY_LIST ${FILE})
ENDFOREACH(FILE)
# CMake 3 version: target_sources(${HOST} PUBLIC ${CCPP_CAPS})
add_library(HELLOLIB OBJECT  ${LIBRARY_LIST})
ADD_EXECUTABLE(${HOST} ${HOST_PROGRAM}.F90 $<TARGET_OBJECTS:HELLOLIB>)

INCLUDE_DIRECTORIES(${CCPP_CAP_FILES})

set_target_properties(${HOST} PROPERTIES
                              COMPILE_FLAGS "${CMAKE_Fortran_FLAGS}"
                              LINK_FLAGS "${CMAKE_Fortran_FLAGS}")
