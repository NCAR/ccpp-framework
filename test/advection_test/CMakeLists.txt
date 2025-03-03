CMAKE_MINIMUM_REQUIRED(VERSION 3.10)
PROJECT(test_host)
ENABLE_LANGUAGE(Fortran)

include(CMakeForceCompiler)

SET(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${CMAKE_SOURCE_DIR}/cmake/modules)

#------------------------------------------------------------------------------
#
# Set where the CCPP Framework lives
#
#------------------------------------------------------------------------------
get_filename_component(TEST_ROOT "${CMAKE_SOURCE_DIR}" DIRECTORY)
get_filename_component(CCPP_ROOT "${TEST_ROOT}" DIRECTORY)
#------------------------------------------------------------------------------
#
# Create list of SCHEME_FILES, HOST_FILES, and SUITE_FILES
# Paths should be relative to CMAKE_SOURCE_DIR (this file's directory)
#
#------------------------------------------------------------------------------
LIST(APPEND SCHEME_FILES "cld_suite_files.txt")
LIST(APPEND SCHEME_FILES_ERROR "cld_suite_files_error.txt")
LIST(APPEND HOST_FILES  "test_host_data" "test_host_mod")
LIST(APPEND SUITE_FILES "cld_suite.xml")
LIST(APPEND SUITE_FILES_ERROR "cld_suite_error.xml")
# HOST is the name of the executable we will build.
# We assume there are files ${HOST}.meta and ${HOST}.F90 in CMAKE_SOURCE_DIR
SET(HOST "${CMAKE_PROJECT_NAME}")

#------------------------------------------------------------------------------
#
# End of project-specific input
#
#------------------------------------------------------------------------------

# By default, no verbose output
SET(VERBOSITY 0 CACHE STRING "Verbosity level of output (default: 0)")
# By default, generated caps go in ccpp subdir
SET(CCPP_CAP_FILES "${CMAKE_BINARY_DIR}/ccpp" CACHE
  STRING "Location of CCPP-generated cap files")

SET(CCPP_FRAMEWORK ${CCPP_ROOT}/scripts)

# Use rpaths on MacOSX
set(CMAKE_MACOSX_RPATH 1)

#------------------------------------------------------------------------------
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
  message (WARNING "This program has only been compiled with gfortran, pgf90 and ifort. If another compiler is needed, the appropriate flags SHOULD be added in ${CMAKE_SOURCE_DIR}/CMakeLists.txt")
endif (${CMAKE_Fortran_COMPILER_ID} MATCHES "GNU")

#------------------------------------------------------------------------------
# CMake Modules
# Set the CMake module path
list(APPEND CMAKE_MODULE_PATH "${CCPP_FRAMEWORK}/cmake")
#------------------------------------------------------------------------------
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
FOREACH(FILE ${SCHEME_FILES})
  FILE(STRINGS ${FILE} FILENAMES)
  LIST(APPEND SCHEME_FILENAMES ${FILENAMES})
ENDFOREACH(FILE)
string(REPLACE ";" "," SCHEME_METADATA "${SCHEME_FILES}")

# Create metadata and source file lists
FOREACH(FILE ${SCHEME_FILES_ERROR})
  FILE(STRINGS ${FILE} FILENAMES)
  LIST(APPEND SCHEME_FILENAMES_ERROR ${FILENAMES})
ENDFOREACH(FILE)
string(REPLACE ";" "," SCHEME_METADATA_ERROR "${SCHEME_FILES_ERROR}")

FOREACH(FILE ${SCHEME_FILENAMES})
  # target_sources prefers absolute pathnames
  string(REPLACE ".meta" ".F90" TEMP "${FILE}")
  get_filename_component(ABS_PATH "${TEMP}" ABSOLUTE)
  list(APPEND LIBRARY_LIST ${ABS_PATH})
ENDFOREACH(FILE)

FOREACH(FILE ${HOST_FILES})
  LIST(APPEND HOST_METADATA "${FILE}.meta")
  # target_sources prefers absolute pathnames
  get_filename_component(ABS_PATH "${FILE}.F90" ABSOLUTE)
  LIST(APPEND HOST_SOURCE   "${ABS_PATH}")
ENDFOREACH(FILE)
list(APPEND LIBRARY_LIST ${HOST_SOURCE})
string(REPLACE ";" ".meta," HOST_METADATA "${HOST_FILES}")
set(HOST_METADATA "${HOST_METADATA}.meta,${HOST}.meta")

string(REPLACE ";" "," SUITE_XML "${SUITE_FILES}")
string(REPLACE ";" "," SUITE_XML_ERROR "${SUITE_FILES_ERROR}")

# Run ccpp_capgen that we expect to fail
set(CAPGEN_CMD "${CCPP_FRAMEWORK}/ccpp_capgen.py")
list(APPEND CAPGEN_CMD "--host-files")
list(APPEND CAPGEN_CMD "${HOST_METADATA}")
list(APPEND CAPGEN_CMD "--scheme-files")
list(APPEND CAPGEN_CMD "${SCHEME_METADATA_ERROR}")
list(APPEND CAPGEN_CMD "--suites")
list(APPEND CAPGEN_CMD "${SUITE_XML_ERROR}")
list(APPEND CAPGEN_CMD "--host-name")
list(APPEND CAPGEN_CMD "test_host")
list(APPEND CAPGEN_CMD "--output-root")
list(APPEND CAPGEN_CMD "${CCPP_CAP_FILES}")
while (VERBOSITY GREATER 0)
  list(APPEND CAPGEN_CMD "--verbose")
  MATH(EXPR VERBOSITY "${VERBOSITY} - 1")
endwhile ()
list(APPEND CAPGEN_CMD "--debug")
string(REPLACE ";" " " CAPGEN_STRING "${CAPGEN_CMD}")
MESSAGE(STATUS "Running: ${CAPGEN_STRING}")
EXECUTE_PROCESS(COMMAND ${CAPGEN_CMD} WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
  OUTPUT_VARIABLE CAPGEN_OUT ERROR_VARIABLE CAPGEN_OUT RESULT_VARIABLE RES)
MESSAGE(STATUS "${CAPGEN_OUT}")
if (RES EQUAL 0)
  MESSAGE(STATUS "CCPP cap generation completed")
else()
  # Example: Validate the error message
  string(FIND "${CAPGEN_OUT}" "Variables of type ccpp_constituent_properties_t only allowed in register phase" ERROR_INDEX)

  if (ERROR_INDEX GREATER -1)
    MESSAGE(STATUS "Capgen build produces expected error message.")
  else()
    MESSAGE(FATAL_ERROR "CCPP cap generation did not generate expected error. Expected 'Variables of type ccpp_cosntituent_properties_t only allowed in register phase. Got: " ${CAPGEN_OUT}"")
  endif()
endif(RES EQUAL 0)

# Run ccpp_capgen
set(CAPGEN_CMD "${CCPP_FRAMEWORK}/ccpp_capgen.py")
list(APPEND CAPGEN_CMD "--host-files")
list(APPEND CAPGEN_CMD "${HOST_METADATA}")
list(APPEND CAPGEN_CMD "--scheme-files")
list(APPEND CAPGEN_CMD "${SCHEME_METADATA}")
list(APPEND CAPGEN_CMD "--suites")
list(APPEND CAPGEN_CMD "${SUITE_XML}")
list(APPEND CAPGEN_CMD "--host-name")
list(APPEND CAPGEN_CMD "test_host")
list(APPEND CAPGEN_CMD "--output-root")
list(APPEND CAPGEN_CMD "${CCPP_CAP_FILES}")
while (VERBOSITY GREATER 0)
  list(APPEND CAPGEN_CMD "--verbose")
  MATH(EXPR VERBOSITY "${VERBOSITY} - 1")
endwhile ()
list(APPEND CAPGEN_CMD "--debug")
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
message(STATUS "CCPP_CAPS = ${CCPP_CAPS}")
if (RES EQUAL 0)
  MESSAGE(STATUS "CCPP cap files retrieved")
else(RES EQUAL 0)
  MESSAGE(FATAL_ERROR "CCPP cap file retrieval FAILED: result = ${RES}")
endif(RES EQUAL 0)
list(APPEND LIBRARY_LIST ${CCPP_CAPS})
add_library(TESTLIB OBJECT  ${LIBRARY_LIST})
ADD_EXECUTABLE(${HOST} ${HOST}.F90 $<TARGET_OBJECTS:TESTLIB>)

INCLUDE_DIRECTORIES(${CCPP_CAP_FILES})

set_target_properties(${HOST} PROPERTIES
                              COMPILE_FLAGS "${CMAKE_Fortran_FLAGS}"
                              LINK_FLAGS "${CMAKE_Fortran_FLAGS}")
