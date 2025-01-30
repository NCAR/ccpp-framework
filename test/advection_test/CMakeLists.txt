
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
SET(HOST "test_host")

#------------------------------------------------------------------------------
#
# End of project-specific input
#
#------------------------------------------------------------------------------

# By default, generated caps go in ccpp subdir
SET(CCPP_CAP_FILES "${CMAKE_CURRENT_BINARY_DIR}/ccpp" CACHE
  STRING "Location of CCPP-generated cap files")

SET(CCPP_FRAMEWORK ${CMAKE_SOURCE_DIR}/scripts)

# Create metadata and source file lists
FOREACH(FILE ${SCHEME_FILES})
  FILE(STRINGS ${FILE} FILENAMES)
  LIST(APPEND SCHEME_FILENAMES ${FILENAMES})
ENDFOREACH(FILE)

# Create metadata and source file lists
FOREACH(FILE ${SCHEME_FILES_ERROR})
  FILE(STRINGS ${FILE} FILENAMES)
  LIST(APPEND SCHEME_FILENAMES_ERROR ${FILENAMES})
ENDFOREACH(FILE)

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

# Run ccpp_capgen that we expect to fail
ccpp_capgen(CAPGEN_EXPECT_THROW_ERROR ON
            CAPGEN_DEBUG ON
            VERBOSITY ${CCPP_VERBOSITY}
            HOSTFILES ${HOST_METADATA}
            SCHEMEFILES ${SCHEME_METADATA_ERROR}
            SUITES ${SUITE_FILES_ERROR}
            HOST_NAME "test_host"
            OUTPUT_ROOT "${CCPP_CAP_FILES}")

# Run ccpp_capgen
ccpp_capgen(CAPGEN_DEBUG ON
            VERBOSITY ${CCPP_VERBOSITY}
            HOSTFILES ${HOST_METADATA}
            SCHEMEFILES ${SCHEME_METADATA}
            SUITES ${SUITE_FILES}
            HOST_NAME "test_host"
            OUTPUT_ROOT "${CCPP_CAP_FILES}")

# Retrieve the list of files from datatable.xml and set to CCPP_CAPS
ccpp_datafile(DATATABLE "${CCPP_CAP_FILES}/datatable.xml"
              REPORT_NAME "--ccpp-files")

list(APPEND LIBRARY_LIST ${CCPP_CAPS_LIST})
add_library(TESTLIB OBJECT  ${LIBRARY_LIST})
ADD_EXECUTABLE(${HOST} ${HOST}.F90 $<TARGET_OBJECTS:TESTLIB>)

INCLUDE_DIRECTORIES(${CCPP_CAP_FILES})

set_target_properties(${HOST} PROPERTIES
                              COMPILE_FLAGS "${CMAKE_Fortran_FLAGS}"
                              LINK_FLAGS "${CMAKE_Fortran_FLAGS}")
