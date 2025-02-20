
#------------------------------------------------------------------------------
#
# Create list of SCHEME_FILES, HOST_FILES, and SUITE_FILES
# Paths should be relative to CMAKE_SOURCE_DIR (this file's directory)
#
#------------------------------------------------------------------------------
set(SCHEME_FILES "temp_scheme_files.txt" "ddt_suite_files.txt")
set(HOST_FILES  "test_host_data" "test_host_mod")
set(SUITE_FILES "ddt_suite.xml" "temp_suite.xml")

# HOST is the name of the executable we will build.
# We assume there are files ${HOST}.meta and ${HOST}.F90 in CMAKE_SOURCE_DIR
set(HOST "test_host")

# By default, generated caps go in ccpp subdir
set(CCPP_CAP_FILES "${CMAKE_CURRENT_BINARY_DIR}/ccpp")

# Create metadata and source file lists
foreach(SCHEME_FILE ${SCHEME_FILES})
  file(STRINGS ${SCHEME_FILE} FILENAMES)
  foreach(filename ${FILENAMES})
    string(REPLACE ".meta" ".F90" TEMP "${filename}")
    file(REAL_PATH "${TEMP}" ABS_PATH)
    list(APPEND CAPGEN_LIBRARY_LIST "${ABS_PATH}")
  endforeach()
endforeach()

foreach(FILE ${HOST_FILES})
  list(APPEND CAPGEN_HOST_METADATA "${FILE}.meta")
  # target_sources prefers absolute pathnames
  file(REAL_PATH "${FILE}.F90" ABS_PATH)
  list(APPEND CAPGEN_LIBRARY_LIST "${ABS_PATH}")
endforeach()

list(APPEND CAPGEN_HOST_METADATA "${HOST}.meta")

message(STATUS "CCPP_VERBOSITY = ${CCPP_VERBOSITY}")
ccpp_capgen(CAPGEN_DEBUG ON
            VERBOSITY ${CCPP_VERBOSITY}
            HOSTFILES ${CAPGEN_HOST_METADATA}
            SCHEMEFILES ${SCHEME_FILES}
            SUITES ${SUITE_FILES}
            HOST_NAME "test_host"
            OUTPUT_ROOT "${CCPP_CAP_FILES}")

# Retrieve the list of files from datatable.xml and set to CCPP_CAPS
ccpp_datafile(DATATABLE "${CCPP_CAP_FILES}/datatable.xml"
              REPORT_NAME "--ccpp-files")

message(STATUS "Adding ${CCPP_CAPS_LIST} to library list")
list(APPEND CAPGEN_LIBRARY_LIST ${CCPP_CAPS_LIST})
add_library(CAPGEN_TESTLIB OBJECT ${CAPGEN_LIBRARY_LIST})

add_pfunit_ctest (capgen_host_integration
  TEST_SOURCES test_capgen_host.pf
  LINK_LIBRARIES CAPGEN_TESTLIB test_utils
  OTHER_SOURCES ${HOST}.F90
)
target_include_directories(capgen_host_integration PRIVATE ${CMAKE_CURRENT_BINARY_DIR}/..)