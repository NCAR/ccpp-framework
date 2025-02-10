
#------------------------------------------------------------------------------
#
# Create list of SCHEME_FILES, HOST_FILES, and SUITE_FILES
# Paths should be relative to CMAKE_SOURCE_DIR (this file's directory)
#
#------------------------------------------------------------------------------
set(SCHEME_FILES "cld_suite_files.txt")
set(SCHEME_FILES_ERROR "cld_suite_files_error.txt")
set(HOST_FILES  "test_host_data" "test_host_mod")
set(SUITE_FILES "cld_suite.xml")
set(SUITE_FILES_ERROR "cld_suite_error.xml")
# HOST is the name of the executable we will build.
# We assume there are files ${HOST}.meta and ${HOST}.F90 in CMAKE_SOURCE_DIR
set(HOST "test_host")

#------------------------------------------------------------------------------
#
# End of project-specific input
#
#------------------------------------------------------------------------------

# By default, generated caps go in ccpp subdir
set(CCPP_CAP_FILES "${CMAKE_CURRENT_BINARY_DIR}/ccpp")

# Create metadata and source file lists
foreach(FILE ${SCHEME_FILES})
  file(STRINGS "${FILE}" FILENAMES)
  list(TRANSFORM FILENAMES REPLACE ".meta" ".F90")
  foreach(filename ${FILENAMES})
    file(REAL_PATH "${filename}" ABS_PATH)
    list(APPEND ADVECTION_LIBRARY_LIST "${ABS_PATH}")
  endforeach()
endforeach()

foreach(FILE ${HOST_FILES})
  list(APPEND ADVECTION_HOST_METADATA "${FILE}.meta")
  # target_sources prefers absolute pathnames
  file(REAL_PATH "${FILE}.F90" ABS_PATH)
  list(APPEND ADVECTION_LIBRARY_LIST "${ABS_PATH}")
endforeach()

list(APPEND ADVECTION_HOST_METADATA "${HOST}.meta")

# Run ccpp_capgen that we expect to fail
ccpp_capgen(CAPGEN_EXPECT_THROW_ERROR ON
            CAPGEN_DEBUG ON
            VERBOSITY ${CCPP_VERBOSITY}
            HOSTFILES ${ADVECTION_HOST_METADATA}
            SCHEMEFILES ${SCHEME_FILES_ERROR}
            SUITES ${SUITE_FILES_ERROR}
            HOST_NAME "test_host"
            OUTPUT_ROOT "${CCPP_CAP_FILES}")

# Run ccpp_capgen
ccpp_capgen(CAPGEN_DEBUG ON
            VERBOSITY ${CCPP_VERBOSITY}
            HOSTFILES ${ADVECTION_HOST_METADATA}
            SCHEMEFILES ${SCHEME_FILES}
            SUITES ${SUITE_FILES}
            HOST_NAME "test_host"
            OUTPUT_ROOT "${CCPP_CAP_FILES}")

# Retrieve the list of files from datatable.xml and set to CCPP_CAPS
ccpp_datafile(DATATABLE "${CCPP_CAP_FILES}/datatable.xml"
              REPORT_NAME "--ccpp-files")

list(APPEND ADVECTION_LIBRARY_LIST ${CCPP_CAPS_LIST})
add_library(ADVECTION_TESTLIB OBJECT ${ADVECTION_LIBRARY_LIST})

add_pfunit_ctest (advection_host_integration
  TEST_SOURCES test_advection_host.pf
  LINK_LIBRARIES ADVECTION_TESTLIB test_utils
  OTHER_SOURCES ${HOST}.F90
)
target_include_directories(advection_host_integration PRIVATE ${CMAKE_CURRENT_BINARY_DIR}/..)