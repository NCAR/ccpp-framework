
#------------------------------------------------------------------------------
#
# Create list of SCHEME_FILES, HOST_FILES, and SUITE_FILES
# Paths should be relative to CMAKE_SOURCE_DIR (this file's directory)
#
#------------------------------------------------------------------------------
set(SCHEME_FILES "var_compatibility_files.txt")
set(HOST_FILES  "test_host_data" "test_host_mod")
set(SUITE_FILES "var_compatibility_suite.xml")
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
foreach(SCHEME_FILE ${SCHEME_FILES})
  file(STRINGS ${SCHEME_FILE} FILENAMES)
  foreach(filename ${FILENAMES})
    string(REPLACE ".meta" ".F90" TEMP "${filename}")
    file(REAL_PATH "${TEMP}" ABS_PATH)
    list(APPEND VAR_COMPATIBILITY_LIBRARY_LIST "${ABS_PATH}")
  endforeach()
endforeach()

foreach(FILE ${HOST_FILES})
  list(APPEND VAR_COMPATIBILITY_HOST_METADATA "${FILE}.meta")
  # target_sources prefers absolute pathnames
  file(REAL_PATH "${FILE}.F90" ABS_PATH)
  list(APPEND VAR_COMPATIBILITY_LIBRARY_LIST "${ABS_PATH}")
endforeach()

list(APPEND VAR_COMPATIBILITY_HOST_METADATA "${HOST}.meta")

# Run ccpp_capgen
ccpp_capgen(CAPGEN_DEBUG ON
            VERBOSITY ${CCPP_VERBOSITY}
            HOSTFILES ${VAR_COMPATIBILITY_HOST_METADATA}
            SCHEMEFILES ${SCHEME_FILES}
            SUITES ${SUITE_FILES}
            HOST_NAME "test_host"
            OUTPUT_ROOT "${CCPP_CAP_FILES}")

# Retrieve the list of files from datatable.xml and set to CCPP_CAPS
ccpp_datafile(DATATABLE "${CCPP_CAP_FILES}/datatable.xml"
              REPORT_NAME "--ccpp-files")

list(APPEND VAR_COMPATIBILITY_LIBRARY_LIST ${CCPP_CAPS_LIST})
add_library(VAR_COMPATIBILITY_TESTLIB OBJECT ${VAR_COMPATIBILITY_LIBRARY_LIST})

add_pfunit_ctest (var_compatibility_host_integration
  TEST_SOURCES test_var_compatibility_integration.pf
  LINK_LIBRARIES VAR_COMPATIBILITY_TESTLIB
  OTHER_SOURCES ${HOST}.F90
)
