
#------------------------------------------------------------------------------
#
# Create list of SCHEME_FILES, HOST_FILES, and SUITE_FILES
# Paths should be relative to CMAKE_SOURCE_DIR (this file's directory)
#
#------------------------------------------------------------------------------
set(SCHEME_FILES       "cld_liq" "cld_ice" "apply_constituent_tendencies" "const_indices")
set(SCHEME_FILES_ERROR "cld_liq" "cld_ice" "dlc_liq")
set(HOST_FILES         "test_host_data" "test_host_mod")
set(SUITE_FILES        "cld_suite.xml")
set(SUITE_FILES_ERROR  "cld_suite_error.xml")
# HOST is the name of the executable we will build.
# We assume there are files ${HOST}.meta and ${HOST}.F90 in CMAKE_SOURCE_DIR
set(HOST "test_host")

# By default, generated caps go in ccpp subdir
set(CCPP_CAP_FILES "${CMAKE_CURRENT_BINARY_DIR}/ccpp")

# Create lists for Fortran and meta data files from file names
list(TRANSFORM SCHEME_FILES       APPEND ".F90"  OUTPUT_VARIABLE SCHEME_FORTRAN_FILES)
list(TRANSFORM SCHEME_FILES       APPEND ".meta" OUTPUT_VARIABLE SCHEME_META_FILES)
list(TRANSFORM SCHEME_FILES_ERROR APPEND ".F90"  OUTPUT_VARIABLE SCHEME_ERROR_FORTRAN_FILES)
list(TRANSFORM SCHEME_FILES_ERROR APPEND ".meta" OUTPUT_VARIABLE SCHEME_ERROR_META_FILES)
list(TRANSFORM HOST_FILES         APPEND ".F90"  OUTPUT_VARIABLE ADVECTION_HOST_FORTRAN_FILES)
list(TRANSFORM HOST_FILES         APPEND ".meta" OUTPUT_VARIABLE ADVECTION_HOST_METADATA_FILES)

list(APPEND ADVECTION_HOST_METADATA_FILES "${HOST}.meta")

# Add Fortran files to library list
foreach(FILE IN LISTS SCHEME_FORTRAN_FILES ADVECTION_HOST_FORTRAN_FILES)
    file(REAL_PATH "${FILE}" ABS_PATH)
    list(APPEND ADVECTION_LIBRARY_LIST "${ABS_PATH}")
endforeach()

# Run ccpp_capgen that we expect to fail
ccpp_capgen(CAPGEN_EXPECT_THROW_ERROR ON
            CAPGEN_DEBUG ON
            VERBOSITY ${CCPP_VERBOSITY}
            HOSTFILES ${ADVECTION_HOST_METADATA_FILES}
            SCHEMEFILES ${SCHEME_ERROR_META_FILES}
            SUITES ${SUITE_FILES_ERROR}
            HOST_NAME ${HOST}
            OUTPUT_ROOT "${CCPP_CAP_FILES}")

# Run ccpp_capgen
ccpp_capgen(CAPGEN_DEBUG ON
            VERBOSITY ${CCPP_VERBOSITY}
            HOSTFILES ${ADVECTION_HOST_METADATA_FILES}
            SCHEMEFILES ${SCHEME_META_FILES}
            SUITES ${SUITE_FILES}
            HOST_NAME ${HOST}
            OUTPUT_ROOT "${CCPP_CAP_FILES}")

# Retrieve the list of files from datatable.xml and set to CCPP_CAPS_LIST
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
