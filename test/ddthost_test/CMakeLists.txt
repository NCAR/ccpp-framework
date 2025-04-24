
#------------------------------------------------------------------------------
#
# Create list of SCHEME_FILES, HOST_FILES, and SUITE_FILES
# Paths should be relative to CMAKE_SOURCE_DIR (this file's directory)
#
#------------------------------------------------------------------------------
set(SCHEME_FILES       "setup_coeffs" "temp_set" "temp_adjust" "temp_calc_adjust")
set(SUITE_SCHEME_FILES "make_ddt" "environ_conditions")
set(HOST_FILES         "test_host_data" "test_host_mod" "host_ccpp_ddt")
set(SUITE_FILES        "ddt_suite.xml" "temp_suite.xml")
# HOST is the name of the executable we will build.
# We assume there are files ${HOST}.meta and ${HOST}.F90 in CMAKE_SOURCE_DIR
set(HOST "test_host")

# By default, generated caps go in ccpp subdir
set(CCPP_CAP_FILES "${CMAKE_CURRENT_BINARY_DIR}/ccpp")

# Create lists for Fortran and meta data files from file names
list(TRANSFORM SCHEME_FILES       APPEND ".F90"  OUTPUT_VARIABLE SCHEME_FORTRAN_FILES)
list(TRANSFORM SCHEME_FILES       APPEND ".meta" OUTPUT_VARIABLE SCHEME_META_FILES)
list(TRANSFORM SUITE_SCHEME_FILES APPEND ".F90"  OUTPUT_VARIABLE SUITE_SCHEME_FORTRAN_FILES)
list(TRANSFORM SUITE_SCHEME_FILES APPEND ".meta" OUTPUT_VARIABLE SUITE_SCHEME_META_FILES)
list(TRANSFORM HOST_FILES         APPEND ".F90"  OUTPUT_VARIABLE DDT_HOST_FORTRAN_FILES)
list(TRANSFORM HOST_FILES         APPEND ".meta" OUTPUT_VARIABLE DDT_HOST_METADATA_FILES)
list(APPEND DDT_HOST_METADATA_FILES "${HOST}.meta")

# Add Fortran files to library list
foreach(FILE IN LISTS SCHEME_FORTRAN_FILES SUITE_SCHEME_FORTRAN_FILES DDT_HOST_FORTRAN_FILES)
    file(REAL_PATH "${FILE}" ABS_PATH)
    list(APPEND DDT_LIBRARY_LIST "${ABS_PATH}")
endforeach()

# Run ccpp_capgen
ccpp_capgen(CAPGEN_DEBUG ON
            VERBOSITY ${CCPP_VERBOSITY}
            HOSTFILES ${DDT_HOST_METADATA_FILES}
            SCHEMEFILES ${SCHEME_META_FILES} ${SUITE_SCHEME_META_FILES}
            SUITES ${SUITE_FILES}
            HOST_NAME ${HOST}
            OUTPUT_ROOT "${CCPP_CAP_FILES}")

# Retrieve the list of files from datatable.xml and set to CCPP_CAPS_LIST
ccpp_datafile(DATATABLE "${CCPP_CAP_FILES}/datatable.xml"
              REPORT_NAME "--ccpp-files")

list(APPEND DDT_LIBRARY_LIST ${CCPP_CAPS_LIST})
add_library(DDT_TESTLIB OBJECT ${DDT_LIBRARY_LIST})

add_pfunit_ctest (ddt_host_integration
  TEST_SOURCES test_ddt_host.pf
  LINK_LIBRARIES DDT_TESTLIB test_utils
  OTHER_SOURCES ${HOST}.F90
)
target_include_directories(ddt_host_integration PRIVATE ${CMAKE_CURRENT_BINARY_DIR}/..)
