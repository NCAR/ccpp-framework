
#------------------------------------------------------------------------------
#
# Create list of SCHEME_FILES, HOST_FILES, and SUITE_FILES
# Paths should be relative to CMAKE_SOURCE_DIR (this file's directory)
#
#------------------------------------------------------------------------------
set(SCHEME_FILES "effr_calc" "effr_diag" "effr_pre" "effr_post" "rad_lw" "rad_sw")
set(HOST_FILES   "module_rad_ddt" "test_host_data" "test_host_mod")
set(SUITE_FILES  "var_compatibility_suite.xml")
# HOST is the name of the executable we will build.
# We assume there are files ${HOST}.meta and ${HOST}.F90 in CMAKE_SOURCE_DIR
set(HOST "test_host")

# By default, generated caps go in ccpp subdir
set(CCPP_CAP_FILES "${CMAKE_CURRENT_BINARY_DIR}/ccpp")

# Create lists for Fortran and meta data files from file names
list(TRANSFORM SCHEME_FILES APPEND ".F90"  OUTPUT_VARIABLE SCHEME_FORTRAN_FILES)
list(TRANSFORM SCHEME_FILES APPEND ".meta" OUTPUT_VARIABLE SCHEME_META_FILES)
list(TRANSFORM HOST_FILES   APPEND ".F90"  OUTPUT_VARIABLE VAR_COMPATIBILITY_HOST_FORTRAN_FILES)
list(TRANSFORM HOST_FILES   APPEND ".meta" OUTPUT_VARIABLE VAR_COMPATIBILITY_HOST_METADATA_FILES)

list(APPEND VAR_COMPATIBILITY_HOST_METADATA_FILES "${HOST}.meta")

# Run ccpp_capgen
ccpp_capgen(CAPGEN_DEBUG ON
            VERBOSITY ${CCPP_VERBOSITY}
            HOSTFILES ${VAR_COMPATIBILITY_HOST_METADATA_FILES}
            SCHEMEFILES ${SCHEME_META_FILES}
            SUITES ${SUITE_FILES}
            HOST_NAME ${HOST}
            OUTPUT_ROOT "${CCPP_CAP_FILES}")

# Retrieve the list of Fortran files required for test host from datatable.xml and set to CCPP_CAPS_LIST
ccpp_datafile(DATATABLE "${CCPP_CAP_FILES}/datatable.xml"
              REPORT_NAME "--ccpp-files")

# Create test host library
add_library(VAR_COMPATIBILITY_TESTLIB OBJECT ${SCHEME_FORTRAN_FILES}
                                             ${VAR_COMPATIBILITY_HOST_FORTRAN_FILES}
                                             ${CCPP_CAPS_LIST})

# Setup test executable with needed dependencies
add_executable(var_compatibility_host_integration test_var_compatibility_integration.F90 ${HOST}.F90)
target_link_libraries(var_compatibility_host_integration PRIVATE VAR_COMPATIBILITY_TESTLIB test_utils)
target_include_directories(var_compatibility_host_integration PRIVATE "$<TARGET_PROPERTY:test_utils,BINARY_DIR>")

# Add executable to be called with ctest
add_test(NAME ctest_var_compatibility_host_integration COMMAND var_compatibility_host_integration)
