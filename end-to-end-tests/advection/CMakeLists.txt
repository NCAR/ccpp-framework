#------------------------------------------------------------------------------
#
# Create list of SCHEME_FILES, HOST_FILES, and SUITE_FILES
# Paths should be relative to CMAKE_SOURCE_DIR (this file's directory)
#
#------------------------------------------------------------------------------

set(SCHEME_FILES "cld_liq" "cld_ice" "apply_constituent_tendencies" "const_indices")
set(HOST_FILES   "test_host_data" "test_host_mod" "test_host")
set(SUITE_FILES  "cld_suite.xml")
set(HOST         "test_host")
# By default, generated caps go in ccpp subdir
set(OUTPUT_ROOT "${CMAKE_CURRENT_BINARY_DIR}/ccpp")

# Create lists for Fortran and meta data files from file names
list(TRANSFORM SCHEME_FILES       APPEND ".F90"  OUTPUT_VARIABLE SCHEME_FORTRAN_FILES)
list(TRANSFORM SCHEME_FILES       APPEND ".meta" OUTPUT_VARIABLE SCHEME_METADATA_FILES)
list(TRANSFORM HOST_FILES         APPEND ".F90"  OUTPUT_VARIABLE HOST_FORTRAN_FILES)
list(TRANSFORM HOST_FILES         APPEND ".meta" OUTPUT_VARIABLE HOST_METADATA_FILES)

# Run ccpp_validator
ccpp_validator(VERBOSITY ${CCPP_VERBOSITY}
               SOURCE_FILES ${SCHEME_FORTRAN_FILES}
               METADATA_FILES ${SCHEME_METADATA_FILES}
               TYPE "SCHEME")
ccpp_validator(VERBOSITY ${CCPP_VERBOSITY}
               SOURCE_FILES ${HOST_FORTRAN_FILES}
               METADATA_FILES ${HOST_METADATA_FILES}
               TYPE "HOST")

# Enable trace output in auto-generated caps
set(CCPP_TRACE ON)

# Run ccpp_capgen
ccpp_capgen(TRACE ${CCPP_TRACE}
            VERBOSITY ${CCPP_VERBOSITY}
            HOSTFILES ${HOST_METADATA_FILES}
            SCHEMEFILES ${SCHEME_METADATA_FILES}
            SUITES ${SUITE_FILES}
            HOST_NAME ${HOST}
            OUTPUT_ROOT "${OUTPUT_ROOT}")

# Retrieve the list of Fortran files required for test host from datatable.xml;
# this includes capgen-generated files and dependencies inferred from metadata
ccpp_datafile(DATATABLE "${OUTPUT_ROOT}/datatable.xml"
              REPORT_NAME "--dependencies")
set(CAPGEN_DEPENDENCIES ${CCPP_FILES})
ccpp_datafile(DATATABLE "${OUTPUT_ROOT}/datatable.xml"
              REPORT_NAME "--scheme-files")
set(SCHEME_FORTRAN_FILES_FILTERED ${CCPP_FILES})
ccpp_datafile(DATATABLE "${OUTPUT_ROOT}/datatable.xml"
              REPORT_NAME "--capgen-files")
set(CAPGEN_FILES ${CCPP_FILES})

message(STATUS "List of capgen dependencies: ${CAPGEN_DEPENDENCIES}")
message(STATUS "List of filtered scheme files: ${SCHEME_FORTRAN_FILES_FILTERED}")
message(STATUS "List of capgen-generated files: ${CAPGEN_FILES}")

# Add extra files needed for testing
set(EXTRA_FILES
  ${CMAKE_CURRENT_SOURCE_DIR}/../utils/test_utils.F90
)

add_executable(test_advection.x
  ${EXTRA_FILES}
  ${CAPGEN_DEPENDENCIES}
  ${SCHEME_FORTRAN_FILES_FILTERED}
  ${HOST_FORTRAN_FILES}
  ${CAPGEN_FILES}
  test_advection_host_integration.F90
)
target_link_libraries(test_advection.x PRIVATE MPI::MPI_Fortran)
if(OPENMP)
  target_link_libraries(test_advection.x PRIVATE OpenMP::OpenMP_Fortran)
endif()
set_target_properties(test_advection.x PROPERTIES LINKER_LANGUAGE Fortran)

# Add executable to be called with ctest
add_test(NAME test_advection
         COMMAND test_advection.x)
