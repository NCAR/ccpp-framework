#------------------------------------------------------------------------------
#
# Multi-instance + constituents combined test
#
# Exercises per-instance state separation (from end-to-end-tests/instances) and
# host + scheme-registered constituents (boiled-down from end-to-end-tests/
# advection). Verification is mass conservation per instance and cross-instance
# distinctness of final state.
#
#------------------------------------------------------------------------------

set(SCHEME_FILES "cld_liq" "apply_constituent_tendencies")
set(HOST_FILES   "data" "main")
set(SUITE_FILES  "cld_suite.xml")
set(HOST         "test_host")
set(OUTPUT_ROOT  "${CMAKE_CURRENT_BINARY_DIR}/ccpp")

list(TRANSFORM SCHEME_FILES APPEND ".F90"  OUTPUT_VARIABLE SCHEME_FORTRAN_FILES)
list(TRANSFORM SCHEME_FILES APPEND ".meta" OUTPUT_VARIABLE SCHEME_METADATA_FILES)
list(TRANSFORM HOST_FILES   APPEND ".F90"  OUTPUT_VARIABLE HOST_FORTRAN_FILES)
list(TRANSFORM HOST_FILES   APPEND ".meta" OUTPUT_VARIABLE HOST_METADATA_FILES)

# Run ccpp_validator
ccpp_validator(VERBOSITY ${CCPP_VERBOSITY}
               SOURCE_FILES ${SCHEME_FORTRAN_FILES}
               METADATA_FILES ${SCHEME_METADATA_FILES}
               TYPE "SCHEME")
ccpp_validator(VERBOSITY ${CCPP_VERBOSITY}
               SOURCE_FILES ${HOST_FORTRAN_FILES}
               METADATA_FILES ${HOST_METADATA_FILES}
               TYPE "HOST")

ccpp_capgen(VERBOSITY ${CCPP_VERBOSITY}
            HOSTFILES ${HOST_METADATA_FILES}
            SCHEMEFILES ${SCHEME_METADATA_FILES}
            SUITES ${SUITE_FILES}
            HOST_NAME ${HOST}
            OUTPUT_ROOT "${OUTPUT_ROOT}")

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

add_executable(test_instances_advection.x
  ${CAPGEN_DEPENDENCIES}
  ${SCHEME_FORTRAN_FILES_FILTERED}
  ${HOST_FORTRAN_FILES}
  ${CAPGEN_FILES})
target_link_libraries(test_instances_advection.x PRIVATE MPI::MPI_Fortran)
if(OPENMP)
  target_link_libraries(test_instances_advection.x PRIVATE OpenMP::OpenMP_Fortran)
endif()
set_target_properties(test_instances_advection.x PROPERTIES LINKER_LANGUAGE Fortran)

add_test(NAME test_instances_advection
         COMMAND test_instances_advection.x)
