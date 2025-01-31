
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
list(APPEND SCHEME_FILES "cld_suite_files.txt")
list(APPEND SCHEME_FILES_ERROR "cld_suite_files_error.txt")
list(APPEND HOST_FILES  "test_host_data" "test_host_mod")
list(APPEND SUITE_FILES "cld_suite.xml")
list(APPEND SUITE_FILES_ERROR "cld_suite_error.xml")
# HOST is the name of the executable we will build.
# We assume there are files ${HOST}.meta and ${HOST}.F90 in CMAKE_SOURCE_DIR
set(HOST "test_host")

#------------------------------------------------------------------------------
#
# End of project-specific input
#
#------------------------------------------------------------------------------

# By default, generated caps go in ccpp subdir
set(CCPP_CAP_FILES "${CMAKE_CURRENT_BINARY_DIR}/ccpp" CACHE
  STRING "Location of CCPP-generated cap files")

set(CCPP_FRAMEWORK ${CMAKE_SOURCE_DIR}/scripts)

# Create metadata and source file lists
foreach(FILE ${SCHEME_FILES})
  file(STRINGS ${FILE} FILENAMES)
  list(TRANSFORM FILENAMES REPLACE ".meta" ".F90")
  foreach(filename ${FILENAMES})
    file(REAL_PATH "${filename}" ABS_PATH)
    list(APPEND LIBRARY_LIST ${ABS_PATH})
  endforeach()
endforeach()

foreach(FILE ${HOST_FILES})
  list(APPEND HOST_METADATA "${FILE}.meta")
  # target_sources prefers absolute pathnames
  file(REAL_PATH "${FILE}.F90" ABS_PATH)
  list(APPEND LIBRARY_LIST "${ABS_PATH}")
endforeach()

list(APPEND HOST_METADATA "${HOST}.meta")

# Run ccpp_capgen that we expect to fail
ccpp_capgen(CAPGEN_EXPECT_THROW_ERROR ON
            CAPGEN_DEBUG ON
            VERBOSITY ${CCPP_VERBOSITY}
            HOSTFILES ${HOST_METADATA}
            SCHEMEFILES ${SCHEME_FILES_ERROR}
            SUITES ${SUITE_FILES_ERROR}
            HOST_NAME "test_host"
            OUTPUT_ROOT "${CCPP_CAP_FILES}")

# Run ccpp_capgen
ccpp_capgen(CAPGEN_DEBUG ON
            VERBOSITY ${CCPP_VERBOSITY}
            HOSTFILES ${HOST_METADATA}
            SCHEMEFILES ${SCHEME_FILES}
            SUITES ${SUITE_FILES}
            HOST_NAME "test_host"
            OUTPUT_ROOT "${CCPP_CAP_FILES}")

# Retrieve the list of files from datatable.xml and set to CCPP_CAPS
ccpp_datafile(DATATABLE "${CCPP_CAP_FILES}/datatable.xml"
              REPORT_NAME "--ccpp-files")

list(APPEND LIBRARY_LIST ${CCPP_CAPS_LIST})
add_library(TESTLIB OBJECT  ${LIBRARY_LIST})
add_executable(${HOST} ${HOST}.F90 $<TARGET_OBJECTS:TESTLIB>)

target_include_directories(${HOST} PUBLIC ${CCPP_CAP_FILES})

set_target_properties(${HOST} PROPERTIES
                              COMPILE_FLAGS "${CMAKE_Fortran_FLAGS}"
                              LINK_FLAGS "${CMAKE_Fortran_FLAGS}")

add_test(NAME advection_${HOST} COMMAND ${HOST})