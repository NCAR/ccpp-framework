
#------------------------------------------------------------------------------
#
# Create list of SCHEME_FILES, HOST_FILES, and SUITE_FILES
# Paths should be relative to CMAKE_SOURCE_DIR (this file's directory)
#
#------------------------------------------------------------------------------
LIST(APPEND SCHEME_FILES "temp_scheme_files.txt" "ddt_suite_files.txt")
LIST(APPEND HOST_FILES  "test_host_data" "test_host_mod")
LIST(APPEND SUITE_FILES "ddt_suite.xml" "temp_suite.xml")

# HOST is the name of the executable we will build.
# We assume there are files ${HOST}.meta and ${HOST}.F90 in CMAKE_SOURCE_DIR
SET(HOST "test_host")

# By default, generated caps go in ccpp subdir
SET(CCPP_CAP_FILES "${CMAKE_CURRENT_BINARY_DIR}/ccpp" CACHE
  STRING "Location of CCPP-generated cap files")

SET(CCPP_FRAMEWORK ${CMAKE_SOURCE_DIR}/scripts)

# Create metadata and source file lists
foreach(SCHEME_FILE ${SCHEME_FILES})
  file(STRINGS ${SCHEME_FILE} FILENAMES)
  foreach(filename ${FILENAMES})
    string(REPLACE ".meta" ".F90" TEMP "${filename}")
    file(REAL_PATH ${TEMP} ABS_PATH)
    list(APPEND LIBRARY_LIST ${ABS_PATH})
  endforeach()
endforeach(FILE)

foreach(FILE ${HOST_FILES})
  LIST(APPEND HOST_METADATA "${FILE}.meta")
  # target_sources prefers absolute pathnames
  file(REAL_PATH "${FILE}.F90" ABS_PATH)
  list(APPEND LIBRARY_LIST "${ABS_PATH}")
endforeach(FILE)

list(APPEND HOST_METADATA "${HOST}.meta")

message(STATUS "CCPP_VERBOSITY = ${CCPP_VERBOSITY}")
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

message(STATUS "Adding ${CCPP_CAPS_LIST} to library list")
list(APPEND LIBRARY_LIST ${CCPP_CAPS_LIST})
add_library(TESTLIB OBJECT ${LIBRARY_LIST})
add_executable(${HOST} ${HOST}.F90 $<TARGET_OBJECTS:TESTLIB>)

target_include_directories(${HOST} PRIVATE ${CCPP_CAP_FILES})

set_target_properties(${HOST} PROPERTIES
                              COMPILE_FLAGS "${CMAKE_Fortran_FLAGS}"
                              LINK_FLAGS "${CMAKE_Fortran_FLAGS}")

add_test(NAME capgen_${HOST} COMMAND ${HOST})
