# CMake wrapper for ccpp_capgen.py
# Currently meant to be a CMake API needed for generating caps for regression tests.
#
# CAPGEN_DEBUG              - ON/OFF (Default: OFF) - Enables debug capability through ccpp_capgen.py
# CAPGEN_EXPECT_THROW_ERROR - ON/OFF (Default: OFF) - Scans ccpp_capgen.py log for error string and errors if not found.
# HOST_NAME                 - String name of host
# OUTPUT_ROOT               - String path to put generated caps
# VERBOSITY                 - Number of --verbose flags to pass to capgen
# HOSTFILES                 - CMake list of host metadata filenames
# SCHEMEFILES               - CMake list of scheme metadata files
# SUITES                    - CMake list of suite xml files
function(ccpp_capgen)
  set(optionalArgs CAPGEN_DEBUG CAPGEN_EXPECT_THROW_ERROR)
  set(oneValueArgs HOST_NAME OUTPUT_ROOT VERBOSITY)
  set(multi_value_keywords HOSTFILES SCHEMEFILES SUITES)

  cmake_parse_arguments(arg "${optionalArgs}" "${oneValueArgs}" "${multi_value_keywords}" ${ARGN})

  # Error if script file not found.
  set(CCPP_CAPGEN_CMD_LIST "${CMAKE_SOURCE_DIR}/scripts/ccpp_capgen.py")
  if(NOT EXISTS ${CCPP_CAPGEN_CMD_LIST})
    message(FATAL_ERROR "function(ccpp_capgen): Could not find ccpp_capgen.py.  Looked for ${CCPP_CAPGEN_CMD_LIST}.")
  endif()

  # Interpret parsed arguments
  if(DEFINED arg_CAPGEN_DEBUG)
    list(APPEND CCPP_CAPGEN_CMD_LIST "--debug")
  endif()
  if(DEFINED arg_HOSTFILES)
    list(JOIN arg_HOSTFILES "," HOSTFILES_SEPARATED)
    list(APPEND CCPP_CAPGEN_CMD_LIST "--host-files" "${HOSTFILES_SEPARATED}")
  endif()
  if(DEFINED arg_SCHEMEFILES)
    list(JOIN arg_SCHEMEFILES "," SCHEMEFILES_SEPARATED)
    list(APPEND CCPP_CAPGEN_CMD_LIST "--scheme-files" "${SCHEMEFILES_SEPARATED}")
  endif()
  if(DEFINED arg_SUITES)
    list(JOIN arg_SUITES "," SUITES_SEPARATED)
    list(APPEND CCPP_CAPGEN_CMD_LIST "--suites" "${SUITES_SEPARATED}")
  endif()
  if(DEFINED arg_HOST_NAME)
    list(APPEND CCPP_CAPGEN_CMD_LIST "--host-name" "${arg_HOST_NAME}")
  endif()
  if(DEFINED arg_OUTPUT_ROOT)
    message(STATUS "Creating output directory: ${arg_OUTPUT_ROOT}")
    file(MAKE_DIRECTORY "${arg_OUTPUT_ROOT}")
    list(APPEND CCPP_CAPGEN_CMD_LIST "--output-root" "${arg_OUTPUT_ROOT}")
  endif()
  if(DEFINED arg_VERBOSITY)
    string(REPEAT "--verbose" ${arg_VERBOSITY} VERBOSE_PARAMS_SEPERATED)
    separate_arguments(VERBOSE_PARAMS UNIX_COMMAND "${VERBOSE_PARAMS_SEPERATED}")
    list(APPEND CCPP_CAPGEN_CMD_LIST ${VERBOSE_PARAMS})
  endif()

  message(STATUS "Running ccpp_capgen.py from ${CMAKE_CURRENT_SOURCE_DIR}")

  unset(CAPGEN_OUT) # Unset CAPGEN_OUT to prevent incorrect output on subsequent ccpp_capgen(...) calls.
  execute_process(COMMAND ${CCPP_CAPGEN_CMD_LIST}
                  WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}"
                  OUTPUT_VARIABLE CAPGEN_OUT
                  ERROR_VARIABLE CAPGEN_OUT
                  RESULT_VARIABLE RES
                  COMMAND_ECHO STDOUT)

  message(STATUS "ccpp-capgen stdout: ${CAPGEN_OUT}")

  if(arg_CAPGEN_EXPECT_THROW_ERROR)
    # Determine if the process succeeded but had an expected string in the process log.
    string(FIND "${CAPGEN_OUT}" "Variables of type ccpp_constituent_properties_t only allowed in register phase" ERROR_INDEX)

    if (ERROR_INDEX GREATER -1)
      message(STATUS "Capgen build produces expected error message.")
    else()
      message(FATAL_ERROR "CCPP cap generation did not generate expected error. Expected 'Variables of type constituent_properties_t only allowed in register phase.")
    endif()
  else()
    if(RES EQUAL 0)
      message(STATUS "ccpp-capgen completed successfully")
    else()
      message(FATAL_ERROR "CCPP cap generation FAILED: result = ${RES}")
    endif()
  endif()
endfunction()

# CMake wrapper for ccpp_datafile.py
# Currently meant to be a CMake API needed for generating caps for regression tests.
#
# DATATABLE   - Path to generated datatable.xml file
# REPORT_NAME - String report name to get list of generated files form capgen (typically --ccpp-files)
function(ccpp_datafile)
  set(oneValueArgs DATATABLE REPORT_NAME)
  cmake_parse_arguments(arg "" "${oneValueArgs}" "" ${ARGN})

  set(CCPP_DATAFILE_CMD "${CMAKE_SOURCE_DIR}/scripts/ccpp_datafile.py")

  if(NOT EXISTS ${CCPP_DATAFILE_CMD})
    message(FATAL_ERROR "function(ccpp_datafile): Could not find ccpp_datafile.py.  Looked for ${CCPP_DATAFILE_CMD}.")
  endif()

  if(NOT DEFINED arg_REPORT_NAME)
    message(FATAL_ERROR "function(ccpp_datafile): REPORT_NAME not set.  Must specify the report to generate to run cpp_datafile.py")
  endif()
  list(APPEND CCPP_DATAFILE_CMD "${arg_REPORT_NAME}")

  if(NOT DEFINED arg_DATATABLE)
    message(FATAL_ERROR "function(ccpp_datafile): DATATABLE not set.  A datatable file must be configured to call ccpp_datafile.")
  endif()
  list(APPEND CCPP_DATAFILE_CMD "${arg_DATATABLE}")

  message(STATUS "Running ccpp_datafile from ${CMAKE_CURRENT_SOURCE_DIR}")

  unset(CCPP_CAPS) # Unset CCPP_CAPS to prevent incorrect output on subsequent ccpp_datafile(...) calls.
  execute_process(COMMAND ${CCPP_DATAFILE_CMD}
                  WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}"
                  OUTPUT_VARIABLE CCPP_CAPS
                  RESULT_VARIABLE RES
                  OUTPUT_STRIP_TRAILING_WHITESPACE
                  ERROR_STRIP_TRAILING_WHITESPACE
                  COMMAND_ECHO STDOUT)
  message(STATUS "CCPP_CAPS = ${CCPP_CAPS}")
  if(RES EQUAL 0)
    message(STATUS "CCPP cap files retrieved")
  else()
    message(FATAL_ERROR "CCPP cap file retrieval FAILED: result = ${RES}")
  endif()
  string(REPLACE "," ";" CCPP_CAPS_LIST ${CCPP_CAPS}) # Convert "," separated list from python back to ";" separated list for CMake.
  set(CCPP_CAPS_LIST "${CCPP_CAPS_LIST}" PARENT_SCOPE)
endfunction()

