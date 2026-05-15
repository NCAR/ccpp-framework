# CMake wrapper for ccpp_validator.py
#
# SOURCE_FILES   - CMake list of Fortran source files
# METADATA_FILES - CMake list of corresponding metadata files
function(ccpp_validator)
  set(optionalArgs)
  set(oneValueArgs VERBOSITY)
  set(multi_value_keywords SOURCE_FILES METADATA_FILES)
  cmake_parse_arguments(arg "${optionalArgs}" "${oneValueArgs}" "${multi_value_keywords}" ${ARGN})

  # Error if script file not found.
  set(CCPP_VALIDATOR_CMD_LIST "${CMAKE_SOURCE_DIR}/../capgen-ng/ccpp_validator.py")
  if(NOT EXISTS ${CCPP_VALIDATOR_CMD_LIST})
    message(FATAL_ERROR "function(ccpp_validator): Could not find ccpp_validator.py.  Looked for ${CCPP_VALIDATOR_CMD_LIST}.")
  endif()

  # Interpret parsed arguments
  if(NOT DEFINED arg_SOURCE_FILES)
    message(FATAL_ERROR "function(ccpp_capgen): SOURCE_FILES not set.")
  endif()
  list(JOIN arg_SOURCE_FILES "," SOURCE_FILES_SEPARATED)
  list(APPEND CCPP_VALIDATOR_CMD_LIST "--source-files" "${SOURCE_FILES_SEPARATED}")

  if(NOT DEFINED arg_METADATA_FILES)
    message(FATAL_ERROR "function(ccpp_capgen): METADATA_FILES not set.")
  endif()
  list(JOIN arg_METADATA_FILES "," METADATA_FILES_SEPARATED)
  list(APPEND CCPP_VALIDATOR_CMD_LIST "--scheme-files" "${METADATA_FILES_SEPARATED}")

  if(DEFINED arg_VERBOSITY)
    string(REPEAT "--verbose " ${arg_VERBOSITY} VERBOSE_PARAMS_SEPARATED)
    separate_arguments(VERBOSE_PARAMS UNIX_COMMAND "${VERBOSE_PARAMS_SEPARATED}")
    list(APPEND CCPP_VALIDATOR_CMD_LIST ${VERBOSE_PARAMS})
  endif()

  message(STATUS "Running ccpp_validator.py from ${CMAKE_CURRENT_SOURCE_DIR}")

  unset(VALIDATOR_OUT)
  execute_process(COMMAND ${CCPP_VALIDATOR_CMD_LIST}
                  WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}"
                  OUTPUT_VARIABLE VALIDATOR_OUT
                  ERROR_VARIABLE VALIDATOR_OUT
                  RESULT_VARIABLE RES
                  COMMAND_ECHO STDOUT)
  if(RES EQUAL 0)
    message(STATUS "ccpp-validator stdout: ${VALIDATOR_OUT}")
  else()
    message(STATUS "${CCPP_VALIDATOR_CMD_LIST} FAILED: result = ${RES}")
    message(STATUS "ccpp-validator stdout: ${VALIDATOR_OUT}")
    message(FATAL_ERROR "Validation of source files failed, abort.")
  endif()

endfunction()


# CMake wrapper for ccpp_capgen_ng.py
#
# TRACE          - ON/OFF (Default: OFF) - Add --trace flag to capgen call
# HOST_NAME      - String name of host
# OUTPUT_ROOT    - String path to put generated caps
# VERBOSITY      - Number of --verbose flags to pass to capgen
# HOSTFILES      - CMake list of host metadata filenames
# SCHEMEFILES    - CMake list of scheme metadata files
# SUITES         - CMake list of suite xml files
function(ccpp_capgen)
  set(optionalArgs TRACE)
  set(oneValueArgs HOST_NAME OUTPUT_ROOT VERBOSITY KIND_SPECS)
  set(multi_value_keywords HOSTFILES SCHEMEFILES SUITES)

  cmake_parse_arguments(arg "${optionalArgs}" "${oneValueArgs}" "${multi_value_keywords}" ${ARGN})

  # Error if script file not found.
  set(CCPP_CAPGEN_CMD_LIST "${CMAKE_SOURCE_DIR}/../capgen-ng/ccpp_capgen_ng.py")
  if(NOT EXISTS ${CCPP_CAPGEN_CMD_LIST})
    message(FATAL_ERROR "function(ccpp_capgen): Could not find ccpp_capgen_ng.py.  Looked for ${CCPP_CAPGEN_CMD_LIST}.")
  endif()

  # Interpret parsed arguments
  if(NOT DEFINED arg_HOSTFILES)
    message(FATAL_ERROR "function(ccpp_capgen): HOSTFILES not set.")
  endif()
  list(JOIN arg_HOSTFILES "," HOSTFILES_SEPARATED)
  list(APPEND CCPP_CAPGEN_CMD_LIST "--host-files" "${HOSTFILES_SEPARATED}")

  if(NOT DEFINED arg_SCHEMEFILES)
    message(FATAL_ERROR "function(ccpp_capgen): SCHEMEFILES not set.")
  endif()
  list(JOIN arg_SCHEMEFILES "," SCHEMEFILES_SEPARATED)
  list(APPEND CCPP_CAPGEN_CMD_LIST "--scheme-files" "${SCHEMEFILES_SEPARATED}")

  if(NOT DEFINED arg_SUITES)
    message(FATAL_ERROR "function(ccpp_capgen): SUITES not set.")
  endif()
  list(JOIN arg_SUITES "," SUITES_SEPARATED)
  list(APPEND CCPP_CAPGEN_CMD_LIST "--suites" "${SUITES_SEPARATED}")

  if(NOT DEFINED arg_HOST_NAME)
    message(FATAL_ERROR "function(ccpp_capgen): HOSTNAME not set.")
  endif()
  list(APPEND CCPP_CAPGEN_CMD_LIST "--host-name" "${arg_HOST_NAME}")

  if(NOT DEFINED arg_OUTPUT_ROOT)
    message(FATAL_ERROR "function(ccpp_capgen): OUTPUT_ROOT not set.")
  endif()
  #file(MAKE_DIRECTORY "${arg_OUTPUT_ROOT}")
  list(APPEND CCPP_CAPGEN_CMD_LIST "--output-root" "${arg_OUTPUT_ROOT}")

  if(DEFINED arg_VERBOSITY)
    string(REPEAT "--verbose " ${arg_VERBOSITY} VERBOSE_PARAMS_SEPARATED)
    separate_arguments(VERBOSE_PARAMS UNIX_COMMAND "${VERBOSE_PARAMS_SEPARATED}")
    list(APPEND CCPP_CAPGEN_CMD_LIST ${VERBOSE_PARAMS})
  endif()

  if(DEFINED arg_KIND_SPECS)
    string(REPLACE "," ";" KIND_SPEC_LIST "${arg_KIND_SPECS}")
    set(KIND_ARGS "")               # start empty
    foreach(pair IN LISTS KIND_SPEC_LIST)
      # Append each pair prefixed with --kind-type and quoted.
      # The surrounding double‑quotes are added explicitly so the
      # resulting string contains them.
      set(KIND_ARGS "${KIND_ARGS}--kind-type \"${pair}\"")
      string(STRIP "${KIND_ARGS}" KIND_ARGS)
    endforeach()
    list(APPEND CCPP_CAPGEN_CMD_LIST ${KIND_SPEC_PARAMS})
  endif()

  if(arg_TRACE)
    list(APPEND CCPP_CAPGEN_CMD_LIST "--trace")
  endif()

  message(STATUS "Running ccpp_capgen.py from ${CMAKE_CURRENT_SOURCE_DIR}")

  # Unset CAPGEN_OUT to prevent incorrect output on subsequent ccpp_capgen(...) calls
  unset(CAPGEN_OUT)
  execute_process(COMMAND ${CCPP_CAPGEN_CMD_LIST}
                  WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}"
                  OUTPUT_VARIABLE CAPGEN_OUT
                  ERROR_VARIABLE CAPGEN_OUT
                  RESULT_VARIABLE RES
                  COMMAND_ECHO STDOUT)

  message(STATUS "ccpp-capgen stdout: ${CAPGEN_OUT}")

endfunction()


# CMake wrapper for ccpp_datafile.py
#
# DATATABLE   - Path to generated datatable.xml file
# REPORT_NAME - String report name to get list of generated files form capgen (typically --ccpp-files)
function(ccpp_datafile)
  set(mandatoryArgs DATATABLE REPORT_NAME)
  cmake_parse_arguments(arg "" "${mandatoryArgs}" "" ${ARGN})

  set(CCPP_DATAFILE_CMD "${CMAKE_SOURCE_DIR}/../capgen-ng/ccpp_datafile.py")

  if(NOT EXISTS ${CCPP_DATAFILE_CMD})
    message(FATAL_ERROR "function(ccpp_datafile): Could not find ccpp_datafile.py.  Looked for ${CCPP_DATAFILE_CMD}.")
  endif()

  if(NOT DEFINED arg_REPORT_NAME)
    message(FATAL_ERROR "function(ccpp_datafile): REPORT_NAME not set.")
  endif()
  list(APPEND CCPP_DATAFILE_CMD "${arg_REPORT_NAME}")

  if(NOT DEFINED arg_DATATABLE)
    message(FATAL_ERROR "function(ccpp_datafile): DATATABLE not set.")
  endif()
  list(APPEND CCPP_DATAFILE_CMD "${arg_DATATABLE}")

  message(STATUS "Running ccpp_datafile from ${CMAKE_CURRENT_SOURCE_DIR}")

  # Unset CCPP_FILES to prevent incorrect output on subsequent ccpp_datafile(...) calls
  unset(CCPP_FILES)
  execute_process(COMMAND ${CCPP_DATAFILE_CMD}
                  WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}"
                  OUTPUT_VARIABLE CCPP_FILES
                  RESULT_VARIABLE RES
                  OUTPUT_STRIP_TRAILING_WHITESPACE
                  ERROR_STRIP_TRAILING_WHITESPACE
                  COMMAND_ECHO STDOUT)
  #message(STATUS "CCPP_FILES = ${CCPP_FILES}")
  if(RES EQUAL 0)
    message(STATUS "CCPP files retrieved")
  else()
    message(FATAL_ERROR "CCPP file retrieval FAILED: result = ${RES}")
  endif()
  if(CCPP_FILES)
    # Convert "," separated list from python back to ";" separated list for CMake
    string(REPLACE "," ";" CCPP_FILES ${CCPP_FILES})
  endif()
  set(CCPP_FILES "${CCPP_FILES}" PARENT_SCOPE)
endfunction()
