function(ccpp_capgen)
  set(optionalArgs CAPGEN_DEBUG)
  set(oneValueArgs HOSTFILES SCHEMEFILES SUITES HOST_NAME OUTPUT_ROOT VERBOSITY)

  cmake_parse_arguments(arg "${optionalArgs}" "${oneValueArgs}" "" ${ARGN})

  list(APPEND CCPP_CAPGEN_CMD "${CMAKE_SOURCE_DIR}/scripts/ccpp_capgen.py")

  if(DEFINED arg_CAPGEN_DEBUG AND arg_CAPGEN_DEBUG)
    list(APPEND CCPP_CAPGEN_CMD "--debug")
  endif()

  if(DEFINED arg_HOSTFILES)
    list(APPEND CCPP_CAPGEN_CMD "--host-files" "${arg_HOSTFILES}")
  endif()
  if(DEFINED arg_SCHEMEFILES)
    list(APPEND CCPP_CAPGEN_CMD "--scheme-files" "${arg_SCHEMEFILES}")
  endif()
  if(DEFINED arg_SUITES)
    list(APPEND CCPP_CAPGEN_CMD "--suites" "${arg_SUITES}")
  endif()
  if(DEFINED arg_HOST_NAME)
    list(APPEND CCPP_CAPGEN_CMD "--host-name" "${arg_HOST_NAME}")
  endif()
  if(DEFINED arg_OUTPUT_ROOT)
    message(STATUS "Creating output directory: ${arg_OUTPUT_ROOT}")
    file(MAKE_DIRECTORY "${arg_OUTPUT_ROOT}")
    list(APPEND CCPP_CAPGEN_CMD "--output-root" "${arg_OUTPUT_ROOT}")
  endif()
  if(DEFINED arg_VERBOSITY)
    string(REPEAT "--verbose" ${arg_VERBOSITY} VERBOSE_PARAMS_SEPERATED)
    separate_arguments(VERBOSE_PARAMS UNIX_COMMAND "${VERBOSE_PARAMS_SEPERATED}")
    list(APPEND CCPP_CAPGEN_CMD ${VERBOSE_PARAMS})
  endif()

  message(STATUS "Running ccpp_capgen from ${CMAKE_CURRENT_SOURCE_DIR}")
  
  string(REPLACE ";" " " CAPGEN_CMD_PARAMS_LIST "${CCPP_CAPGEN_CMD}")
  message(STATUS "Running ccpp_capgen: ${CAPGEN_CMD_PARAMS_LIST}")

  execute_process(COMMAND ${CCPP_CAPGEN_CMD}
                  WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}"
                  OUTPUT_VARIABLE CAPGEN_OUT
                  ERROR_VARIABLE CAPGEN_OUT
                  RESULT_VARIABLE RES)

  message(STATUS "ccpp-capgen stdout:" ${CAPGEN_OUT})

  if(RES EQUAL 0)
    message(STATUS "ccpp-capgen completed successfully")
  else()
    message(FATAL_ERROR "CCPP cap generation FAILED: result = ${RES}")
  endif()
endfunction()



function(ccpp_datafile)
  set(oneValueArgs DATATABLE REPORT_NAME CCPP_CAPS_LIB_FILES)
  cmake_parse_arguments(arg "" "${oneValueArgs}" "" ${ARGN})

  set(CCPP_DATAFILE_CMD "${CMAKE_SOURCE_DIR}/scripts/ccpp_datafile.py")

  if(NOT DEFINED arg_DATATABLE)
    message(FATAL_ERROR "function(ccpp_datafile): DATATABLE not set.  A datatable file must be configured to call ccpp_datafile.")
  endif()
  list(APPEND CCPP_DATAFILE_CMD "${arg_DATATABLE}")

  if(NOT DEFINED arg_REPORT_NAME)
    message(FATAL_ERROR "function(ccpp_datafile): REPORT_NAME not set.  Must specify the report to generate to run cpp_datafile.py")
  endif()
  list(APPEND CCPP_DATAFILE_CMD "${arg_REPORT_NAME}")

  message(STATUS "${CCPP_DATAFILE_CMD}")
  message(STATUS "Running ccpp_datafile from ${CMAKE_CURRENT_SOURCE_DIR}")

  string(REPLACE ";" " " CCPP_DATAFILE_CMD_SEPERATED "${CCPP_DATAFILE_CMD}")
  message(STATUS "Running ccpp_datafile.py command: ${CCPP_DATAFILE_CMD_SEPERATED}")

  execute_process(COMMAND ${CCPP_DATAFILE_CMD}
                  WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}"
                  OUTPUT_VARIABLE CCPP_CAPS
                  RESULT_VARIABLE RES
                  OUTPUT_STRIP_TRAILING_WHITESPACE
                  ERROR_STRIP_TRAILING_WHITESPACE)
  message(STATUS "CCPP_CAPS = ${CCPP_CAPS}")
  if(RES EQUAL 0)
    message(STATUS "CCPP cap files retrieved")
  else()
    message(FATAL_ERROR "CCPP cap file retrieval FAILED: result = ${RES}")
  endif()
  set(CCPP_CAPS "${CCPP_CAPS}" PARENT_SCOPE)
endfunction()

