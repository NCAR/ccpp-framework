function(ccpp_capgen)
  set(optionalArgs CAPGEN_DEBUG CAPGEN_EXPECT_THROW_ERROR)
  set(oneValueArgs HOSTFILES SCHEMEFILES SUITES HOST_NAME OUTPUT_ROOT VERBOSITY)

  cmake_parse_arguments(arg "${optionalArgs}" "${oneValueArgs}" "" ${ARGN})

  list(APPEND CCPP_CAPGEN_CMD "${CMAKE_SOURCE_DIR}/scripts/ccpp_capgen.py")

  if(DEFINED arg_CAPGEN_DEBUG)
    list(APPEND CCPP_CAPGEN_CMD "--debug")
  endif()

  if(DEFINED arg_HOSTFILES)
    list(JOIN arg_HOSTFILES "," HOSTFILES_SEPARATED)
    list(APPEND CCPP_CAPGEN_CMD "--host-files" "${HOSTFILES_SEPARATED}")
  endif()
  if(DEFINED arg_SCHEMEFILES)
    list(JOIN arg_SCHEMEFILES "," SCHEMEFILES_SEPARATED)
    list(APPEND CCPP_CAPGEN_CMD "--scheme-files" "${SCHEMEFILES_SEPARATED}")
  endif()
  if(DEFINED arg_SUITES)
    list(JOIN arg_SUITES "," SUITES_SEPARATED)
    list(APPEND CCPP_CAPGEN_CMD "--suites" "${SUITES_SEPARATED}")
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

  list(JOIN CAPGEN_CMD_PARAMS_LIST " " CCPP_CAPGEN_CMD)
  message(STATUS "Running ccpp_capgen: ${CAPGEN_CMD_PARAMS_LIST}")

  execute_process(COMMAND ${CCPP_CAPGEN_CMD}
                  WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}"
                  OUTPUT_VARIABLE CAPGEN_OUT
                  ERROR_VARIABLE CAPGEN_OUT
                  RESULT_VARIABLE RES)

  message(STATUS "ccpp-capgen stdout:" ${CAPGEN_OUT})

  if(arg_CAPGEN_EXPECT_THROW_ERROR)
    string(FIND "${CAPGEN_OUT}" "Variables of type ccpp_constituent_properties_t only allowed in register phase" ERROR_INDEX)

    if (ERROR_INDEX GREATER -1)
      MESSAGE(STATUS "Capgen build produces expected error message.")
    else()
      MESSAGE(FATAL_ERROR "CCPP cap generation did not generate expected error. Expected 'Variables of type ccpp_cosntituent_properties_t only allowed in register phase.")
    endif()
  else()
    if(RES EQUAL 0)
      message(STATUS "ccpp-capgen completed successfully")
    else()
      message(FATAL_ERROR "CCPP cap generation FAILED: result = ${RES}")
    endif()
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
  string(replace "," ";" CCPP_CAPS_LIST ${CCPP_CAPS})
  set(CCPP_CAPS_LIST "${CCPP_CAPS_LIST}" PARENT_SCOPE)
endfunction()

