# Set default project to unknown
if(NOT PROJECT)
  message(STATUS "Setting CCPP project to 'unknown' as none was specified.")
  set(PROJECT "Unknown")
endif (NOT PROJECT)

#------------------------------------------------------------------------------
# Set the sources
set(SOURCES_F90
    ccpp_types.F90
    ccpp_api.F90
)

# Generate list of Fortran modules from defined sources
foreach(source_f90 ${SOURCES_F90})
    string(REGEX REPLACE ".F90" ".mod" module_f90 ${source_f90})
    list(APPEND MODULES_F90 ${CMAKE_CURRENT_BINARY_DIR}/${module_f90})
endforeach()

#------------------------------------------------------------------------------
# CMake Modules
# Set the CMake module path
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/../cmake")

#------------------------------------------------------------------------------
# Set a default build type if none was specified
if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
    message(STATUS "Setting build type to 'Release' as none was specified.")
    set(CMAKE_BUILD_TYPE Release CACHE STRING "Choose the type of build." FORCE)

    # Set the possible values of build type for cmake-gui
    set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release"
                 "MinSizeRel" "RelWithDebInfo")
endif()

#------------------------------------------------------------------------------
# Pass debug/release flag to Fortran files for preprocessor
if(CMAKE_BUILD_TYPE STREQUAL "Debug")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -DDEBUG")
else(CMAKE_BUILD_TYPE)
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -DRELEASE")
endif()

#------------------------------------------------------------------------------
# Add the toplevel source directory to our include directoies (for .h)
include_directories(${CMAKE_CURRENT_SOURCE_DIR})

# Add the toplevel binary directory to our include directoies (for .mod)
include_directories(${CMAKE_CURRENT_BINARY_DIR})

# Set a cached variable containing the includes, so schemes can use them
set(${PACKAGE}_INCLUDE_DIRS
    "${CMAKE_CURRENT_SOURCE_DIR}$<SEMICOLON>${CMAKE_CURRENT_BINARY_DIR}"
     CACHE FILEPATH "${PACKAGE} include directories")
set(${PACKAGE}_LIB_DIRS
    "${CMAKE_CURRENT_BINARY_DIR}"
     CACHE FILEPATH "${PACKAGE} library directories")

#------------------------------------------------------------------------------
# Define the executable and what to link
add_library(ccpp STATIC ${SOURCES_F90})
target_link_libraries(ccpp LINK_PUBLIC ${LIBS} ${CMAKE_DL_LIBS})
set_target_properties(ccpp PROPERTIES VERSION ${PROJECT_VERSION}
                                      SOVERSION ${PROJECT_VERSION_MAJOR}
                                      LINK_FLAGS ${CMAKE_Fortran_FLAGS})

#------------------------------------------------------------------------------
# Installation
#
if (PROJECT STREQUAL "CCPP-FV3")
  target_include_directories(ccpp PUBLIC
      $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
      $<INSTALL_INTERFACE:include>
  )
elseif (PROJECT STREQUAL "CCPP-SCM")
  target_include_directories(ccpp PUBLIC
      $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
      $<INSTALL_INTERFACE:include/${PROJECT_NAME}>
  )
endif (PROJECT STREQUAL "CCPP-FV3")

# Define where to install the library
install(TARGETS ccpp
        EXPORT ccpp-targets
        ARCHIVE DESTINATION lib
        LIBRARY DESTINATION lib
        RUNTIME DESTINATION lib
)

# Export our configuration
install(EXPORT ccpp-targets
        FILE ccpp-config.cmake
        DESTINATION lib/cmake
)

# Define where to install the Fortran modules
if (PROJECT STREQUAL "CCPP-FV3")
  install(FILES ${MODULES_F90} DESTINATION include)
else (PROJECT STREQUAL "CCPP-SCM")
  install(FILES ${MODULES_F90} DESTINATION include/${PROJECT_NAME})
endif (PROJECT STREQUAL "CCPP-FV3")
