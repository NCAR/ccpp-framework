#------------------------------------------------------------------------------
cmake_minimum_required(VERSION 3.0)

project(ccppstub
        VERSION 1.0.0
        LANGUAGES Fortran)

#------------------------------------------------------------------------------
# Request a static build
option(BUILD_SHARED_LIBS "Build a shared library" OFF)

#------------------------------------------------------------------------------
# Set the sources: physics type definitions
set(TYPEDEFS $ENV{CCPP_TYPEDEFS})
if(TYPEDEFS)
  message(STATUS "Got CCPP TYPEDEFS from environment variable: ${TYPEDEFS}")
else(TYPEDEFS)
  include(${CMAKE_CURRENT_BINARY_DIR}/CCPP_TYPEDEFS.cmake)
  message(STATUS "Got CCPP TYPEDEFS from cmakefile include file: ${TYPEDEFS}")
endif(TYPEDEFS)

# Generate list of Fortran modules from the CCPP type
# definitions that need need to be installed
foreach(typedef_module ${TYPEDEFS})
    list(APPEND MODULES_F90 ${CMAKE_CURRENT_BINARY_DIR}/${typedef_module})
endforeach()

#------------------------------------------------------------------------------
# Set the sources: physics schemes
set(SCHEMES $ENV{CCPP_SCHEMES})
if(SCHEMES)
  message(STATUS "Got CCPP SCHEMES from environment variable: ${SCHEMES}")
else(SCHEMES)
  include(${CMAKE_CURRENT_BINARY_DIR}/CCPP_SCHEMES.cmake)
  message(STATUS "Got CCPP SCHEMES from cmakefile include file: ${SCHEMES}")
endif(SCHEMES)

# Set the sources: physics scheme caps
set(CAPS $ENV{CCPP_CAPS})
if(CAPS)
  message(STATUS "Got CCPP CAPS from environment variable: ${CAPS}")
else(CAPS)
  include(${CMAKE_CURRENT_BINARY_DIR}/CCPP_CAPS.cmake)
  message(STATUS "Got CCPP CAPS from cmakefile include file: ${CAPS}")
endif(CAPS)

# Set the sources: physics scheme caps
set(API $ENV{CCPP_API})
if(API)
  message(STATUS "Got CCPP API from environment variable: ${API}")
else(API)
  include(${CMAKE_CURRENT_BINARY_DIR}/CCPP_API.cmake)
  message(STATUS "Got CCPP API from cmakefile include file: ${API}")
endif(API)

#------------------------------------------------------------------------------
add_library(ccppstub STATIC ${SCHEMES} ${CAPS} ${API})
# Generate list of Fortran modules from defined sources
foreach(source_f90 ${CAPS} ${API})
    get_filename_component(tmp_source_f90 ${source_f90} NAME)
    string(REGEX REPLACE ".F90" ".mod" tmp_module_f90 ${tmp_source_f90})
    string(TOLOWER ${tmp_module_f90} module_f90)
    list(APPEND MODULES_F90 ${CMAKE_CURRENT_BINARY_DIR}/${module_f90})
endforeach()

set_target_properties(ccppstub PROPERTIES VERSION ${PROJECT_VERSION}
                      SOVERSION ${PROJECT_VERSION_MAJOR})

# Define where to install the library
install(TARGETS ccppstub
        EXPORT ccppstub-targets
        ARCHIVE DESTINATION lib
        LIBRARY DESTINATION lib
        RUNTIME DESTINATION lib
)
# Export our configuration
install(EXPORT ccppstub-targets
        FILE ccppstub-config.cmake
        DESTINATION lib/cmake
)
# Define where to install the C headers and Fortran modules
#install(FILES ${HEADERS_C} DESTINATION include)
install(FILES ${MODULES_F90} DESTINATION include)

