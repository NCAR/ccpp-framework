#------------------------------------------------------------------------------
# Set the sources
set(SOURCES
    ccpp.F90
    ccpp_dl.h
    ccpp_dl.c
    ccpp_dl.F90
    ccpp_errors.F90
    ccpp_fcall.F90
    ccpp_fields.F90
    ccpp_fields_idx.h
    ccpp_fields_idx.c
    ccpp_strings.F90
    ccpp_suite.F90
    ccpp_types.F90
    ccpp_utils.h
    ccpp_utils.c
    ccpp_xml.F90
    ccpp_xml.h
    ccpp_xml.c
)

#------------------------------------------------------------------------------
# Find libXML2
find_package(LibXml2 REQUIRED)
if (LIBXML2_FOUND)
    include_directories(${LIBXML2_INCLUDE_DIR})
    list(APPEND LIBS ${LIBXML2_LIBRARIES})
endif(LIBXML2_FOUND)

#------------------------------------------------------------------------------
# CMake Modules
# Set the CMake module path
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/../cmake")

#------------------------------------------------------------------------------
# Find OpenMP for C/C++
find_package(OpenMP)
if(OPENMP_FOUND)
  set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${OpenMP_C_FLAGS}")
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${OpenMP_CXX_FLAGS}")
  message(STATUS "Enabled OpenMP support for C/C++ compiler")
else(OPENMP_FOUND)
  message (STATUS "C/C++ compiler does not support OpenMP")
endif()

#------------------------------------------------------------------------------
# Find OpenMP for Fortran
FIND_PACKAGE(OpenMP_Fortran)
if(OpenMP_FLAG_DETECTED)
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${OpenMP_Fortran_FLAGS}")
  message(STATUS "Enabled OpenMP support for Fortran compiler")
else(OpenMP_FLAG_DETECTED)
  message (STATUS "Fortran compiler does not support OpenMP")
endif()

#------------------------------------------------------------------------------
# The Fortran compiler/linker flag inserted by cmake to create shared libraries
# with the Intel compiler is deprecated (-i_dynamic), correct here.
# CMAKE_Fortran_COMPILER_ID = {"Intel", "PGI", "GNU", "Clang", "MSVC", ...}
if ("${CMAKE_Fortran_COMPILER_ID}" STREQUAL "Intel")
    string(REPLACE "-i_dynamic" "-shared-intel"
           CMAKE_SHARED_LIBRARY_CREATE_Fortran_FLAGS
           "${CMAKE_SHARED_LIBRARY_CREATE_Fortran_FLAGS}")
    string(REPLACE "-i_dynamic" "-shared-intel"
           CMAKE_SHARED_LIBRARY_LINK_Fortran_FLAGS
           "${CMAKE_SHARED_LIBRARY_LINK_Fortran_FLAGS}")
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
# Add the tests
add_subdirectory(tests)

#------------------------------------------------------------------------------
# Define the executable and what to link
add_library(ccpp ${SOURCES})
target_link_libraries(ccpp LINK_PUBLIC ${LIBS} ${CMAKE_DL_LIBS})
set_target_properties(ccpp PROPERTIES VERSION ${PROJECT_VERSION}
                                      SOVERSION ${PROJECT_VERSION_MAJOR}
                                      COMPILE_FLAGS ${OpenMP_Fortran_FLAGS}
                                      LINK_FLAGS ${OpenMP_Fortran_FLAGS})

#------------------------------------------------------------------------------
# Installation
#

# Find all the C headers and Fortran modules
file(GLOB headers
     "${CMAKE_CURRENT_SOURCE_DIR}/ccpp*.h"
     "${CMAKE_CURRENT_BINARY_DIR}/ccpp*.mod"
)

target_include_directories(ccpp PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
    $<INSTALL_INTERFACE:include/${PROJECT_NAME}>
)

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

# Define where to install the modules
install(FILES ${headers} DESTINATION include/${PROJECT_NAME})
