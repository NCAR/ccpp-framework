#------------------------------------------------------------------------------
cmake_minimum_required(VERSION 2.8.11)

if(POLICY CMP0048)
    cmake_policy(SET CMP0048 NEW)
    project(check VERSION 0.0.1)
else(POLICY CMP0048)
    project(check)
    set(PROJECT_VERSION 0.0.1)
    set(PROJECT_VERSION_MAJOR 0)
    set(PROJECT_VERSION_MINOR 0)
    set(PROJECT_VERSION_PATCH 1)
endif(POLICY CMP0048)

#------------------------------------------------------------------------------
set(PACKAGE "check")
set(AUTHORS  "Timothy Brown")
string(TIMESTAMP YEAR "%Y")

#------------------------------------------------------------------------------
# Enable Fortran
enable_language(Fortran)

#------------------------------------------------------------------------------
# CMake Modules
# Set the CMake module path
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/../../cmake")

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

message (STATUS "CMAKE_C_FLAGS: ${CMAKE_C_FLAGS}")
message (STATUS "CMAKE_CXX_FLAGS: ${CMAKE_CXX_FLAGS}")
message (STATUS "CMAKE_Fortran_FLAGS: ${CMAKE_Fortran_FLAGS}")

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
# Set a default build type if none was specified
if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
    message(STATUS "Setting build type to 'Debug' as none was specified.")
    set(CMAKE_BUILD_TYPE Debug CACHE STRING "Choose the type of build." FORCE)

    # Set the possible values of build type for cmake-gui
    set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release"
                 "MinSizeRel" "RelWithDebInfo")
endif()

#------------------------------------------------------------------------------
# By default we want a shared library
option(BUILD_SHARED_LIBS "Build a shared library" ON)

#------------------------------------------------------------------------------
# Add the CCPP include/module directory
set(CCPP_INCLUDE_DIRS "" CACHE FILEPATH "Path to ccpp includes")
set_property(DIRECTORY PROPERTY INCLUDE_DIRECTORIES ${CCPP_INCLUDE_DIRS})

#------------------------------------------------------------------------------
# Add the CCPP library
set(CCPP_LIB_DIRS "" CACHE FILEPATH "Path to ccpp library")
link_directories(${CCPP_LIB_DIRS})
list(APPEND LIBS "ccpp")

#------------------------------------------------------------------------------
# Set the sources
set(SOURCES
    check_test.f90
    check_noop.f90
)

#------------------------------------------------------------------------------
# Add the auto-generated caps
add_custom_command(
    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/check_test_cap.f90
    DEPENDS ${CCPP_MKCAP}
    COMMAND ${CCPP_MKCAP} -o ${CMAKE_CURRENT_BINARY_DIR}/check_test_cap.f90
                             ${CMAKE_CURRENT_SOURCE_DIR}/scheme.xml
)
list(APPEND SOURCES ${CMAKE_CURRENT_BINARY_DIR}/check_test_cap.f90)

add_library(check ${SOURCES})
target_link_libraries(check LINK_PUBLIC ${LIBS})
set_target_properties(check PROPERTIES VERSION ${PROJECT_VERSION}
                                       SOVERSION ${PROJECT_VERSION_MAJOR}
                                       COMPILE_FLAGS ${OpenMP_Fortran_FLAGS}
                                       LINK_FLAGS ${OpenMP_Fortran_FLAGS})

