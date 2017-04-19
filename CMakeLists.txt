cmake_minimum_required(VERSION 2.8.11)

if(POLICY CMP0048)
    cmake_policy(SET CMP0048 NEW)
    project(ccpp VERSION 0.0.1)
else(POLICY CMP0048)
    project(ccpp)
    set(PROJECT_VERSION 0.0.1)
    set(PROJECT_VERSION_MAJOR 0)
    set(PROJECT_VERSION_MINOR 0)
    set(PROJECT_VERSION_PATCH 1)
endif(POLICY CMP0048)

if(POLICY CMP0042)
    cmake_policy(SET CMP0042 NEW)
endif(POLICY CMP0042)

#------------------------------------------------------------------------------
# Enable Fortran
enable_language(Fortran)

#------------------------------------------------------------------------------
# Set package definitions
set(PACKAGE "CCPP")
set(AUTHORS  "Timothy Brown" "David Gill")
string(TIMESTAMP YEAR "%Y")

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
# CMake Modules
#
#Set the CMake module path
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake")

# Include modules
include(add_sources)
include(export_dynamic)

#------------------------------------------------------------------------------
# By default we want a shared library
option(BUILD_SHARED_LIBS "Build a shared library" ON)


#------------------------------------------------------------------------------
# Enable code coverage
if(CMAKE_COMPILER_IS_GNUCC AND (CMAKE_BUILD_TYPE STREQUAL "Coverage"))
    include(code_coverage)
    setup_target_for_coverage(coverage
                              "src/atm_drv ${CMAKE_SOURCE_DIR}/tests/suite_DUMMY.xml"
                              coverage)
    list(APPEND LIBS "gcov")
endif()

#------------------------------------------------------------------------------
# Enable testing
enable_testing()

#------------------------------------------------------------------------------
# Add the sub-directories
# Source
add_subdirectory(src)
# Documentation
add_subdirectory(doc)

#------------------------------------------------------------------------------
# Configure and enable packaging
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "Common Community Phyics Package")
set(CPACK_PACKAGE_VENDOR "DTC NCAR")
set(CPACK_PACKAGE_DESCRIPTION_FILE "${CMAKE_CURRENT_SOURCE_DIR}/README.md")
set(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/LICENSE")
set(CPACK_PACKAGE_VERSION_MAJOR ${PROJECT_VERSION_MAJOR})
set(CPACK_PACKAGE_VERSION_MINOR ${PROJECT_VERSION_MINOR})
set(CPACK_PACKAGE_VERSION_PATCH ${PROJECT_VERSION_PATCH})
set(CPACK_PACKAGE_INSTALL_DIRECTORY
    "CMake ${CMake_VERSION_MAJOR}.${CMake_VERSION_MINOR}")
set(CPACK_SOURCE_PACKAGE_FILE_NAME
    "${CMAKE_PROJECT_NAME}-${CPACK_PACKAGE_VERSION_MAJOR}.${CPACK_PACKAGE_VERSION_MINOR}.${CPACK_PACKAGE_VERSION_PATCH}")
set(CPACK_SOURCE_GENERATOR "TBZ2")
set(CPACK_GENERATOR "TBZ2")
#set(CPACK_GENERATOR "DEB")
#set(CPACK_DEBIAN_PACKAGE_MAINTAINER "Timothy Brown")

include(CPack)
