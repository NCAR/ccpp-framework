cmake_minimum_required(VERSION 2.8.11)
cmake_policy(SET CMP0048 NEW)

project(CCPP VERSION 0.0.1)

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
# Set the CMake module path
SET(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${CMAKE_SOURCE_DIR}/cmake)

# Include modules
include(${CMAKE_MODULE_PATH}/add_sources.cmake)
include(${CMAKE_MODULE_PATH}/export_dynamic.cmake)

#------------------------------------------------------------------------------
# Add the sub-directories
# Source
add_subdirectory(src)
# Documentation
add_subdirectory(doc)

#------------------------------------------------------------------------------
# Enable testing
enable_testing()
add_test(DUMMY src/atm_drv ${CMAKE_SOURCE_DIR}/tests/suite_DUMMY.xml)

