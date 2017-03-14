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

# The project version number
#set(CCPP_VERSION_MAJOR   1   CACHE STRING "Project major version number.")
#set(CCPP_VERSION_MINOR   0   CACHE STRING "Project minor version number.")
#set(CCPP_VERSION_PATCH   1   CACHE STRING "Project patch version number.")
#mark_as_advanced(CCPP_VERSION_MAJOR CCPP_VERSION_MINOR CCPP_VERSION_PATCH)


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
# Add the sub-directories
# Source
add_subdirectory(src)
# Documentation
add_subdirectory(doc)
