# Set default project to unknown
if(NOT PROJECT)
  message(STATUS "Setting CCPP project to 'unknown' as none was specified.")
  set(PROJECT "Unknown")
endif (NOT PROJECT)

#------------------------------------------------------------------------------
cmake_minimum_required(VERSION 3.0)

project(ccpp
        VERSION 3.0.0
        LANGUAGES C CXX Fortran)

# Use rpaths on MacOSX
set(CMAKE_MACOSX_RPATH 1)
if(POLICY CMP0042)
    cmake_policy(SET CMP0042 NEW)
endif(POLICY CMP0042)

#------------------------------------------------------------------------------
# Set package definitions
set(PACKAGE "ccpp-framework")
set(AUTHORS "Dom Heinzeller" "Grant Firl" "Laurie Carson")
string(TIMESTAMP YEAR "%Y")

#------------------------------------------------------------------------------
# CMake Modules
# Set the CMake module path
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake")

#------------------------------------------------------------------------------
# Set OpenMP flags for C/C++/Fortran
if (OPENMP)
  include(detect_openmp)
  detect_openmp()
  set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${OpenMP_C_FLAGS}")
  set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${OpenMP_CXX_FLAGS}")
  set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${OpenMP_Fortran_FLAGS}")
  message(STATUS "Enable OpenMP support for C/C++/Fortran compiler")
else (OPENMP)
  message (STATUS "Disable OpenMP support for C/C++/Fortran compiler")
endif (OPENMP)

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
    set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release" "Coverage")
endif()

#------------------------------------------------------------------------------
# The PGI compiler can not find any cap routines in their library.
# This is due to how it labels subroutines within a modules.
# For example the subroutine b() in the moduel a(), gets named a_b.
# GCC and Intel do NOT do this, it is name simply as b.
if ("${CMAKE_Fortran_COMPILER_ID}" STREQUAL "PGI")
    message(STATUS "WARNING: PGI compiler is not fully ISO_C compliant; working solution involves a hack pgifix.py")
endif()

#------------------------------------------------------------------------------
# Request a static build
option(BUILD_SHARED_LIBS "Build a static library" OFF)

#------------------------------------------------------------------------------
# Enable code coverage
if ("${CMAKE_Fortran_COMPILER_ID}" STREQUAL "GNU" AND (CMAKE_BUILD_TYPE STREQUAL "Coverage"))
    include(code_coverage)
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
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "Common Community Physics Package - Framework")
set(CPACK_PACKAGE_VENDOR "GMTB NOAA/NCAR")
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

include(CPack)
