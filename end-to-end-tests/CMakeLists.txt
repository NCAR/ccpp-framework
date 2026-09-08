cmake_minimum_required(VERSION 3.18)

project(ccpp_framework_end_to_end_tests
        VERSION 8.0.0
        LANGUAGES Fortran)

enable_testing()
include(cmake/ccpp_capgen.cmake)

option(OPENMP "Enable OpenMP support for the framework" ON)
message(STATUS "OpenMP ${OPENMP}")

set(CCPP_VERBOSITY "2" CACHE STRING "Verbosity level of output (default: 0)")

# Set appropriate flags to help with debugging test issues
if(${CMAKE_Fortran_COMPILER_ID} STREQUAL "GNU")
  ADD_COMPILE_OPTIONS(-fcheck=all)
  ADD_COMPILE_OPTIONS(-fbacktrace)
  ADD_COMPILE_OPTIONS(-ffpe-trap=zero)
  ADD_COMPILE_OPTIONS(-finit-real=nan)
  ADD_COMPILE_OPTIONS(-ggdb)
  ADD_COMPILE_OPTIONS(-ffree-line-length-none)
  ADD_COMPILE_OPTIONS(-cpp)
elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL "Intel")
  ADD_COMPILE_OPTIONS(-fpe0)
  ADD_COMPILE_OPTIONS(-warn)
  ADD_COMPILE_OPTIONS(-traceback)
  ADD_COMPILE_OPTIONS(-debug extended)
  ADD_COMPILE_OPTIONS(-fpp)
  ADD_COMPILE_OPTIONS(-diag-disable=10448)
elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL "IntelLLVM")
  ADD_COMPILE_OPTIONS(-fpe0)
  ADD_COMPILE_OPTIONS(-warn)
  ADD_COMPILE_OPTIONS(-traceback)
  ADD_COMPILE_OPTIONS(-debug full)
  ADD_COMPILE_OPTIONS(-fpp)
elseif (${CMAKE_Fortran_COMPILER_ID} STREQUAL "NVIDIA" OR ${CMAKE_Fortran_COMPILER_ID} STREQUAL "NVHPC")
  ADD_COMPILE_OPTIONS(-Mnoipa)
  ADD_COMPILE_OPTIONS(-traceback)
  ADD_COMPILE_OPTIONS(-Mfree)
  ADD_COMPILE_OPTIONS(-Ktrap=fp)
  ADD_COMPILE_OPTIONS(-Mpreprocess)
else()
  message (WARNING "This program may not be able to be compiled with compiler :${CMAKE_Fortran_COMPILER_ID}")
endif()

#------------------------------------------------------------------------------
# Set MPI flags for Fortran with MPI F08 interface
find_package(MPI COMPONENTS Fortran REQUIRED)
if(NOT MPI_Fortran_HAVE_F08_MODULE)
  message(FATAL_ERROR "MPI implementation does not support the Fortran 2008 mpi_f08 interface")
endif()

#------------------------------------------------------------------------------
# Set OpenMP flags for C/C++/Fortran
if(OPENMP)
  find_package(OpenMP REQUIRED)
endif()

#------------------------------------------------------------------------------
# Set a default build type if none was specified
if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
    message(STATUS "Setting build type to 'Debug' as none was specified.")
    set(CMAKE_BUILD_TYPE Debug CACHE STRING "Choose the type of build." FORCE)
endif()

#------------------------------------------------------------------------------
# Run simple tests first
add_subdirectory(chunked_data)
add_subdirectory(opt_arg)

#------------------------------------------------------------------------------
# Run intermediate tests next
add_subdirectory(nested_suite)
add_subdirectory(ddthost)
add_subdirectory(instances)

#------------------------------------------------------------------------------
# Run most complex tests last
add_subdirectory(capgen)
add_subdirectory(var_compat)
add_subdirectory(suite_allocate)
add_subdirectory(constituents_dim)
add_subdirectory(advection)
add_subdirectory(instances_advection)
# auto-clone-constituents: Remove this test when the legacy switch to support
# auto-clone constituents is removed (i.e. after CAM-SIMA updated its physics)
add_subdirectory(advection_auto_clone)
