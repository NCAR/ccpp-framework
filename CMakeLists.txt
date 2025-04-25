cmake_minimum_required(VERSION 3.18)

project(ccpp_framework
        VERSION 5.0.0
        LANGUAGES Fortran)

include(cmake/ccpp_capgen.cmake)
set(CMAKE_MODULE_PATH "${CMAKE_MODULE_PATH};${PROJECT_SOURCE_DIR}/cmake")


#------------------------------------------------------------------------------
# Set package definitions
set(PACKAGE "ccpp-framework")
set(AUTHORS "Dom Heinzeller" "Grant Firl" "Mike Kavulich" "Dustin Swales" "Courtney Peverley")
string(TIMESTAMP YEAR "%Y")

option(CCPP_FRAMEWORK_BUILD_DOCUMENTATION
       "Create and install the HTML documentation (requires Doxygen)" OFF)
option(OPENMP "Enable OpenMP support for the framework" OFF)
option(CCPP_FRAMEWORK_ENABLE_TESTS "Enable building/running tests" OFF)
option(BUILD_SHARED_LIBS "Build a static library" OFF)
set(CCPP_VERBOSITY "0" CACHE STRING "Verbosity level of output (default: 0)")

if(CCPP_FRAMEWORK_ENABLE_TESTS)
  ADD_COMPILE_OPTIONS(-O0)
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
  elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL "IntelLLVM")
    ADD_COMPILE_OPTIONS(-fpe0)
    ADD_COMPILE_OPTIONS(-warn)
    ADD_COMPILE_OPTIONS(-traceback)
    ADD_COMPILE_OPTIONS(-debug full)
    ADD_COMPILE_OPTIONS(-fpp)
  elseif (${CMAKE_Fortran_COMPILER_ID} STREQUAL "NVIDIA" OR ${CMAKE_Fortran_COMPILER_ID} STREQUAL "NVHPC")
    ADD_COMPILE_OPTIONS(-g)
    ADD_COMPILE_OPTIONS(-Mnoipa)
    ADD_COMPILE_OPTIONS(-traceback)
    ADD_COMPILE_OPTIONS(-Mfree)
    ADD_COMPILE_OPTIONS(-Ktrap=fp)
    ADD_COMPILE_OPTIONS(-Mpreprocess)
  else()
    message (WARNING "This program may not be able to be compiled with compiler :${CMAKE_Fortran_COMPILER_ID}")
  endif()
endif()

# Use rpaths on MacOSX
set(CMAKE_MACOSX_RPATH 1)

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
  set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${OpenMP_C_FLAGS}")
  set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${OpenMP_CXX_FLAGS}")
  set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${OpenMP_Fortran_FLAGS}")
endif()

#------------------------------------------------------------------------------
# Set a default build type if none was specified
if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
    message(STATUS "Setting build type to 'Release' as none was specified.")
    set(CMAKE_BUILD_TYPE Release CACHE STRING "Choose the type of build." FORCE)
endif()

#------------------------------------------------------------------------------
# Pass debug/release flag to Fortran files for preprocessor
if(CMAKE_BUILD_TYPE STREQUAL "Debug")
  add_definitions(-DDEBUG)
endif()

#------------------------------------------------------------------------------
# Add the sub-directories
add_subdirectory(src)

if(CCPP_FRAMEWORK_ENABLE_TESTS)
  find_package(PFUNIT REQUIRED)
  enable_testing()
  add_subdirectory(test)
endif()

if (CCPP_FRAMEWORK_BUILD_DOCUMENTATION)
  find_package(Doxygen REQUIRED)
  if(NOT DOXYGEN_FOUND)
      message(FATAL_ERROR "Doxygen is needed to build the documentation.")
  endif()
  add_subdirectory(doc)
endif()

