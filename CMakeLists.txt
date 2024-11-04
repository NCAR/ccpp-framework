cmake_minimum_required(VERSION 3.10)

project(ccpp_framework
        VERSION 5.0.0
        LANGUAGES Fortran)

enable_language(C CXX)
include(FetchContent)
include(cmake/ccpp_capgen.cmake)
set(CMAKE_MODULE_PATH "${CMAKE_MODULE_PATH};${PROJECT_SOURCE_DIR}/cmake")


#------------------------------------------------------------------------------
# Set package definitions
set(PACKAGE "ccpp-framework")
set(AUTHORS "Dom Heinzeller" "Grant Firl" "Mike Kavulich" "Dustin Swales" "Courtney Peverley")
string(TIMESTAMP YEAR "%Y")

option(CCPP_FRAMEWORK_BUILD_DOCUMENTATION
       "Create and install the HTML documentation (requires Doxygen)" OFF)
option(CCPP_FRAMEWORK_ENABLE_OPENMP "Enable OpenMP support for the framework" OFF)
option(CCPP_FRAMEWORK_ENABLE_TESTS "Enable building/running tests" OFF)
option(BUILD_SHARED_LIBS "Build a static library" OFF)
set(CCPP_VERBOSITY "0" CACHE STRING "Verbosity level of output (default: 0)")

# Use rpaths on MacOSX
set(CMAKE_MACOSX_RPATH 1)

ADD_COMPILE_OPTIONS(-O0)
message(STATUS "Compiling Fortran with ${CMAKE_Fortran_COMPILER_ID}")
if(${CMAKE_Fortran_COMPILER_ID} MATCHES "GNU")
  ADD_COMPILE_OPTIONS(-fcheck=all)
  ADD_COMPILE_OPTIONS(-fbacktrace)
  ADD_COMPILE_OPTIONS(-ffpe-trap=zero)
  ADD_COMPILE_OPTIONS(-finit-real=nan)
  ADD_COMPILE_OPTIONS(-ggdb)
  ADD_COMPILE_OPTIONS(-ffree-line-length-none)
  ADD_COMPILE_OPTIONS(-cpp)
elseif(${CMAKE_Fortran_COMPILER_ID} MATCHES "IntelLLVM")
  #ADD_COMPILE_OPTIONS(-check all)
  ADD_COMPILE_OPTIONS(-fpe0)
  ADD_COMPILE_OPTIONS(-warn)
  ADD_COMPILE_OPTIONS(-traceback)
  ADD_COMPILE_OPTIONS(-debug full)
  ADD_COMPILE_OPTIONS(-fpp)
else()
  message (WARNING "This program has only been compiled with gfortran and ifx.")
endif()

#------------------------------------------------------------------------------
# Set MPI flags for Fortran with MPI F08 interface
# find_package(MPI REQUIRED Fortran)
if(NOT MPI_Fortran_HAVE_F08_MODULE)
  message(FATAL_ERROR "MPI implementation does not support the Fortran 2008 mpi_f08 interface")
endif()

#------------------------------------------------------------------------------
# Set OpenMP flags for C/C++/Fortran
if (CCPP_FRAMEWORK_ENABLE_OPENMP)
  find_package(OpenMP REQUIRED)
  set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${OpenMP_C_FLAGS}")
  set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${OpenMP_CXX_FLAGS}")
  set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${OpenMP_Fortran_FLAGS}")
endif (CCPP_FRAMEWORK_ENABLE_OPENMP)

#------------------------------------------------------------------------------
# Set a default build type if none was specified
if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
    message(STATUS "Setting build type to 'Release' as none was specified.")
    set(CMAKE_BUILD_TYPE Debug CACHE STRING "Choose the type of build." FORCE)
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
  FetchContent_Declare(
    pFUnit
    GIT_REPOSITORY https://github.com/Goddard-Fortran-Ecosystem/pFUnit.git
    GIT_TAG        26dadb1157819ea1bd9c355c60ed52f42dd36432 # v4.10.0
  )
  FetchContent_MakeAvailable(pFUnit)
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

