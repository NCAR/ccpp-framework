#------------------------------------------------------------------------------
cmake_minimum_required(VERSION 3.0)

project(ccpp_mpi_test
        VERSION 1.0.0
        LANGUAGES C Fortran)

#------------------------------------------------------------------------------
# Request a static build
option(BUILD_SHARED_LIBS "Build a shared library" OFF)

#------------------------------------------------------------------------------
# Set MPI flags for C/C++/Fortran
if (MPI_F90 OR MPI_F08)
  find_package(MPI REQUIRED C Fortran)
  if (MPI_F90)
    add_definitions(-DMPI_F90)
  elseif (MPI_F08)
    add_definitions(-DMPI_F08)
  endif (MPI_F90)
endif()

#------------------------------------------------------------------------------
# Pass debug/release flag to Fortran files for preprocessor
if(CMAKE_BUILD_TYPE STREQUAL "Debug")
  add_definitions(-DDEBUG)
endif()

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
#target_link_libraries(SCHEMES PUBLIC MPI::MPI_Fortran MPI::MPI_C)

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

set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -O0 -fno-unsafe-math-optimizations -frounding-math -fsignaling-nans -ffpe-trap=invalid,zero,overflow -fbounds-check -ggdb -fbacktrace -ffree-line-length-none")

#------------------------------------------------------------------------------
add_library(ccpp_mpi_test STATIC ${SCHEMES} ${CAPS} ${API})
if(MPI_F90 OR MPI_F08)
  target_link_libraries(ccpp_mpi_test PUBLIC MPI::MPI_Fortran MPI::MPI_C)
endif(MPI_F90 OR MPI_F08)

# Generate list of Fortran modules from defined sources
foreach(source_f90 ${CAPS} ${API})
    get_filename_component(tmp_source_f90 ${source_f90} NAME)
    string(REGEX REPLACE ".F90" ".mod" tmp_module_f90 ${tmp_source_f90})
    string(TOLOWER ${tmp_module_f90} module_f90)
    list(APPEND MODULES_F90 ${CMAKE_CURRENT_BINARY_DIR}/${module_f90})
endforeach()

set_target_properties(ccpp_mpi_test PROPERTIES VERSION ${PROJECT_VERSION}
                      SOVERSION ${PROJECT_VERSION_MAJOR})

add_executable(test_mpi_test.x main.F90)
add_dependencies(test_mpi_test.x ccpp_mpi_test)
#target_link_libraries(test_mpi_test.x ccpp_mpi_test MPI::MPI_Fortran MPI::MPI_C)
target_link_libraries(test_mpi_test.x ccpp_mpi_test)
set_target_properties(test_mpi_test.x PROPERTIES LINKER_LANGUAGE Fortran)

# Define where to install the library
install(TARGETS ccpp_mpi_test
        EXPORT ccpp_mpi_test-targets
        ARCHIVE DESTINATION lib
        LIBRARY DESTINATION lib
        RUNTIME DESTINATION lib
)
# Export our configuration
install(EXPORT ccpp_mpi_test-targets
        FILE ccpp_mpi_test-config.cmake
        DESTINATION lib/cmake
)
# Define where to install the C headers and Fortran modules
#install(FILES ${HEADERS_C} DESTINATION include)
install(FILES ${MODULES_F90} DESTINATION include)
