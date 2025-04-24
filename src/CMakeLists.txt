include(GNUInstallDirs)

#------------------------------------------------------------------------------
# Set the sources
set(SOURCES_F90
    ccpp_types.F90
)

set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_INSTALL_INCLUDEDIR})

#------------------------------------------------------------------------------
# Define the executable and what to link
add_library(ccpp_framework STATIC ${SOURCES_F90})
target_link_libraries(ccpp_framework PUBLIC MPI::MPI_Fortran)
set_target_properties(ccpp_framework PROPERTIES
                                      VERSION ${PROJECT_VERSION}
                                      SOVERSION ${PROJECT_VERSION_MAJOR}
                                      LINK_FLAGS "${CMAKE_Fortran_FLAGS}")

#------------------------------------------------------------------------------
# Installation
#
target_include_directories(ccpp_framework PUBLIC
  INTERFACE $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_INSTALL_INCLUDEDIR}>
  $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>
)


# Define where to install the library
install(TARGETS ccpp_framework
        EXPORT ccpp_framework-targets
        ARCHIVE DESTINATION lib
        LIBRARY DESTINATION lib
        RUNTIME DESTINATION bin
)

# Export our configuration
install(EXPORT ccpp_framework-targets
        FILE ccpp_framework-config.cmake
        DESTINATION lib/cmake
)

install(DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/ DESTINATION ${CMAKE_INSTALL_INCLUDEDIR})
