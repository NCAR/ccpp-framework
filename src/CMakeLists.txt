#------------------------------------------------------------------------------
# Set the sources
add_sources(
    ccpp.f90
    ccpp_errors.f90
    ccpp_fields.f90
    ccpp_fields_idx.h
    ccpp_fields_idx.c
    ccpp_ipd.f90
    ccpp_ipd.h
    ccpp_ipd.c
    ccpp_strings.f90
    ccpp_suite.f90
    ccpp_types.f90
    ccpp_utils.h
    ccpp_utils.c
    ccpp_xml.f90
    ccpp_xml.h
    ccpp_xml.c
)

#------------------------------------------------------------------------------
# Find libXML2
find_package(LibXml2 REQUIRED)
if (LIBXML2_FOUND)
    include_directories(${LIBXML2_INCLUDE_DIR})
    list(APPEND LIBS ${LIBXML2_LIBRARIES})
endif(LIBXML2_FOUND)

#------------------------------------------------------------------------------
# Add the toplevel source directory to our include directoies (for .h)
include_directories("${CMAKE_CURRENT_SOURCE_DIR}")
# Add the toplevel binary directory to our include directoies (for .mod)
include_directories("${CMAKE_CURRENT_BINARY_DIR}")

#------------------------------------------------------------------------------
# Add all the different physics schemes
add_subdirectory(schemes)

#------------------------------------------------------------------------------
# Add the tests
add_subdirectory(tests)

#------------------------------------------------------------------------------
# Define the executable and what to link
add_library(ccpp ${SOURCES})
target_link_libraries(ccpp LINK_PUBLIC ${LIBS} ${CMAKE_DL_LIBS})
set_target_properties(ccpp PROPERTIES VERSION ${PROJECT_VERSION}
                                      SOVERSION ${PROJECT_VERSION_MAJOR})

#------------------------------------------------------------------------------
# Installation
#

# Find all the C headers and Fortran modules
file(GLOB headers
     "${CMAKE_CURRENT_SOURCE_DIR}/ccpp*.h"
     "${CMAKE_CURRENT_BINARY_DIR}/ccpp*.mod"
)

target_include_directories(ccpp PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
    $<INSTALL_INTERFACE:include/${PROJECT_NAME}>
)

# Define where to install the library
install(TARGETS ccpp
        EXPORT ccpp-targets
        ARCHIVE DESTINATION lib
        LIBRARY DESTINATION lib
        RUNTIME DESTINATION lib
)

# Export our configuration
install(EXPORT ccpp-targets
        FILE ccpp-config.cmake
        DESTINATION lib/cmake
)

# Define where to install the modules
install(FILES ${headers} DESTINATION include/${PROJECT_NAME})

