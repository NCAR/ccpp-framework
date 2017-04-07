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

set(CMAKE_Fortran_MODULE_DIRECTORY
    "${CMAKE_BINARY_DIR}/${PROJECT_NAME_LC}")

#------------------------------------------------------------------------------
# Find libXML2
find_package(LibXml2 REQUIRED)
if (LIBXML2_FOUND)
    include_directories(${LIBXML2_INCLUDE_DIR})
    list(APPEND LIBS ${LIBXML2_LIBRARIES})
endif(LIBXML2_FOUND)

#------------------------------------------------------------------------------
# Add the toplevel source directory to our include directoies
include_directories(".")

#------------------------------------------------------------------------------
# Add all the different physics schemes
add_subdirectory(schemes)

#------------------------------------------------------------------------------
# Add the tests
add_subdirectory(tests)

#------------------------------------------------------------------------------
# Define the executable and what to link
add_library(ccpp SHARED ${SOURCES})
target_link_libraries(ccpp LINK_PUBLIC ${LIBS} ${CMAKE_DL_LIBS})

#------------------------------------------------------------------------------
# Installation
#
# Define where to install the library
install(TARGETS ccpp
        ARCHIVE DESTINATION lib
        LIBRARY DESTINATION lib)

# Define where to install the modules
install(DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY} DESTINATION include)
