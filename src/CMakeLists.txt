#------------------------------------------------------------------------------
# Set the sources
add_sources(
    #atm_drv.f90
    kinds.f90
    errors.f90
    strings.f90
    types.f90
    types.h
    xml.c
    xml.f90
    xml.h
    ipd/ipd.f90
    ipd/ipd_phy.c
    ipd/ipd_phy.h
    phy/phy_field_idx.c
    phy/phy_fields.f90
    phy/phy_infra.f90
)

#------------------------------------------------------------------------------
# Find libXML2
find_package(LibXml2 REQUIRED)
if (LIBXML2_FOUND)
        include_directories(${LIBXML2_INCLUDE_DIR})
        set(LIBS ${LIBS} ${LIBXML2_LIBRARIES})
endif(LIBXML2_FOUND)

#------------------------------------------------------------------------------
# Add the toplevel source directory to our include directoies
include_directories(".")

#------------------------------------------------------------------------------
# Add all the different physics schemes
add_subdirectory(schemes)

#------------------------------------------------------------------------------
# Define the executable and what to link
#add_executable(atm_drv ${SOURCES})
#target_link_libraries(atm_drv LINK_PUBLIC ${LIBS} ${CMAKE_DL_LIBS})
add_library(ccpp SHARED ${SOURCES})
target_link_libraries(ccpp LINK_PUBLIC ${LIBS} ${CMAKE_DL_LIBS})
