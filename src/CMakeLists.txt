#------------------------------------------------------------------------------
# Set the sources
file(GLOB_RECURSE SOURCES *.F90 *.f90 *.h *.c)

#------------------------------------------------------------------------------
# Define the CCPP library
add_library(ccpp SHARED ${SOURCES})

#------------------------------------------------------------------------------
# Set the version number of the library
set_target_properties(ccpp PROPERTIES
                      SOVERSION ${PROJECT_VERSION_MAJOR}
                      VERSION   ${PROJECT_VERSION})

#------------------------------------------------------------------------------
# Always use full RPATH
# use the full RPATH for the build tree
set(CMAKE_SKIP_BUILD_RPATH FALSE)

# when building, don't use the install RPATH
set(CMAKE_BUILD_WITH_INSTALL_RPATH FALSE)

set(CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_PREFIX}/lib")

# add the automatically determined parts of the RPATH
# which point to directories outside the build tree to the install RPATH
set(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)

# the RPATH to be used when installing, but only if it's not a system directory
list(FIND CMAKE_PLATFORM_IMPLICIT_LINK_DIRECTORIES "${CMAKE_INSTALL_PREFIX}/lib" isSystemDir)
if("${isSystemDir}" STREQUAL "-1")
   SET(CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_PREFIX}/lib")
endif("${isSystemDir}" STREQUAL "-1")
