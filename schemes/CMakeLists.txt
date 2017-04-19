#------------------------------------------------------------------------------
# Add all the physics schemes in their own sub-directory (a.k.a repos)
#------------------------------------------------------------------------------

include(ExternalProject)

#------------------------------------------------------------------------------
# The checker scheme
ExternalProject_Add(
    check
    SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/check"
    PREFIX "check"
    DOWNLOAD_COMMAND  ""
    UPDATE_COMMAND    ""
    INSTALL_COMMAND   ""
    CMAKE_ARGS -DCCPP_INCLUDE_DIRS=${CCPP_INCLUDE_DIRS}
)
