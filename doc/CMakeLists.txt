#-----------------------------------------------------------------------------
# Doxygen rules
#
# Add a target to generate API documentation with Doxygen
set(doxyfile_in ${CMAKE_CURRENT_SOURCE_DIR}/Doxyfile.in)
set(doxyfile ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile)

configure_file(${doxyfile_in} ${doxyfile} @ONLY)

add_custom_target(doc
    COMMAND ${DOXYGEN_EXECUTABLE} ${doxyfile}
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    COMMENT "Generating API documentation with Doxygen"
    VERBATIM)

set(gmtb_sty_in ${CMAKE_CURRENT_SOURCE_DIR}/DevelopersGuide/gmtb.sty)
set(gmtb_sty ${CMAKE_CURRENT_BINARY_DIR}/DevelopersGuide/gmtb.sty)

configure_file(${gmtb_sty_in} ${gmtb_sty} @ONLY)
