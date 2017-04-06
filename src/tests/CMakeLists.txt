#------------------------------------------------------------------------------
# Include the module directory
include_directories("${CMAKE_Fortran_MODULE_DIRECTORY}")

#------------------------------------------------------------------------------
# Add all the tests
add_executable(test_xml test_xml.f90)
target_link_libraries(test_xml ccpp)

add_executable(test_dummy test_dummy.f90)
target_link_libraries(test_dummy ccpp)

add_executable(test_fields test_fields.c)
target_link_libraries(test_fields ccpp)

#------------------------------------------------------------------------------
# Run all the tests

# Make sure we can accept valid xml suites
add_test(XML_1 test_xml ${CMAKE_CURRENT_SOURCE_DIR}/suite_test_1.xml)
add_test(XML_2 test_xml ${CMAKE_CURRENT_SOURCE_DIR}/suite_test_2.xml)
add_test(XML_3 test_xml ${CMAKE_CURRENT_SOURCE_DIR}/suite_test_3.xml)
add_test(XML_4 test_xml ${CMAKE_CURRENT_SOURCE_DIR}/suite_test_4.xml)
add_test(XML_5 test_xml ${CMAKE_CURRENT_SOURCE_DIR}/suite_test_5.xml)
add_test(XML_6 test_xml ${CMAKE_CURRENT_SOURCE_DIR}/suite_test_6.xml)

# Make sure we can do the physics cap call
add_test(DUMMY test_dummy  ${CMAKE_CURRENT_SOURCE_DIR}/suite_DUMMY.xml)

# Make sure we can grow the fields index
add_test(FIELDS test_fields)
