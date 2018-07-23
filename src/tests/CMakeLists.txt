
#------------------------------------------------------------------------------
# Add all the tests
add_executable(test_init_finalize test_init_finalize.f90)
target_link_libraries(test_init_finalize ccpp)

add_executable(test_fields test_fields.c)
target_link_libraries(test_fields ccpp)

add_executable(test_check test_check.f90)
target_link_libraries(test_check ccpp)


#------------------------------------------------------------------------------
# Run all the tests

# Make sure we can accept valid xml suites and reject invalid ones
add_test(XML_1 test_init_finalize ${CMAKE_CURRENT_SOURCE_DIR}/suite_noop_1.xml)
add_test(XML_2 test_init_finalize ${CMAKE_CURRENT_SOURCE_DIR}/suite_noop_2.xml)
add_test(XML_3 test_init_finalize ${CMAKE_CURRENT_SOURCE_DIR}/suite_noop_3.xml)
add_test(XML_4 test_init_finalize ${CMAKE_CURRENT_SOURCE_DIR}/suite_noop_4.xml)
add_test(XML_5 test_init_finalize ${CMAKE_CURRENT_SOURCE_DIR}/suite_noop_5.xml)
add_test(XML_6 test_init_finalize ${CMAKE_CURRENT_SOURCE_DIR}/suite_noop_6.xml)

# Make sure we can grow the fields index
add_test(FIELDS test_fields)

# Make sure we can do the cap call
add_test(CHECK_1 test_check ${CMAKE_CURRENT_SOURCE_DIR}/suite_check_1.xml)
add_test(CHECK_2 test_check ${CMAKE_CURRENT_SOURCE_DIR}/suite_check_2.xml)
add_test(CHECK_3 test_check ${CMAKE_CURRENT_SOURCE_DIR}/suite_check_3.xml)

set_tests_properties(XML_3 PROPERTIES WILL_FAIL TRUE)

set_tests_properties(XML_1
                     XML_2
                     XML_3
                     XML_4
                     XML_5
                     XML_6
                     CHECK_1
                     CHECK_2
                     CHECK_3
                     PROPERTIES ENVIRONMENT "LD_LIBRARY_PATH=${CMAKE_CURRENT_BINARY_DIR}/../../schemes/check/src/check-build:$ENV{LD_LIBRARY_PATH}")

#------------------------------------------------------------------------------
# Coverage tests
if(CMAKE_COMPILER_IS_GNUCC AND (CMAKE_BUILD_TYPE STREQUAL "Coverage"))
    setup_target_for_coverage(coverage
                              "test_check
                              ${CMAKE_CURRENT_SOURCE_DIR}/suite_check_1.xml"
                              coverage)
endif()
