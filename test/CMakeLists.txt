add_library(test_utils STATIC test_utils.F90)

add_subdirectory(advection_test)
add_subdirectory(capgen_test)
add_subdirectory(ddthost_test)
add_subdirectory(var_compatibility_test)
