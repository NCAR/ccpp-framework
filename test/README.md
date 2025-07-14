# Testing

## Unit tests
To run the Python based unit tests, see the associated documentation in the `unit_tests` directory.

## Doc tests
The Python source code has a wide range of doctests that can be used to verify implementation details quickly.  To run the Python based doc tests, run:
```bash
$ export PYTHONPATH=<root_project_source_directory>/scripts:<root_project_source_directory>/scripts/parse_tools
$ pytest -v <root_project_source_directory>/scripts/ --doctest-modules
```

## Regression tests
The run the regression tests with mock host models, build the main project with your test option:

```bash
$ cmake -S<root_project_source_directory> -B<build_directory> <options>...
$ cd <build_directory>
$ make
$ ctest
```

For example, to run all of the regression tests from the root of the project, you can use:
```bash
cmake -B./build -S./ -DCCPP_FRAMEWORK_ENABLE_TESTS=ON
```

Currently (as of July 2025), if everything works as expected, you should see something like:
```
Test project <your_build_directory>
    Start 1: ctest_advection_host_integration
1/4 Test #1: ctest_advection_host_integration ...........   Passed    0.01 sec
    Start 2: ctest_capgen_host_integration
2/4 Test #2: ctest_capgen_host_integration ..............   Passed    0.01 sec
    Start 3: ctest_ddt_host_integration
3/4 Test #3: ctest_ddt_host_integration .................   Passed    0.01 sec
    Start 4: ctest_var_compatibility_host_integration
4/4 Test #4: ctest_var_compatibility_host_integration ...   Passed    0.02 sec

100% tests passed, 0 tests failed out of 4

Total Test time (real) =   0.06 sec
```

There are several `<options>...` to enable tests:

1) `-DCCPP_FRAMEWORK_ENABLE_TESTS=ON` Turns on all regression tests.
2) `-DCCPP_RUN_ADVECTION_TEST=ON` Turns on only the advection test
3) `-DCCPP_RUN_CAPGEN_TEST=ON` Turns on only the capgen test
4) `-DCCPP_RUN_DDT_HOST_TEST=ON` Turns on only the ddt host test
5) `-DCCPP_RUN_VAR_COMPATIBILITY_TEST=ON` Turns on only the variable compatibility test

By default, the tests will build in release mode.  To enable debug mode, you will need to set the build type: `-DCMAKE_BUILD_TYPE=Release` (or if you want release with debug symbols: `-DCMAKE_BUILD_TYPE=RelWithDebInfo`).

To enable more verbose output for `ccpp_capgen.py`, add `-DCCPP_VERBOSITY=<n>` to the `cmake` command line arguments where `n={1,2,3}` (`n=0` or no verbosity by default).

If needed, the generated caps will be in `<build_directory>/test/<regression_test>/ccpp`.

### Python regression test interface

There is a matching Python based API for each regression test.  To run the corresponding python tests, build the framework using the build process from above and then you can run:

```bash
BUILD_DIR=<build_directory> \
PYTHONPATH=<root_project_source_directory>/test/:<root_project_source_directory>/scripts/ \
    pytest                                                                                \
        <root_project_source_directory>/test/capgen_test/capgen_test_reports.py           \
        <root_project_source_directory>/test/advection_test/advection_test_reports.py     \
        <root_project_source_directory>/test/ddthost_test/ddthost_test_reports.py         \
        <root_project_source_directory>/test/var_compatibility_test/var_compatibility_test_reports.py
```

You may run tests individually instead of all tests as your use case needs.

Please see each test directory for more information on that specific test.
