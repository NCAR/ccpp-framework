# How to build the test/capgen_test (on hera)

## Quick start:
```
cd test/capgen_test
mkdir build
cd build
cmake ..
make
./test_host
```

The command to run ccpp_capgen.py is:

`<root>/scripts/ccpp_capgen.py \
   --host-files test_host_data.meta,test_host_mod.meta,test_host.meta \
   --scheme-files temp_scheme_files.txt,ddt_suite_files.txt \
   --suites ddt_suite.xml,temp_suite.xml\
   --output-root <root>/test/capgen_test/build/ccpp\
   --generate-host-cap`

where `<root>` is the path to your ccpp/framework directory.

Modify a *meta* file in `capgen_test` and write a test that passes when fixed.

To run the unit tests:
```
cd <root>/test/unit_tests
python  test_metadata_table.py
```
For more verbose output:
```
python  test_metadata_table.py -v 
```
If you have `coverage` installed, to get test coverage:
```
coverage run test_metadata_table.py
coverage report -m
```
To check source code quality with pylint:
```
cd <root>
env PYTHONPATH=scripts:${PYTHONPATH} pylint --rcfile ./test/.pylintrc ./test/unit_tests/test_metadata_table.py
env PYTHONPATH=scripts:${PYTHONPATH} pylint --rcfile ./test/.pylintrc ./test/unit_tests/test_metadata_scheme_file.py
```
