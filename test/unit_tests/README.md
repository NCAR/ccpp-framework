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

`/scratch1/BMC/gmtb/Julie.Schramm/NEMSfv3gfs/ccpp/framework/scripts/ccpp_capgen.py \
   --host-files test_host_data.meta,test_host_mod.meta,test_host.meta \
   --scheme-files temp_scheme_files.txt,ddt_suite_files.txt \
   --suites ddt_suite.xml,temp_suite.xml\
   --output-root /scratch1/BMC/gmtb/Julie.Schramm/NEMSfv3gfs/ccpp/framework/test/capgen_test/build/ccpp\
   --generate-host-cap`

Modify a *meta* file in `capgen_test` and write a test that passes when fixed.

To run the unit tests:
```
cd /scratch1/BMC/gmtb/Julie.Schramm/ccpp-framework-fork/test/unit_tests
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
