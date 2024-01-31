# How to build the blocked data test

1. Set compiler environment as appropriate for your system
2. Run the following commands:
```
cd test_prebuild/test_mpi/

rm -fr build_no_mpi
mkdir build_no_mpi
../../scripts/ccpp_prebuild.py --config=ccpp_prebuild_config.py --builddir=build_no_mpi
cd build_no_mpi
cmake .. 2>&1 | tee log.cmake
make 2>&1 | tee log.make
./test_mpi_test.x
cd ..

rm -fr build_mpi_f90
mkdir build_mpi_f90
../../scripts/ccpp_prebuild.py --config=ccpp_prebuild_config.py --builddir=build_mpi_f90
cd build_mpi_f90
cmake -DMPI_F90=ON .. 2>&1 | tee log.cmake
make 2>&1 | tee log.make
./test_mpi_test.x
cd ..

rm -fr build_mpi_f90
mkdir build_mpi_f90
../../scripts/ccpp_prebuild.py --config=ccpp_prebuild_config.py --builddir=build_mpi_f08
cd build_mpi_f08
cmake -DMPI_F08=ON .. 2>&1 | tee log.cmake
make 2>&1 | tee log.make
./test_mpi_test.x
cd ..

```
