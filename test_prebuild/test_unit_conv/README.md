# How to build the unit conv test

1. Set compiler environment as appropriate for your system
2. Run the following commands:
```
cd test_prebuild/test_unit_conv/
rm -fr build
mkdir build
../../scripts/ccpp_prebuild.py --config=ccpp_prebuild_config.py --builddir=build
cd build
cmake .. 2>&1 | tee log.cmake
make 2>&1 | tee log.make
./test_unit_conv.x
# On systems where linking against the MPI library requires a parallel launcher,
# use 'mpirun -np 1 ./test_unit_conv.x' or 'srun -n 1 ./test_unit_conv.x' etc.
```
