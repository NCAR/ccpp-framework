# How to build the stub

1. Set compiler environment as appropriate for your system
2. Run the following commands:
```
cd stub
rm -fr build
mkdir build
../scripts/ccpp_prebuild.py --config=ccpp_prebuild_config.py --builddir=build
cd build
cmake .. 2>&1 | tee log.cmake
make 2>&1 | tee log.make
```
