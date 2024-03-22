#!/usr/bin/env bash

rm -fr build
mkdir build
../../scripts/ccpp_prebuild.py --debug --config=ccpp_prebuild_config.py --builddir=build
cd build
cmake .. 2>&1 | tee log.cmake
make 2>&1 | tee log.make
./test_chunked_data.x
cd ..
rm -fr build
