#!/usr/bin/env bash

set -e

rm -fr build
mkdir build
../../scripts/ccpp_prebuild.py --verbose --debug --config=ccpp_prebuild_config.py --builddir=build
cd build
cmake .. 2>&1 | tee log.cmake
make VERBOSE=1 -j1 2>&1 | tee log.make
./test_unit_conv.x
cd ..
rm -fr build
