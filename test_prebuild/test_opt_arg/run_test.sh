#!/usr/bin/env bash

set -e

rm -fr build
mkdir build
../../scripts/ccpp_prebuild.py --debug --config=ccpp_prebuild_config.py --builddir=build
cd build
cmake .. 2>&1 | tee log.cmake
make 2>&1 | tee log.make
./test_opt_arg.x
cd ..
rm -fr build
