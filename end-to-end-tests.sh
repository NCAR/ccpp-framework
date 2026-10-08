#!/usr/bin/env bash

mkdir -p build
rm -fr build/*
cd build
cmake ../end-to-end-tests
make
ctest
cd ..
