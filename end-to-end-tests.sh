#!/usr/bin/env bash

rm -fr build/*
cd build
cmake ../end-to-end-tests
make
ctest
cd ..
