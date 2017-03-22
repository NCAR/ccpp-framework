# ccpp
GMTB Common Community Physics Package

[![Build Status](https://travis-ci.org/t-brown/ccpp.svg?branch=master)](https://travis-ci.org/t-brown/ccpp)

## Requirements
1. Compilers [GNU Compiler Collection](https://gcc.gnu.org/)
  * C
  * Fortran (must be 2003 compliant)
2. [Cmake](https://cmake.org)

## Building
It is recommend to do an out of source build.

1. Clone the repository.
  * `git clone https://github.com/t-brown/ccpp`
2. Change into the repository clone
  * `cd ccpp`
3. Specify the compiler to use. For example the GNU compiler.
  * `ml gcc`
  * `export CC=gcc`
  * `export FC=gfortran`
  * `export CXX=g++`
4. Make a build directory and change into it.
  * `mkdir build`
  * `cd build`
5. Create the makefiles.
  * `cmake ..`
5. Build the test program.
  * `make`

## Running
The test program will be in the `ccpp/build/src` directory. While test suite
definitions are in the `ccpp/tests` directory.
  * `src/atm_drv ../tests/suite_DUMMY.xml`

## Validating XML
A suite is defined in XML. There is a test suite definied within
the `tests` directory, there is also the XML Schema Definition in
that directory too. To validate a new test suite, you can use
`xmllint`. For example to validate `suite_RAP.xml`:
```
xmllint --schema suite.xsd --noout suite_RAP.xml
suite_RAP.xml validates
```

## Documentation
The code is documented with [Doxygen](www.doxygen.org/).

1. Make the documentation.
  * `make doc`

## Physics Schemes
All physics schemes are kept in the repository under the `src/schemes`
directory.

To add a new scheme one needs to

1. Add/Create the scheme within `src/schemes`.
  a. If it is a simple single source file, you can add
     it in this directory. Then also add the file to
     list of source files in the `CMakeLists.txt` file
     in the `src/schemes` directory.
  b. If it is a more complicated scheme, you should
     create a sub-directory under the `src/schemes`
     directory. If you are using CMake as the build
     system, you need to add an `add_subdirectory()`
     directive to end of those entires (before the
     `add_sources()` list). If you are using an
     external build system we will need to address
     the usage of `ExternalProject_Add()`.
2. Create a `cap` subroutine. The IPD will call your
   cap routine.
  a. The cap routine must be labelled "schemename_cap".
     For example, the dummy scheme has a cap called
     "dummy_cap". The requirements are that it is
    1. Lowercased
    2. "_cap" is appended.
  b. Map all the inputs for the cap from the `ap_data`
     fields array. The `phy_field_data()` subroutine
     should be used for this.

An example of a scheme that does nothing is `src/schemes/dummy.f90`.


