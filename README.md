# GMTB CCPP
[GMTB](http://www.dtcenter.org/GMTB/html/) Common Community Physics Package
(CCPP), including the Interoperable Physics Driver (IPD).

[![Build Status](https://travis-ci.org/t-brown/ccpp.svg?branch=master)](https://travis-ci.org/NCAR/gmtb-ccpp)
[![Coverage Status](https://coveralls.io/repos/github/t-brown/ccpp/badge.svg?branch=master)](https://coveralls.io/github/NCAR/gmtb-ccpp?branch=master)

## Requirements
1. Compilers for example the [GNU Compiler Collection](https://gcc.gnu.org/)
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
5. Build the CCPP library and test programs.
  * `make`

## Running Tests
There are a few test programs within the `ccpp/src/tests` directory.
These should be built when the CCPP library is compiled. To run all
the tests issue the following within the build directory.
  * `make test`

## Validating XML
A suite is defined in XML. There is a test suite definied within
the `tests` directory, there is also the XML Schema Definition in
that directory too. To validate a new test suite, you can use
`xmllint`. For example to validate `suite_RAP.xml`:
```
xmllint --schema suite.xsd --noout suite_RAP.xml
suite_RAP.xml validates
```

Within the `src/tests` directoty there is `test_init_fini.f90` which
will get built when the CCPP library is built. This program only calls
  * `ccpp_init()`
  * `ccpp_fini()`
It is a program to check the suite XML validation within the CCPP
library. The following is an example of using it from within the
`build` directory.
```
src/tests/test_init_fini my_suite.xml
```

## Physics Schemes
All physics schemes are kept in the repository under the `src/schemes`
directory.

To add a new scheme one needs to

1. Add/Create the scheme within `src/schemes`. You should create a
   sub-directory under the `src/schemes` directory. If you are using
   CMake as the build system, you need to add an `add_subdirectory()`
   directive to end of those entires (before the `add_sources()` list).
   If you are using an external build system we will need to address
   the usage of `ExternalProject_Add()`.
2. Create a `cap` subroutine. The IPD will call your
   cap routine.
  a. The cap routine must be labelled "schemename_cap".
     For example, the dummy scheme has a cap called
     "dummy_cap". The requirements are that it is
    1. Lowercased
    2. "_cap" is appended.
  b. Map all the inputs for the cap from the `cdata` encapsulating
     type (this is of the `ccpp_t` type). The cap will extract the
     fields from the fields array with the `ccpp_fields_get()`
     subroutine.

An example of a scheme that does nothing is `src/schemes/check/test.f90`.

## Documentation
The code is documented with [doxygen](www.doxygen.org/).
To generate the documentation you must have [doxygen](www.doxygen.org/)
and [graphviz](http://www.graphviz.org/) installed. The execute:
```
make doc
```

## Code Coverage
The code can be built and run to indicate code coverage. In order to do
this, you must have GNU [gcov](https://gcc.gnu.org/onlinedocs/gcc/Gcov.html)
and [lcov](http://ltp.sourceforge.net/coverage/lcov.php) installed.
To generate the coverage:

1. Make sure you are using the GNU compilers.
2. Configure the build for coverage.
  * `cmake -DCMAKE_BUILD_TYPE=Coverage ..`
3. Build the CCPP.
  * `make`
4. Build the covage report
  * `make coverage`
The coverage report will be in the `coverage` directory within the build.
