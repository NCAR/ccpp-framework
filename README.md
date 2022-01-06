# CCPP Framework

This repository contains the Common Community Physics Package (CCPP) Framework: The infrastructure that connects CCPP physics schemes with a host model, as well as stand-alone tools for use with CCPP.

## Documentation
The CCPP Framework is designed to be used with the [CCPP Physics repository](https://github.com/NCAR/ccpp-physics) and a host model. Links to further information about this repository and its place in the context of the CCPP as a whole can be found on the [GitHub Wiki](https://github.com/NCAR/ccpp-framework/wiki).

The CCPP Single Column Model (SCM) is designed as a simple host model for testing and development of the CCPP; information about building and using the CCPP in this context can be found in [the CCPP SCM Users Guide](https://dtcenter.org/sites/default/files/paragraph/scm-ccpp-guide-v5.0.0.pdf).

More information about the design and use of CCPP can be found in the [CCPP Technical Documentation](https://ccpp-techdoc.readthedocs.io/en/v5.0.0/).

## Validating XML
A suite is defined in XML. There are several test suites defined within
the `ccpp/src/tests` directory (which are able to test the build and
installation of the standalone CCPP). There is also the XML Schema
Definition in that directory too. To validate a new test suite, you can
use `xmllint`. For example to validate `suite_EXAMPLE.xml`:
~~~~{.sh}
cd src/tests
xmllint --schema suite.xsd --noout suite_EXAMPLE.xml
suite_EXAMLE.xml validates
~~~~

Within the `src/tests` directory there is a Fortran file
`test_init_finalize.f90` which will get built into an executable program
when the CCPP library is built. This program only calls:
  * `ccpp_init()`
  * `ccpp_finalize()`

It is a program to check the suite XML validation within the CCPP
library. The following is an example of using it from within the
`build` directory.
~~~~{.sh}
src/tests/test_init_finalize my_suite.xml
~~~~

For this to work, the library that is referenced in the xml file
must be added to the LD_LIBRARY_PATH (see above). To test the
correct functionality of CCPP itself, the suite suite_EXAMPLE.xml
in src/tests can be used.

There are two general types of XML files for the CCPP. The first is the
definition file for a suite. These reside in the host model repositories.
Here is a fairly short example:

~~~~{.xml}
<?xml version="1.0" encoding="UTF-8"?>

<suite name="RAP">
  <ipd part="1">
    <subcycle loop="1">
      <scheme>RRTMGLW</scheme>
      <scheme>RRTMGSW</scheme>
      <scheme>MYNNSFC</scheme>
      <scheme>RUCLSM</scheme>
      <scheme>MYNNPBL</scheme>
      <scheme>GF</scheme>
    </subcycle>
  </ipd>
  <ipd part="2">
    <subcycle loop="1">
      <scheme>THOMPSONAERO</scheme>
    </subcycle>
  </ipd>
</suite>
~~~~

*  suite
  * This text string "name" attribute is compared to the user-selected
  physics suite option at run-time.
*  ipd part
  * To allow for the design of the interface between the dynamics and
physical parameterization schemes, this attribute clearly associates particular
packages with the dynamical sections. In this XML example, there are two "part"
sections, with the second part only containing the "THOMPSONAERO" microphysics
scheme.
  * Users should carefully construct the XML file to map the schemes into the
existing sections of the code that calls the physical parameterization schemes.
*  subcycle
  * This functionality is not fully enabled. It is expected to be utilized for
early testing, and is included in the initial release.
*  scheme
  * The scheme elements fully describe the calling sequence of the physical
    parameterization schemes within the model.
  * For each scheme, an XML file (the scheme definition file) needs to exist.
    For the initial release, this XML file has not yet been designed.


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
4. Build the coverage report
  * `make coverage`
The coverage report will be in the `coverage` directory within the build.
