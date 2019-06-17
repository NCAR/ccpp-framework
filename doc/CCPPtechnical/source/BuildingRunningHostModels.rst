.. _BuildingRunningHostModels:
  
****************************************
Building and Running Host Models
****************************************

The following instructions describe how to compile and run the CCPP code with the SCM (:numref:`Section %s <SCM>`) and with the UFS Atmosphere (:numref:`Section %s <UFSAtmo>`). Instructions are for the *Theia, Jet* and *Cheyenne* computational platforms, with examples on how to run the code on *Theia*.

.. _SCM:

SCM
====================

One option for a CCPP host model is the SCM. This can be a valuable tool for diagnosing the performance of a physics suite, from validating that schemes have been integrated into a suite correctly to deep dives into how physical processes are being represented by the approximating code. In fact, this SCM likely serves as the simplest example for using the CCPP and its framework in an atmospheric model.

System Requirements, Libraries, and Tools
--------------------------------------------

The source code for the SCM and CCPP component is in the form of programs written in FORTRAN, FORTRAN 90, and C. In addition, the I/O relies on the netCDF libraries. Beyond the standard scripts, the build system relies on the use of the Python scripting language, along with cmake, GNU make and date.

The basic requirements for building and running the CCPP and SCM bundle are listed below. The versions listed reflect successful tests and there is no guarantee that the code will work with different versions.

    * FORTRAN 90+ compiler versions
        * ifort 18.0.1.163 and 19.0.2
        * gfortran 6.2, 8.1, and 9.1
        * pgf90 17.7 and 17.9
    * C compiler versions
        * icc v18.0.1.163 and 19.0.2
        * gcc 6.2 and 8.1
        * AppleClang 10.0.0.10001145
        * pgcc 17.7 and 17.9
    * cmake versions 2.8.12.1, 2.8.12.2, and 3.6.2
    * netCDF with HDF5, ZLIB and SZIP versions 4.3.0, 4.4.0, 4.4.1.1, 4.5.0, 4.6.1, and 4.6.3 (not 3.x)
    * Python versions 2.7.5, 2.7.9, and 2.7.13 (not 3.x)
    * Libxml2 versions 2.2 and 2.9.7 (not 2.9.9)

Because these tools and libraries are typically the purview of system administrators to install and maintain, they are considered part of the basic system requirements.
Further, there are several utility libraries as part of the NCEPlibs package that must be installed prior to building the SCM.

    * bacio v2.0.1 - Binary I/O library
    * sp v2.0.2 - Spectral Transformation Library
    * w3nco v2.0.6 - GRIB decoder and encoder library

These libraries are prebuilt on most NOAA machines using the Intel compiler. For those needing to build the libraries themselves, GMTB recommends using the source code from GitHub at https://github.com/NCAR/NCEPlibs.git, which includes build files for various compilers and machines using OpenMP flags and which are thread-safe. Instructions for installing NCEPlibs are included on the GitHub repository webpage, but for the sake of example, execute the following for obtaining and building from source in ``/usr/local/NCEPlibs`` on a Mac:

.. code-block:: console

    mkdir /usr/local/NCEPlibs
    cd /usr/local/src
    git clone https://github.com/NCAR/NCEPlibs.git
    cd NCEPlibs
    ./make_ncep_libs.sh -s macosx -c gnu -d /usr/local/NCEPlibs -o 1

Once NCEPlibs is built, the ``NCEPLIBS_DIR`` environment variable must be set to the location of the installation. For example, if NCEPlibs was installed in ``/usr/local/NCEPlibs``, one would execute

.. code-block:: console

    export NCEPLIB_DIR=/usr/local/NCEPlibs

If using *Theia* or *Cheyenne* HPC systems, this environment variable is automatically set to an appropriate installation of NCEPlibs on those machines through use of one of the setup scripts described below.

Building and Running the SCM
--------------------------------------------

Instructions for downloading the code are provided in :numref:`Chapter %s <CodeManagement>`. Here are the steps to compile and run SCM:

* Run the CCPP *prebuild* script to match required physics variables with those available from the dycore (SCM) and to generate physics *caps* and ``makefile`` segments.

    .. code-block:: console

        ./ccpp/framework/scripts/ccpp_prebuild.py --config=./ccpp/config/ccpp_prebuild_config.py [ -- debug ]

* Change directory to the top-level SCM directory.

    .. code-block:: console

        cd scm

* (Optional) Run the machine setup script if necessary. This script loads compiler modules (Fortran 2003-compliant), netCDF module, etc. and sets compiler environment variables.

   * ``source etc/Theia_setup_intel.csh`` (for csh) or ``. etc/Theia_setup_intel.sh`` (for bash)
   * ``source etc/Theia_setup_gnu.csh`` (for csh) or ``. etc/Theia_setup_gnu.sh`` (for bash)
   * ``source etc/Theia_setup_pgi.csh`` (for csh) or ``. etc/Theia_setup_pgi.sh`` (for bash)
   * ``source etc/Cheyenne_setup_intel.csh`` (for csh) or ``. etc/Cheyenne_setup_intel.sh`` (for bash)
   * ``source etc/Cheyenne_setup_gnu.csh`` (for csh) or ``. etc/Cheyenne_setup_gnu.sh`` (for bash)
   * ``source etc/Cheyenne_setup_pgi.csh`` (for csh) or ``. etc/Cheyenne_setup_pgi.sh`` (for bash)
   * ``source etc/UBUNTU_setup.csh`` (for csh) or ``. etc/UBUNTU_setup.sh`` (for bash) if following the instructions in ``doc/README_UBUNTU.txt``
   * ``source etc/CENTOS_setup.csh`` (for csh) or ``. etc/CENTOS_setup.sh`` (for bash) if following the instructions in ``doc/README_CENTOS.txt``
   * ``source etc/MACOSX_setup.csh`` (for csh) or ``. etc/MACOSX_setup.sh`` (for bash) if following the instructions in ``doc/README_MACOSX.txt``

.. note:: If using a local Linux or Mac system, we provide instructions for how to set up your development system (compilers and libraries) in ``doc/README_{MACOSX,UBUNTU,CENTOS}.txt``. If following these, you will need to run the respective setup script listed above. If your computing environment was previously set up to use modern compilers with an associated netCDF installation, it may not be necessary, although we recommend setting environment variables such as ``CC`` and ``FC``. For version 3.0 and above, it is required to have the ``NETCDF`` environment variable set to the path of the netCDF installation that was compiled with the same compiler used in the following steps. Otherwise, the ``cmake`` step will not complete successfully.

* Make a build directory and change into it.

    .. code-block:: console

        mkdir bin && cd bin

* Invoke cmake on the source code to build using one of the commands below.

* Without threading / OpenMP

    .. code-block:: console

        cmake ../src

    * With threading / OpenMP

    .. code-block:: console

        cmake -DOPENMP=ON ../ src

    * Debug mode

    .. code-block:: console

        cmake -DCMAKE_BUILD_TYPE=Debug ../ src

* If ``cmake`` cannot find ``libxml2`` because it is installed in a non-standard location, add the following to the ``cmake`` command.

    .. code-block:: console

        -DPC_LIBXML_INCLUDEDIR=...
        -DPC_LIBXML_LIBDIR=...

* Compile with ``make`` command. Add ``VERBOSE=1`` to obtain more information on the build process.

    .. code-block:: console

        make

Note that this will produce executable ``gmtb_scm`` and library ``libccppphys.so.X.Y.Z`` (where X is a major version number; Y is a minor version number, and Z is a patchlevel) and ``libccppphys.so``, which is a link to ``libccppphys.so.X.Y.Z``. The library, which is located in ``ccpp/lib``, will be dynamically linked to the executable at runtime.

If compilation successfully completes, a working executable named ``gmtb_scm`` will have been created in the ``bin`` directory.

Although ``make clean`` is not currently implemented, an out-of-source build is used, so all that is required to clean the ``build/run`` directory is (from the ``bin`` directory)

.. code-block:: console

    pwd #confirm that you are in the build/run directory before deleting files
    rm -rfd *

.. warning:: This command can be dangerous (deletes files without confirming), so make sure that you’re in the right directory before executing!

There are several test cases provided with this version of the SCM. For all cases, the SCM will go through the time steps, applying forcing and calling the physics defined in the chosen SDF using physics configuration options from an associated namelist. The model is executed through one of two Python run scripts that are pre-staged into the ``bin`` directory: ``run_gmtb_scm.py`` or ``multi_run_gmtb_scm.py``. The former sets up and runs one integration while the latter sets up and runs several integrations serially.

**Single Run Script Usage**

Running a case requires three pieces of information: the case to run (consisting of initial conditions, geolocation, forcing data, etc.), the physics suite to use (through a CCPP SDF), and a physics namelist (that specifies configurable physics options to use). Cases are set up via their own namelists in ``../etc/case_config``. A default physics suite is provided as a user-editable variable in the script and default namelists are associated with each physics suite (through ``../src/default_namelists.py``), so, technically, one must only specify a case to run with the SCM. The single run script’s interface is described below.

.. code-block:: console

    ./run_gmtb_scm.py -c CASE_NAME [-s SUITE_NAME] [-n PHYSICS_NAMELIST_PATH] [-g]


When invoking the run script, the only required argument is the name of the case to run. The case name used must match one of the case configuration files located in ``../etc/case_config`` (*without the .nml extension!*). If specifying a suite other than the default, the suite name used must match the value of the suite name in one of the SDFs located in ``../../ccpp/suites`` (Note: not the filename of the SDF). As part of the third CCPP release, the following suite names are valid:

    * SCM_GFS_v15
    * SCM_GFS_v15plus
    * SCM_csawmg
    * SCM_GSD_v0

Note that using the Thompson microphysics scheme (as in ``SCM_GSD_v0``) requires the existence of lookup tables during its initialization phase. As of the release, computation of the lookup tables has been prohibitively slow with this model, so it is highly suggested that they be downloaded and staged to use this scheme (and the ``SCM_GSD_v0`` suite). Pre-computed tables have been created and are available for download at the following URLs:
    * https://dtcenter.org/GMTB/freezeH2O.dat (243 M)
    * https://dtcenter.org/GMTB/qr_acr_qg.dat (49 M)
    * https://dtcenter.org/GMTB/qr_acr_qs.dat (32 M)

These files should be staged in ``gmtb-scm/scm/data/physics_input_data`` prior to executing the run script. Since binary files can be system-dependent (due to endianness), it is possible that these files will not be read correctly on your system. For reference, the linked files were generated on *Theia* using the Intel v18 compiler.

Also note that some cases require specified surface fluxes. Special SDFs that correspond to the suites listed above have been created and use the ``*_prescribed_surface`` decoration. It is not necessary to specify this filename decoration when specifying the suite name. If the ``spec_sfc_flux`` variable in the configuration file of the case being run is set to ``.true.``, the run script will automatically use the special SDF that corresponds to the chosen suite from the list above.

If specifying a namelist other than the default, the value must be an entire filename that exists in ``../../ccpp/physics_namelists``. Caution should be exercised when modifying physics namelists since some redundancy between flags to control some physics parameterizations and scheme entries in the SDFs currently exists. Values of numerical parameters are typically OK to change without fear of inconsistencies. Lastly, the ``-g`` flag can be used to run the executable through the ``gdb`` debugger (assuming it is installed on the system).

If the run aborts with the error message

.. code-block:: console
   :emphasize-lines: 1,1

   gmtb_scm: libccppphys.so.X.X.X: cannot open shared object file: No such file or directory

the environment variable ``LD_LIBRARY_PATH`` must be set to

.. code-block:: console

    export LD_LIBRARY_PATH=$PWD/ccpp/physics:$LD_LIBRARY_PATH

before running the model.

A netCDF output file is generated in the location specified in the case configuration file, if the ``output_dir`` variable exists in that file. Otherwise an output directory is constructed from the case, suite, and namelist used (if different from the default). All output directories are placed in the ``bin`` directory. Any standard netCDF file viewing or analysis tools may be used to examine the output file (ncdump, ncview, NCL, etc).

**Multiple Run Script Usage**

A second Python script is provided for automating the execution of multiple integrations through repeated calling of the single run script. From the run directory, one may use this script through the following interface.

.. code-block:: console

    ./multi_run_gmtb_scm.py {[-c CASE_NAME] [-s SUITE_NAME] [-f PATH_TO_FILE]} [-v{v}] [-t]

No arguments are required for this script. The ``-c`` or ``--case``, ``-s`` or ``–-suite``, or ``-f`` or ``–-file`` options form a mutually-exclusive group, so exactly one of these is allowed at one time. If ``–c`` is specified with a case name, the script will run a set of integrations for all supported suites (defined in ``../src/supported_suites.py``) for that case. If ``-s`` is specified with a suite name, the script will run a set of integrations for all supported cases (defined in ``../src/supported_cases.py``) for that suite. If ``-f`` is specified with the path to a filename, it will read in lists of cases, suites, and namelists to use from that file. If multiple namelists are specified in the file, there either must be one suite specified or the number of suites must match the number of namelists. If none of the ``-c`` or ``--case``, ``-s`` or ``–-suite``, or ``-f`` or ``–-file`` options group is specified, the script will run through all permutations of supported cases and suites (as defined in the files previously mentioned).

In addition to the main options, some helper options can also be used with any of those above. The ``-vv`` or ``–-verbose`` option can be used to output more information from the script to the console and to a log file. If this option is not used, only completion progress messages are written out. If ``-v`` is used, the script will write out completion progress messages and all messages and output from the single run script. If ``-vv`` is used, the script will also write out all messages and single run script output to a log file (``multi_run_gmtb_scm.log``) in the ``bin`` directory. The final option, ``-t`` or ``–-timer``, can be used to output the elapsed time for each integration executed by the script. Note that the execution time includes file operations performed by the single run script in addition to the execution of the underlying (Fortran) SCM executable. By default, this option will execute one integration of each subprocess. Since some variability is expected for each model run, if greater precision is required, the number of integrations for timing averaging can be set through an internal script variable. This option can be useful, for example, for getting a rough idea of relative computational expense of different physics suites.

**Batch Run Script**

If using the model on HPC resources and significant amounts of processor time is anticipated for the experiments, it will likely be necessary to submit a job through the HPC’s batch system. An example script has been included in the repository for running the model on *Theia*’s batch system (SLURM). It is located in ``gmtb-scm/scm/etc/gmtb_scm_slurm_example.py``. Edit the job_name, account, etc. to suit your needs and copy to the ``bin`` directory. The case name to be run is included in the command variable. To use, invoke

.. code-block:: console

    ./gmtb_scm_slurm_example.py

from the ``bin`` directory.

Additional information on the SCM can be found at https://dtcenter.org/gmtb/users/ccpp/docs/SCM-CCPP-Guide_v3.0.pdf

.. _UFSAtmo:

UFS Atmosphere
====================

Another option for a CCPP host model is the UFS Atmosphere, located in the umbrella repository NEMSfv3gfs.

System Requirements, Libraries, and Compilers
---------------------------------------------
The build system for the UFS with CCPP relies on the use of the Python scripting language, along with ``cmake``.

The basic requirements for building and running the UFS with CCPP are listed below. The versions listed reflect successful tests and there is no guarantee that the code will work with different versions.

    * FORTRAN 90+ compiler versions
        * ifort 15.1.133, 18.0.1.163 and 19.0.2
        * gfortran 6.2, 8.1, and 9.1
    * C compiler versions
        * icc v18.0.1.163 and 19.0.2
        * gcc 6.2.0 and 8.1
        * AppleClang 10.0
    * MPI job scheduler versions
        * mpt 2.19
        * impi 5.1.1.109 and 5.1.2.150
        * mpich 3.2.1
    * cmake versions 2.8.12.1, 2.8.12.2, and 3.6.2
    * netCDF with HDF5, ZLIB and SZIP versions 4.3.0, 4.4.0, 4.4.1.1, 4.5.0, 4.6.1, and 4.6.3 (not 3.x)
    * Python versions 2.7.5, 2.7.9, and 2.7.13 (not 3.x)

A number of NCEP libraries are required to build and run FV3 and are listed in :numref:`Table %s <NCEP_lib_FV3>`.

.. _NCEP_lib_FV3:

.. table:: *NCEP libraries required to build the UFS Atmosphere*

    +---------------------------+-------------+----------------------------------------------------+
    | Library                   | Version     | Description                                        |
    +===========================+=============+====================================================+
    | bacio                     | 2.0.1       | NCEP binary I/O library                            |
    +---------------------------+-------------+----------------------------------------------------+
    | ip                        | 2.0.0/3.0.0 | NCEP general interpolation library                 |
    +---------------------------+-------------+----------------------------------------------------+
    | nemsio                    | 2.2.3       | NEMS I/O routines                                  |
    +---------------------------+-------------+----------------------------------------------------+
    | sp                        | 2.0.2       | NCEP spectral grid transforms                      |
    +---------------------------+-------------+----------------------------------------------------+
    | w3emc                     | 2.2.0       | NCEP/EMC library for decoding data in GRIB1 format |
    +---------------------------+-------------+----------------------------------------------------+
    | w3nco/v2.0.6              | 2.0.6       | NCEP/NCO library for decoding data in GRIB1 format |
    +---------------------------+-------------+----------------------------------------------------+

These libraries are prebuilt on most NOAA machines using the Intel compiler. For those needing to build the libraries themselves, GMTB recommends using the source code from GitHub at https://github.com/NCAR/NCEPlibs.git, which includes build files for various compilers and machines using OpenMP flags and which are thread-safe.

In addition to the NCEP libraries, some additional external libraries are needed (:numref:`Table %s <ext_lib_FV3>`).

.. _ext_lib_FV3:

.. table:: *External libraries necessary to build the UFS Atmosphere*

    +--------------------+-------------------------+---------------------------------------------------------------------------------------------+
    | Library            | Version                 | Description                                                                                 |
    +====================+=========================+=============================================================================================+
    | ESMF               | V7.1.0r and v8.0.0_bs21 | Earth System Modeling Framework for coupling applications                                   |
    +--------------------+-------------------------+---------------------------------------------------------------------------------------------+
    | netCDF             | 4.3.0 and 4.6.1         | Interface to data access functions for storing and retrieving data arrays                   |
    +--------------------+-------------------------+---------------------------------------------------------------------------------------------+
    | SIONlib (optional) | v1.7.2                  | Parallel I/O library (link) that can be used to read precomputed lookup tables instead of \ |
    |                    |                         | computing them on the fly (or using traditional Fortran binary data files)                  |
    +--------------------+-------------------------+---------------------------------------------------------------------------------------------+

The Earth System Modeling Framework (ESMF), the SIONlib, the NCEPlibs, and the netCDF libraries must be built with the same compiler as the other components of the UFS Atmosphere.

Building the UFS Atmosphere
---------------------------

A complete listing and description of the FV3 build options were discussed in :numref:`Chapter %s <ConfigBuildOptions>` and are shown in :numref:`Figure %s <ccpp_build_option>`. This section will describe the commands needed to build the different options using the script ``compile.sh`` provided in the NEMSfv3gfs distribution. This script calls ``ccpp_prebuild.py``, so users do not need to run the *prebuild* step manually. All builds using ``compile.sh`` are made from the ``./tests`` directory of NEMSfv3gfs and follow the basic command:

.. code-block:: console

    ./compile.sh $PWD/../FV3 system.compiler 'MAKEOPTS'

Here, ``system`` stands for the machine on which the code is compiled and can be any of the following machines and compilers: *theia, jet, cheyenne, gaea, stampede, wcoss_cray, wcoss_dell_p3, supermuc_phase2, macosx*, or *linux*.

``compiler`` stands for the compiler to use and depends on the system. For *theia* and *cheyenne*, the available options are ``intel`` and ``gnu``. For *macosx* and *linux*, the only tested compiler is ``gnu``. For all other platforms, ``intel`` is the only option at this time.

The ``MAKEOPTS`` string, enclosed in single or double quotes, allows to specify options for compiling the code. The following options are of interest for building the CCPP version of NEMSfv3gfs:

* **CCPP=Y** - enables :term:`CCPP` (default is ``N``)
* **STATIC=Y** - enables the CCPP static mode; requires ``CCPP=Y`` (default is ``N``) and ``SUITES=...`` (see below)
* **SUITES=XYZ, ABC, DEF, ...** - specify SDF(s) to use when compiling the code in CCPP static mode; SDFs are located in ``ccpp/suites/``, omit the path in the argument; requires ``CCPP=Y STATIC=Y`` (default is ``‘’``)
* **SION=Y** - enables support for the SIONlib I/O library (used by CCPP to read precomputed lookup tables instead of computing them on the fly); available on *Theia, Cheyenne, Jet*; also available on *Mac OS X* and *Linux* if instructions in ``doc/README_{macosx,linux}.txt`` are followed (default is ``N``)
* **32BIT=Y** - compiles FV3 dynamical core in single precision; note that physics are always compiled in double precision; this option is only available on *Theia, Cheyenne*, and *Jet* (default is ``N``)
* **REPRO=Y** - compiles code in :term:`REPRO` mode, i.e. removes certain compiler optimization flags used in the default :term:`PROD` mode to obtain bit-for-bit (b4b) identical results between CCPP and non-CCPP code (default is ``N``)
* **DEBUG=Y** - compiles code in DEBUG mode, i.e. removes all optimization of :term:`PROD` mode and add bound checks; mutually exclusive with ``REPRO=Y`` (default is ``N``)
* **INTEL18=Y** - available on *Theia* and *Jet* only, compiles code with Intel 18 compiler instead of the default Intel 15 compiler (default is ``N``); note that Intel 18 is the only supported compiler on *Cheyenne*.
* **TRANSITION=Y** - applies selective lowering of optimization for selected files to obtain b4b with non-CCPP code in PROD mode (only when using Intel 15 on *Theia*)

Examples:

* Compile non-CCPP code with 32-bit dynamics on *Theia* with the Intel compiler

    .. code-block:: console

        ./compile.sh $PWD/../FV3 theia.intel ‘32BIT=Y’

* Compile dynamic CCPP code in ``DEBUG`` mode on *Jet* with Intel 18

    .. code-block:: console

        ./compile.sh $PWD/../FV3 jet.intel ‘CCPP=Y DEBUG=Y INTEL18=Y’

* Compile static CCPP code for the CPT suite on *Linux* with the GNU compiler, enable support for the SIONlib I/O library (requires that the library to be installed)

    .. code-block:: console

        ./compile.sh $PWD/../FV3 linux.gnu ‘SION=Y CCPP=Y STATIC=Y SUITES=FV3_CPT_v0’

* *Cheyenne* static build with multiple suites:

    .. code-block:: console

        ./compile.sh $PWD/../FV3 cheyenne.intel ‘CCPP=Y STATIC=Y SUITES=FV3_GFS_v15,FV3_CPT_v0’


Running the UFS Atmosphere Using the Regression Tests (RTs)
------------------------------------------------------------

Regression testing is the process of testing changes to the programs to make sure that the existing functionalities still work when changes are introduced. By running the RTs (or a subset of them by copying a RT configuration file and editing it), the code is compiled, the run directories are set up, and the code is executed. The results are typically compared against a pre-existing baseline, but in certain occasions it is necessary to first create a new baseline (for example, in a new platform where a baseline does not exist or when it is expected that a new development will change the answer). Because the RTs set up the run directories, this is a useful and easy way to get started, since all the model configuration files and necessary input data (initial conditions, fixed data) are copied into the right place.

Overview of the RTs
^^^^^^^^^^^^^^^^^^^

The RT configuration files are located in ``./tests`` relative to the top-level directory of NEMSfv3gfs and have names ``rt*.conf``. The default RT configuration file, supplied with the NEMSfv3gfs master, compares the results from the non-CCPP code to the *official baseline* and is called ``rt.conf``. Before running the RT script ``rt.sh`` in the same directory, the user has to set one or more environment variables and potentially modify the script to change the location of the automatically created run directories. The environment variables are ``ACCNR`` (mandatory unless the user is a member of the default project *nems*; sets the account to be charged for running the RTs), ``NEMS_COMPILER`` (optional for the ``intel`` compiler option, set to ``gnu`` to switch), and potentially ``RUNDIR_ROOT``. ``RUNDIR_ROOT`` allows the user to specify an alternative location for the RT run directories underneath which directories called ``rt_$PID`` are created (``$PID`` is the process identifier of the ``rt.sh`` invocation). This may be required on systems where the user does not have write permissions in the default run directory tree.

.. code-block:: console

    export ACCNR=...
    export NEMS_COMPILER=intel
    export RUNDIR_ROOT=/full/path/under/which/rt_$PID/will/be/created

Running the full default RT suite defined in ``rt.conf`` using the script ``rt.sh``:

.. code-block:: console

    ./rt.sh -f

This command can only be used on a NOAA machine using the Intel compiler, where the output of a non-CCPP build using the default Intel version is compared against the *official baseline*. For information on testing the CCPP code, or using alternate computational platforms, see the following sections.

This command and all others below produce log output in ``./tests/log_machine.compiler``. These log files contain information on the location of the run directories that can be used as templates for the user. Each ``rt*.conf`` contains one or more compile commands preceding a number of tests.


Baselines
^^^^^^^^^^^^^^^^^^^

Regression testing is only possible on machines for which baselines exist. EMC maintains *official baselines* of non-CCPP runs on *Jet* and *Wcoss* created with the Intel compiler. GMTB maintains additional baselines on *Theia, Jet, Cheyenne*, and *Gaea*. While GMTB is trying to keep up with changes to the official repositories, baselines maintained by GMTB are not guaranteed to be up-to-date.

When porting the code to a new machine, it is useful to start by establishing a *personal baseline*. Future runs of the RT can then be compared against the *personal baseline* to ascertain that the results have not been inadvertently affected by code developments. The ``rt.sh -c`` option is used to create a *personal baseline*.

.. code-block:: console

    ./rt.sh -l rt.conf -c fv3 # create own reg. test baseline

Once the *personal baseline* has been created, future runs of the RT should be compared against the *personal baseline* using the ``-m`` option.

.. code-block:: console

    ./rt.sh -l rt.conf -m # compare against own baseline

The script rt.sh
^^^^^^^^^^^^^^^^^^^

``rt.sh`` is a bash shell file to run the RT and has the following options:

.. code-block:: console

    Usage: $0 -c <model> | -f | -s | -l <file> | -m | -r | -e | -h
    -c  create new baseline results for <model>
    -f  run full suite of regression tests
    -s  run standard suite of regression tests
    -l  run test specified in <file>
    -m  compare against new baseline results
    -r  use Rocoto workflow manager
    -e  use ecFlow workflow manager
    -h  display this help

The location of the run directories and *personal baseline* directories is controlled in ``rt.sh`` on a per-machine basis. The user is strongly advised to NOT modify the path to the *official baseline* directories.

The *official baseline* directory is defined as:

.. code-block:: console

    RTPWD=$DISKNM/trunk-yyyymmdd/${COMPILER} # on Cheyenne
    RTPWD=$DISKNM/trunk-yyyymmdd             # elsewhere

Note that ``yyyymmdd`` is the year, month and day the RT was created.

.. warning::  Modifying ``$DISKNM`` will break the RTs!

*Personal baseline* results (see below) are stored in

.. code-block:: console

    NEW_BASELINE=${STMP}/${USER}/FV3_RT/REGRESSION_TEST

and RTs are run in ``$RUNDIR_ROOT``.

Example: *Theia*

.. code-block:: console

    ...
    dprefix=/scratch4/NCEPDEV
    DISKNM=$dprefix/nems/noscrub/emc.nemspara/RT
    STMP=$dprefix/stmp4
    PTMP=$dprefix/stmp3
    ..

In case a user does not have write permissions to ``$STMP (/scratch4/NCEPDEV/stmp4/)``, ``$STMP`` must be modified without modifying ``$DISKNM`` (i.e. ``dprefix``). Similarly, if the user does not have write permissions to ``$PTMP``, the user can set the ``$RUNDIR_ROOT`` environment variable to change the location of the run directories as described below.

.. code-block:: console

    # Overwrite default RUNDIR_ROOT if environment variable RUNDIR_ROOT is set
    RUNDIR_ROOT=${RUNDIR_ROOT:-${PTMP}/${USER}/FV3_RT}/rt_$$


Non-CCPP vs CCPP Tests
^^^^^^^^^^^^^^^^^^^^^^

While the official EMC RTs do not execute the CCPP code, GMTB provides RTs to exercise the CCPP in its various modes: ``rt_ccpp_standalone.conf`` tests the CCPP with dynamic build and ``rt_ccpp_static.conf`` tests the CCPP with static build. These tests compare the results of runs done using the CCPP against a previously generated *personal baseline* created without the CCPP by running ``rt_ccpp_ref.conf``.  For this comparison, both the non-CCPP *personal baseline* and the tests using the CCPP are performed with code built with the :term:`REPRO` compiler options.

The command below should be used to create a *personal baseline* using non-CCPP code compiled in :term:`REPRO` mode.

.. code-block:: console

    ./rt.sh -l rt_ccpp_ref.conf -c fv3 # create own reg. test baseline

Once the *personal baseline* in REPRO mode has been created, the CCPP tests can be run to compare against it. Use the ``-l`` option to select the test suite and the ``-m`` option to compare against the *personal baseline*.

.. code-block:: console

    ./rt.sh -l rt_ccpp_standalone.conf -m # dynamic build
    ./rt.sh -l rt_ccpp_static.conf -m     # static build


Compatibility between the Code Base, the SDF, and the Namelist in the UFS Atmosphere
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The variable ``suite_name`` within the ``namelist.input`` file used in the UFS Atmosphere determines which suite will be employed at run time (e.g., ``suite_name=FV3_GFS_v15``). It is the user’s responsibility to ascertain that the other variables in ``namelist.input`` are compatible with the chosen suite. When runs are executed using the RT framework described in the preceding sections, compatibility is assured. For new experiments, users are responsible for modifying the two files (``SDF`` and ``namelist.input``) consistently, since limited checks are in place.

Information about the UFS Atmosphere physics namelist can be found with the CCPP Scientific Documentation at https://dtcenter.org/GMTB/v3.0/sci_doc/.
