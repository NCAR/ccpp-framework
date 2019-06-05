.. _BuildingRunningHostModels:
  
****************************************
Building and Running Host Models
****************************************

The following instructions describe how to compile and run the CCPP code with the SCM (:numref:`Section %s <SCM>`) and with the UFS Atmosphere (:numref:`Section %s <UFSAtmo>`). Instructions are for the Theia, Jet and Cheyenne computational platforms, with examples on how to run the code on Theia included as necessary.

.. _SCM:

SCM
====================

One option for a CCPP host model is the SCM. This can be a valuable tool for diagnosing the performance of a physics suite, from validating that schemes have been integrated into a suite correctly to deep dives into how physical processes are being represented by the approximating code. In fact, this SCM likely serves as the simplest example for using the CCPP and its framework in an atmospheric model.

System Requirements, Libraries, and Tools
--------------------------------------------

The source code for the SCM and CCPP component is in the form of programs written in FORTRAN, FORTRAN 90, and C. In addition, the I/O relies on the netCDF libraries. Beyond the standard scripts, the build system relies on the use of the Python scripting language, along with cmake, GNU make and date.

The basic requirements for building and running the CCPP and SCM bundle are listed below. The versions listed reflect successful tests and there is no guarantee that the code will work with different versions.

    * FORTRAN 90+ compiler (ifort v15+, ifort v18+, gfortran v5.4+)
    * C compiler (icc v15+, gcc v5.4+)
    * cmake v2.8.11+
    * netCDF v4.x (not v3.x) with HDF5, ZLIB and SZIP
    * Python v2.7 (not v3.x)
    * Libxml2 (2.2 and 2.9.7, not 2.9.9)

Because these tools and libraries are typically the purview of system administrators to install and maintain, they are considered part of the basic system requirements.
Further, there are several utility libraries as part of the NCEPlibs package that must be installed prior to building the SCM.

    * bacio - Binary I/O library
    * sp - Spectral transformation library
    * w3nco - GRIB decoder and encoder library

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

If using Theia or Cheyenne HPC systems, this environment variable is automatically set to an appropriate installation of NCEPlibs on those machines through use of one of the setup scripts described below.

Building and Running the SCM
--------------------------------------------

Instructions for downloading the code are provided in Chapter 7. Here are the steps to compile and run SCM:

* Run the CCPP *prebuild* script to match required physics variables with those available from the dycore (SCM) and to generate physics caps and makefile segments.

    .. code-block:: console

        ./ccpp/framework/scripts/ccpp_prebuild.py --config=./ccpp/config/ccpp_prebuild_config.py [ -- debug ]

* Change directory to the top-level SCM directory.

    .. code-block:: console

        cd scm

* (Optional) Run the machine setup script if necessary. This script loads compiler modules (Fortran 2003-compliant), netCDF module, etc. and sets compiler environment variables.

    .. code-block:: console

        source etc/Theia_setup_intel.csh (for csh) or . etc/Theia_setup_intel.sh (for bash)
        source etc/Theia_setup_gnu.csh (for csh) or . etc/Theia_setup_gnu.sh (for bash)
        source etc/Theia_setup_pgi.csh (for csh) or . etc/Theia_setup_pgi.sh (for bash)
        source etc/Cheyenne_setup_intel.csh (for csh) or . etc/Cheyenne_setup_intel.sh (for bash)
        source etc/Cheyenne_setup_gnu.csh (for csh) or . etc/Cheyenne_setup_gnu.sh (for bash)
        source etc/Cheyenne_setup_pgi.csh (for csh) or . etc/Cheyenne_setup_pgi.sh (for bash)
        source etc/UBUNTU_setup.csh (for csh) or . etc/UBUNTU_setup.sh (for bash) if following the instructions in doc/README_UBUNTU.txt
        source etc/CENTOS_setup.csh (for csh) or . etc/CENTOS_setup.sh (for bash) if following the instructions in doc/README_CENTOS.txt
        source etc/MACOSX_setup.csh (for csh) or . etc/MACOSX_setup.sh (for bash) if following the instructions in doc/README_MACOSX.txt

Note: If using a local Linux or Mac system, we provide instructions for how to set up your development system (compilers and libraries) in ``doc/README_{MACOSX,UBUNTU,CENTOS}.txt``. If following these, you will need to run the respective setup script listed above. If your computing environment was previously set up to use modern compilers with an associated netCDF installation, it may not be necessary, although we recommend setting environment variables such as ``CC`` and ``FC``. For version 3.0 and above, it is required to have the ``NETCDF`` environment variable set to the path of the netCDF installation that was compiled with the same compiler used in the following steps. Otherwise, the ``cmake`` step will not complete successfully.

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

There are several test cases provided with this version of the SCM. For all cases, the SCM will go through the time steps, applying forcing and calling the physics defined in the chosen SDF using physics configuration options from an associated namelist. The model is executed through one of two python run scripts that are pre-staged into the ``bin`` directory: ``run_gmtb_scm.py`` or ``multi_run_gmtb_scm.py``. The former sets up and runs one integration while the latter sets up and runs several integrations serially.

**Single Run Script Usage**

Running a case requires three pieces of information: the case to run (consisting of initial conditions, geolocation, forcing data, etc.), the physics suite to use (through a CCPP SDF), and a physics namelist (that specifies configurable physics options to use). Cases are set up via their own namelists in ``../etc/case_config``. A default physics suite is provided as a user-editable variable in the script and default namelists are associated with each physics suite (through ``../src/default_namelists.py``), so, technically, one must only specify a case to run with the SCM. The single run script’s interface is described below.

.. code-block:: console

    ./run_gmtb_scm.py -c CASE_NAME [-s SUITE_NAME] [-n PHYSICS_NAMELIST_PATH] [-g]


When invoking the run script, the only required argument is the name of the case to run. The case name used must match one of the case configuration files located in ``../etc/case_config`` (*without the .nml extension!*). If specifying a suite other than the default, the suite name used must match the value of the suite name in one of the SDFs located in ``../../ccpp/suites`` (Note: not the filename of the SDF). As part of the third CCPP release, the following suite names are valid:

    * SCM_GFS_v15
    * SCM_GFS_v15plus
    * SCM_csawmg
    * SCM_GSD_v0

Note that using the Thompson microphysics scheme (as in ``SCM_GSD_v0``) requires the computation of look-up tables during its initialization phase. As of the release, this process has been prohibitively slow with this model, so it is HIGHLY suggested that these look-up tables are downloaded and staged to use this scheme (and the ``SCM_GSD_v0`` suite). Pre-computed tables have been created and are available for download at the following URLs:
    * https://dtcenter.org/GMTB/freezeH2O.dat (243 M)
    * https://dtcenter.org/GMTB/qr_acr_qg.dat (49 M)
    * https://dtcenter.org/GMTB/qr_acr_qs.dat (32 M)

These files should be staged in ``gmtb-scm/scm/data/GFS_physics_data`` prior to running ``cmake``. Since binary files can be system-dependent (due to endianness), it is possible that these files will not be read correctly on your system. For reference, the linked files were generated on Theia using the Intel v18 compiler.

Also note that some cases require specified surface fluxes. Special SDFs that correspond to the suites listed above have been created and use the ``*_prescribed_surface`` decoration. It is not necessary to specify this filename decoration when specifying the suite name. If the ``spec_sfc_flux`` variable in the configuration file of the case being run is set to ``.true.``, the run script will automatically use the special SDF that corresponds to the chosen suite from the list above.

If specifying a namelist other than the default, the value must be an entire filename that exists in ``../../ccpp/physics_namelists``. Caution should be exercised when modifying physics namelists since some redundancy between flags to control some physics parameterizations and scheme entries in the CCPP SDFs currently exists. Values of numerical parameters are typically OK to change without fear of inconsistencies. Lastly, the ``-g`` flag can be used to run the executable through the ``gdb`` debugger (assuming it is installed on the system).

If the run aborts with the error message

.. code-block:: console

    gmtb_scm: libccppphys.so.X.X.X: cannot open shared object file: No such file or directory

the environment variable ``LD_LIBRARY_PATH`` must be set to

.. code-block:: console

    export LD_LIBRARY_PATH=$PWD/ccpp/physics:$LD_LIBRARY_PATH

before running the model.

A netCDF output file is generated in the location specified in the case configuration file, if the ``output_dir`` variable exists in that file. Otherwise an output directory is constructed from the case, suite, and namelist used (if different from the default). All output directories are placed in the bin directory. Any standard netCDF file viewing or analysis tools may be used to examine the output file (ncdump, ncview, NCL, etc).

**Multiple Run Script Usage**

A second Python script is provided for automating the execution of multiple integrations through repeated calling of the single run script. From the run directory, one may use this script through the following interface.

.. code-block:: console

    ./multi_run_gmtb_scm.py {[-c CASE_NAME] [-s SUITE_NAME] [-f PATH_TO_FILE]} [-v{v}] [-t]

No arguments are required for this script. The ``-c`` or ``--case``, ``-s`` or ``–-suite``, or ``-f`` or ``–-file`` options form a mutually-exclusive group, so exactly one of these is allowed at one time. If ``–c`` is specified with a case name, the script will run a set of integrations for all supported suites (defined in ``../src/supported_suites.py``) for that case. If ``-s`` is specified with a suite name, the script will run a set of integrations for all supported cases (defined in ``../src/supported_cases.py``) for that suite. If ``-f`` is specified with the path to a filename, it will read in lists of cases, suites, and namelists to use from that file. If multiple namelists are specified in the file, there either must be one suite specified or the number of suites must match the number of namelists. If none of the ``-c`` or ``--case``, ``-s`` or ``–-suite``, or ``-f`` or ``–-file`` options group is specified, the script will run through all permutations of supported cases and suites (as defined in the files previously mentioned).

In addition to the main options, some helper options can also be used with any of those above. The ``-vv`` or ``–-verbose`` option can be used to output more information from the script to the console and to a log file. If this option is not used, only completion progress messages are written out. If ``-v`` is used, the script will write out completion progress messages and all messages and output from the single run script. If ``-vv`` is used, the script will also write out all messages and single run script output to a log file (``multi_run_gmtb_scm.log``) in the ``bin`` directory. The final option, ``-t`` or ``–-timer``, can be used to output the elapsed time for each integration executed by the script. Note that the execution time includes file operations performed by the single run script in addition to the execution of the underlying (Fortran) SCM executable. By default, this option will execute one integration of each subprocess. Since some variability is expected for each model run, if greater precision is required, the number of integrations for timing averaging can be set through an internal script variable. This option can be useful, for example, for getting a rough idea of relative computational expense of different physics suites.

**Batch Run Script**

If using the model on HPC resources and significant amounts of processor time is anticipated for the experiments, it will likely be necessary to submit a job through the HPC’s batch system. An example script has been included in the repository for running the model on Theia’s batch system (SLURM). It is located in ``gmtb-scm/scm/etc/gmtb_scm_slurm_example.py``. Edit the job_name, account, etc. to suit your needs and copy to the ``bin`` directory. The case name to be run is included in the command variable. To use, invoke

.. code-block:: console

    ./gmtb_scm_slurm_example.py

from the ``bin`` directory.

Additional information on the SCM can be found at https://dtcenter.org/gmtb/users/ccpp/docs/SCM-CCPP-Guide_v3.0.pdf.




.. _UFSAtmo:

UFS Atmosphere
====================

Another option for a CCPP host model is the UFS Atmosphere, located in the umbrella repository NEMSfv3gfs.

System Requirements, Libraries, and Compilers
---------------------------------------------

A number of NCEP libraries are required to build and run FV3 and are listed in :numref:`Table %s <NCEP_lib_FV3>`.

.. _NCEP_lib_FV3:

.. table:: NCEP production libraries required to build/run FV3

    +---------------------------+----------------------------------------------------+
    | Library name/version      | Description                                        |
    +===========================+====================================================+
    | bacio                     | NCEP binary I/O library                            |
    +---------------------------+----------------------------------------------------+
    | ip                        | NCEP general interpolation library                 |
    +---------------------------+----------------------------------------------------+
    | nemsio                    | NEMS I/O routines                                  |
    +---------------------------+----------------------------------------------------+
    | sp                        | NCEP spectral grid transforms                      |
    +---------------------------+----------------------------------------------------+
    | w3emc                     | NCEP/EMC library for decoding data in GRIB1 format |
    +---------------------------+----------------------------------------------------+
    | w3nco/v2.0.6              | NCEP/NCO library for decoding data in GRIB1 format |
    +---------------------------+----------------------------------------------------+
