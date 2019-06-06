.. _Host-side Coding:

**************************************************
Host Side Coding
**************************************************

This chapter describes the connection of a host model with the pool of :term:`CCPP-Physics` schemes through the :term:`CCPP-Framework`. 

In several places, references are made to an Interoperable Physics Driver (IPD). The IPD was originally developed by EMC and later expanded by NOAA GFDL with the goal of connecting GFS physics to various models. A top motivation for its development was the dycore test that led to the selection of FV3 as the dycore for the :term:`UFS`. Designed in a fundamentally different way than the :term:`CCPP`, the IPD will be phased out in the near future in favor of the CCPP as a single way to interface with physics in the UFS. To enable a smooth transition, several of the CCPP components must interact with the IPD and, as such, parts of the CCPP code in the UFS currently carry the tag “IPD”.

==================================================
Variable Requirements on the Host Model Side
==================================================

All variables required to communicate between the host model and the physics, as well as to communicate between physics schemes, need to be allocated by the host model. An exception is variables ``errflg``, ``errmsg``, ``loop_cnt``, ``blk_no``, and ``thrd_no``, which are allocated by the CCPP-Framework, as explained in :numref:`Section %s <DataStructureTransfer>`. A list of all variables required for the current pool of physics can be found in ``ccpp/framework/doc/DevelopersGuide/CCPP_VARIABLES_XYZ.pdf`` (XYZ: SCM, FV3). 

At present, only two types of variable definitions are supported by the CCPP-Framework:

* Standard Fortran variables (character, integer, logical, real) defined in a module or in the main program. For character variables, a fixed length is required. All others can have a kind attribute of a kind type defined by the host model.
* Derived data types (DDTs) defined in a module or the main program. While the use of DDTs as arguments to physics schemes in general is discouraged (see :numref:`Section %s <IOVariableRules>`), it is perfectly acceptable for the host model to define the variables requested by physics schemes as components of DDTs and pass these components to CCPP by using the correct local_name (e.g., ``myddt%thecomponentIwant``; see :numref:`Section %s <VariableTablesHostModel>`.)

.. _VariableTablesHostModel:

==================================================
Metadata Variable Tables in the Host Model
==================================================

To establish the link between host model variables and physics scheme variables, the host model must provide metadata tables similar to those presented in :numref:`Section %s <GeneralRules>`. The host model can have multiple metadata tables or just one. For each variable required by the pool of CCPP-Physics schemes, one and only one entry must exist on the host model side. The connection between a variable in the host model and in the physics scheme is made through its standard_name.

The following requirements must be met when defining variables in the host model metadata tables (see also :ref:`Listing 6.1 <example_vardefs>` for examples of host model metadata tables).

* The ``standard_name`` must match that of the target variable in the physics scheme.
* The type, kind, shape and size of the variable (as defined in the host model Fortran code) must match that of the target variable.
* The attributes ``units``, ``rank``, ``type`` and ``kind`` in the host model metadata table must match those in the physics scheme table.
* The attributes ``optional`` and ``intent`` must be set to ``F`` and ``none``, respectively.
* The ``local_name`` of the variable must be set to the name the host model cap uses to refer to the variable.
* The metadata table that exposes a DDT to the CCPP (as opposed to the table that describes the components of a DDT) must be in the same module where the memory for the DDT is allocated. If the DDT is a module variable, then it must be exposed via the module’s metadata table, which must have the same name as the module.
* Metadata tables describing module variables must be placed inside the module.
* Metadata tables describing components of DDTs must be placed immediately before the type definition and have the same name as the DDT.

.. _example_vardefs:

.. code-block:: fortran

       module example_vardefs
 
         implicit none
 
   !> \section arg_table_example_vardefs
   !! | local_name | standard_name | long_name | units | rank | type      | kind   | intent | optional |
   !! |---------------|---------------|--------------|-------|----|-----------|--------|--------|----------|
   !! | ex_int     | example_int   | ex. int      | none  |  0 | integer   |        | none   | F        |
   !! | ex_real1   | example_real1 | ex. real     | m     |  2 | real      | kind=8 | none   | F        |
   !! | ex_ddt     | ex_ddt        | ex. ddt type | DDT   |  2 | ex_ddt    |        | none   | F        |
   !! | ext      | ex_ddt_instance | ex. ddt inst | DDT   |  2 | ex_ddt    |        | none   | F        |
   !! | errmsg     | error_message | err. msg.    | none  |  0 | character | len=64 | none   | F        |
   !! | errflg     | error_flag    | err. flg.    | flag  |  0 | logical   |        | none   | F        |
   !!
         integer, parameter           :: r15 = selected_real_kind(15)
         integer                      :: ex_int
         real(kind=8), dimension(:,:) :: ex_real1
         character(len=64)            :: errmsg
         logical                      :: errflg
    
   ! Derived data types
    
   !> \section arg_table_ex_ddt
   !! | local_name | standard_name | long_name | units | rank | type      | kind   | intent | optional |
   !! |------------|---------------|-----------|-------|------|-----------|--------|--------|----------|
   !! | ext%l      | example_flag  | ex. flag  | flag  |    0 | logical   |        | none   | F        |
   !! | ext%r      | example_real3 | ex. real  | kg    |    2 | real      | r15    | none   | F        |
   !! | ext%r(:,1) | example_slice | ex. slice | kg    |    1 | real      | r15    | none   | F        |
   !!
         type ex_ddt
           logical              :: l
           real, dimension(:,:) :: r
         end type ex_ddt
    
         type(ex_ddt) :: ext
    
       end module example_vardefs


*Listing 6.1:  Example Host Model Metadata Table.  In this example, both the definition and the declaration (memory allocation) of a DDT* ``ext`` *(of type* ``ex_ddt`` *) are in the same module.*

========================================================
CCPP Variables in the SCM and UFS Atmosphere Host Models
========================================================

While the use of standard Fortran variables is preferred, in the current implementation of the CCPP in the UFS Atmosphere and in the SCM almost all data is contained in DDTs for organizational purposes. In the case of the SCM, DDTs are defined in ``gmtb_scm_type_defs.f90`` and ``GFS_typedefs.F90``, and in the case of the UFS Atmosphere, they are defined in both ``GFS_typedefs.F90`` and ``CCPP_typedefs.F90``.  The current implementation of the CCPP in both host models uses the following set of DDTs:

* ``GFS_init_type`` 		variables to allow proper initialization of GFS physics
* ``GFS_statein_type``	prognostic state data provided by dycore to physics
* ``GFS_stateout_type``	prognostic state after physical parameterizations
* ``GFS_sfcprop_type``	surface properties read in and/or updated by climatology, obs, physics
* ``GFS_coupling_type``	fields from/to coupling with other components, e.g., land/ice/ocean
* ``GFS_control_type``	control parameters input from a namelist and/or derived from others
* ``GFS_grid_type``		grid data needed for interpolations and length-scale calculations
* ``GFS_tbd_type``		data not yet assigned to a defined container
* ``GFS_cldprop_type``	cloud properties and tendencies needed by radiation from physics
* ``GFS_radtend_type``	radiation tendencies needed by physics
* ``GFS_diag_type``		fields targeted for diagnostic output to disk
* ``GFS_interstitial_type``	fields used to communicate variables among schemes in the slow physics group required to replace interstitial code in GFS_{physics, radiation}_driver.F90 in CCPP
* ``GFS_data_type``	combined type of all of the above except GFS_control_type and GFS_interstitial_type
* ``CCPP_interstitial_type`` fields used to communicate variables among schemes in the fast physics group

The DDT descriptions provide an idea of what physics variables go into which data type.  ``GFS_diag_type`` can contain variables that accumulate over a certain amount of time and are then zeroed out. Variables that require persistence from one timestep to another should not be included in the ``GFS_diag_type`` nor the ``GFS_interstitial_type`` DDTs. Similarly, variables that need to be shared between groups cannot be included in the ``GFS_interstitial_type`` DDT. Although this memory management is somewhat arbitrary, new variables provided by the host model or derived in an interstitial scheme should be put in a DDT with other similar variables.

Each DDT contains a create method that allocates the data defined in the metadata table. For example, the ``GFS_stateout_type`` contains:

.. code-block:: fortran

 type GFS_stateout_type

    !-- Out (physics only)
    real (kind=kind_phys), pointer :: gu0 (:,:)   => null()  !< updated zonal wind
    real (kind=kind_phys), pointer :: gv0 (:,:)   => null()  !< updated meridional wind
    real (kind=kind_phys), pointer :: gt0 (:,:)   => null()  !< updated temperature
    real (kind=kind_phys), pointer :: gq0 (:,:,:) => null()  !< updated tracers

    contains
      procedure :: create  => stateout_create  !<   allocate array data
  end type GFS_stateout_type

In this example, ``gu0``, ``gv0``, ``gt0``, and ``gq0`` are defined in the host-side metadata table, and when the subroutine ``stateout_create`` is called, these arrays are allocated and initialized to zero.  With the CCPP, it is possible to not only refer to components of DDTs, but also to slices of arrays in the metadata table as long as these are contiguous in memory. An example of an array slice from the ``GFS_stateout_type`` looks like:

.. code-block:: fortran

  !! | GFS_Data(cdata%blk_no)%Stateout%gq0(:,:,GFS_Control%ntsw)    | snow_water_mixing_ratio_updated_by_physics                             | moist (dry+vapor, no condensates) mixing ratio of snow water updated by physics            | kg kg-1 |    2 | real    | kind_phys | none   | F   

Array slices can be used by physics schemes that only require certain values from an array. 

.. _CCPP_API:

========================================================
CCPP API 
========================================================

The CCPP Application Programming Interface (API) is comprised of a set of clearly defined methods used to communicate variables between the host model and the physics and to run the physics. The bulk of the CCPP API is located in the CCPP-Framework, and is described in file ccpp_api.F90. Some aspects of the API differ between the dynamic and static build. In particular, subroutines ccpp_physics_init, ccpp_physics_finalize, and ccpp_physics_run (described below) are made public from ccpp_api.F90 for the dynamic build, and are contained in ccpp_static_api.F90 for the static build. Moreover, these subroutines take an additional argument (suite_name) for the static build. File ccpp_static_api.F90 is auto-generated when the script ccpp_prebuild.py is run for the static build.

.. _DataStructureTransfer:

,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
Data Structure to Transfer Variables between Dynamics and Physics 
,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

The roles of cdata structure in dealing with data exchange are not the same between the dynamic and the static builds of the CCPP. For the dynamic build, the cdata structure handles the data exchange between the host model and the physics schemes. cdata is a DDT containing a list of pointers to variables and their metadata and is persistent in memory. 

For both the dynamic and static builds, the cdata structure is used for holding five variables that must always be available to the physics schemes. These variables are listed in a metadata table in ccpp/framework/src/ccpp_types.F90 (:ref:`Listing 6.2 <MandatoryVariables>`). 


* Error flag for handling in CCPP (``errmsg``).
* Error message associated with the error flag (``errflg``).
* Loop counter for subcycling loops (``loop_cnt``).
* Number of block for explicit data blocking in CCPP (``blk_no``).
* Number of thread for threading in CCPP (``thrd_no``).

.. _MandatoryVariables:

.. code-block:: fortran

 !! | local_name                        | standard_name             | long_name                                             | units   | rank | type      |   kind   | intent | optional |
 !! |-----------------------------------|-------------------------- |-------------------------------------------------------|---------|------|-----------|----------|--------|----------|
 !! | cdata%errflg                      | ccpp_error_flag           | error flag for error handling in CCPP                 | flag    |    0 | integer   |          | none   | F        |
 !! | cdata%errmsg                      | ccpp_error_message        | error message for error handling in CCPP              | none    |    0 | character | len=512  | none   | F        |
 !! | cdata%loop_cnt                    | ccpp_loop_counter         | loop counter for subcycling loops in CCPP             | index   |    0 | integer   |          | none   | F        |
 !! | cdata%blk_no                      | ccpp_block_number         | number of block for explicit data blocking in CCPP    | index   |    0 | integer   |          | none   | F        |
 !! | cdata%thrd_no                     | ccpp_thread_number        | number of thread for threading in CCPP                | index   |    0 | integer   |          | none   | F        |
 !!

*Listing 6.2: Mandatory variables provided by the CCPP-Framework from* ``ccpp/framework/src/ccpp_types.F90`` *.
These variables must not be defined by the host model.*

Two of the variables are mandatory and must be passed to every physics scheme: ``errmsg`` and ``errflg``. The variables ``loop_cnt``, ``blk_no``, and ``thrd_no`` can be passed to the schemes if required, but are not mandatory.  For the static build of the CCPP, the ``cdata`` structure is only used to hold these five variables, since the host model variables are directly passed to the physics without the need for an intermediate data structure.

Note that ``cdata`` is not restricted to being a scalar but can be a multidimensional array, depending on the needs of the host model. For example, a model that uses a one-dimensional array of blocks for better cache-reuse may require ``cdata`` to be a one-dimensional array of the same size. Another example of a multi-dimensional array of ``cdata`` is in the SCM, which uses a one-dimensional cdata array for N independent columns. 

Due to a restriction in the Fortran language, there are no standard pointers that are generic pointers, such as the C language allows. The CCPP system therefore has an underlying set of pointers in the C language that are used to point to the original data within the host application cap. The user does not see this C data structure, but deals only with the public face of the Fortran cdata DDT. The type ``ccpp_t`` is defined in ``ccpp/framework/src/ccpp_types.F90``.

,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
Adding and Retrieving Information from cdata (dynamic build option)
,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

Subroutines ``ccpp_field_add`` and ``ccpp_field_get`` are part of the CCPP-Framework and are used (in the dynamic build only) to load and retrieve information to and from ``cdata``. The calls to ``ccpp_field_add`` are auto-generated by the script ``ccpp_prebuild.py`` and inserted onto the host model code via include files (i.e. ``FV3/CCPP_layer/ccpp_fields_slow_physics.inc``) before it is compiled.

A typical call to ``ccpp_field_add`` is below, where the first argument is the instance of ``cdata`` to which the information should be added, the second argument is the standard_name of the variable, the third argument is the corresponding host model variable, the fourth argument is an error flag, the fifth argument is the units of the variable, and the last (optional) argument is the position within ``cdata`` in which the variable is expected to be stored.

.. code-block:: fortran

 call ccpp_field_add(cdata, 'y_wind_updated_by_physics', GFS_Data(cdata%blk_no)%Stateout%gv0, ierr=ierr, units='m s-1', index=886)

Each new variable added to ``cdata`` is always placed at the next free position, and a check is performed to confirm that this position corresponds to the expected one, which in this example is 886.  A mismatch will occur if a developer manually adds a call to ``ccpp_field_add``, in which case a costly binary search is applied every time a variable is retrieved from memory. Adding calls manually is not recommended as all calls to ``ccpp_fields_add`` should be auto-generated.

The individual physics caps used in the dynamic build, which are auto-generated using the script ``ccpp_prebuild.py``, contain calls to ``ccpp_field_get`` to pull data from the ``cdata`` DDT as a Fortran pointer to a variable that will be passed to the individual physics scheme. 

,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
Initializing and Finalizing the CCPP
,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

At the beginning of each run, the ``cdata`` structure needs to be set up. Similarly, at the end of each run, it needs to be terminated. This is done with subroutines ``ccpp_init`` and ``ccpp_finalize``. These subroutines should not be confused with ``ccpp_physics_init`` and ``ccpp_physics_finalize``, which were described in :numref:`Chapter %s <AutoGenPhysCaps>`.

Note that optional arguments are denoted with square brackets.

.. _SuiteInitSubroutine:

^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Suite Initialization Subroutine 	
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The suite initialization subroutine, ``ccpp_init``, takes three mandatory and two optional arguments. The mandatory arguments are the name of the suite (of type character), the name of the ``cdata`` variable that must be allocated at this point, and an integer used for the error status. Note that the suite initialization routine ``ccpp_init`` parses the SDF corresponding to the given suite name and initializes the state of the suite and its schemes. This process must be repeated for every element of a multi-dimensional ``cdata``. For performance reasons, it is possible to avoid repeated reads of the SDF and to have a single state of the suite shared between the elements of ``cdata``. To do so, specify an optional argument variable called ``cdata_target = X`` in the call to ``ccpp_init``, where X refers to the instance of ``cdata`` that has already been initialized.

For a given suite name XYZ, the name of the suite definition file is inferred as ``suite_XYZ.xml``, and the file is expected to be present in the current run directory. It is possible to specify the optional argument ``is_filename=.true.`` to ``ccpp_init``, which will treat the suite name as an actual file name (with or without the path to it).

Typical calls to ``ccpp_init`` are below, where ``ccpp_suite`` is the name of the suite, and ``ccpp_sdf_filepath`` the actual SDF filename, with or without a path to it.

.. code-block:: fortran

 call ccpp_init(trim(ccpp_suite), cdata, ierr)
 call ccpp_init(trim(ccpp_suite), cdata2, ierr, [cdata_target=cdata])

 call ccpp_init(trim(ccpp_sdf_filepath), cdata, ierr, [is_filename=.true.])

^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Suite Finalization Subroutine
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The suite finalization subroutine, ``ccpp_finalize``, takes two arguments, the name of the ``cdata`` variable that must be de-allocated at this point, and an integer used for the error status. A typical call to ``ccpp_finalize`` is below:

.. code-block:: fortran

 call ccpp_finalize(cdata, ierr)

If a specific data instance was used in a call to ``ccpp_init``, as in the above example in :numref:`Section %s <SuiteInitSubroutine>`, then this data instance must be finalized last:

.. code-block:: fortran

 call ccpp_finalize(cdata2, ierr)
 call ccpp_finalize(cdata, ierr)

,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
Running the physics
,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

The physics is invoked by calling subroutine ``ccpp_physics_run``. This subroutine is part of the CCPP API and is included with the CCPP-Framework (for the dynamic build) or auto-generated (for the static build). This subroutine is capable of executing the physics with varying granularity, that is, a single scheme (dynamic build only), a single group, or an entire suite can be run with a single subroutine call. Typical calls to ccpp_physics_run are below, where ``scheme_name`` and ``group_name`` are optional and mutually exclusive (dynamic build), and where ``suite_name`` is mandatory and ``group_name`` is optional (static build).

Dynamic build:

.. code-block:: fortran

 call ccpp_physics_run(cdata, [group_name], [scheme_name], ierr=ierr)

Static build:

.. code-block:: fortran

 call ccpp_physics_run(cdata, suite_name, [group_name], ierr=ierr)

,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
Initializing and Finalizing the Physics
,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

Many (but not all) physical parameterizations need to be initialized, which includes functions such as reading lookup tables, reading input datasets, computing derived quantities, broadcasting information to all MPI ranks, etc. Initialization procedures are typically done for the entire domain, that is, they are not subdivided by blocks. Similarly, many (but not all) parameterizations need to be finalized, which includes functions such as deallocating variables, resetting flags from *initialized* to *non-initiaIized*, etc. Initialization and finalization functions are each performed once per run, before the first call to the physics and after the last call to the physics, respectively.

The initialization and finalization can be invoked for a single parameterization (only in dynamic build), for a single group, or for the entire suite. In all cases, subroutines ``ccpp_physics_init`` and ``ccpp_physics_finalize`` are used and the arguments passed to those subroutines determine the type of initialization.

These subroutines should not be confused with ``ccpp_init`` and ``ccpp_finalize``, which were explained previously.

^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Subroutine ``ccpp_physics_init``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This subroutine is part of the CCPP API and is included with the CCPP-Framework (for the dynamic build) or auto-generated (for the static build). It cannot contain thread-dependent information but can have block-dependent information. Typical calls to ``ccpp_physics_init`` are below.

Dynamic build:

.. code-block:: fortran

 call ccpp_physics_init(cdata, [group_name], [scheme_name], ierr=ierr)

Static build:

.. code-block:: fortran

 call ccpp_physics_init(cdata, suite_name, [group_name], ierr=ierr)

^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Subroutine ``ccpp_physics_finalize``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This subroutine is part of the CCPP API and is included with the CCPP-Framework (for the dynamic build) or auto-generated (for the static build). Typical calls to ``ccpp_physics_finalize`` are below.

Dynamic build:

.. code-block:: fortran

 call ccpp_physics_finalize(cdata, [group_name], [scheme_name], ierr=ierr)

Static build:

.. code-block:: fortran

 call ccpp_physics_finalize(cdata, suite_name, [group_name], ierr=ierr)

========================================================
Host Caps
========================================================

The purpose of the host model cap is to abstract away the communication between the host model and the CCPP-Physics schemes. While CCPP calls can be placed directly inside the host model code (as is done for the relatively simple SCM), it is recommended to separate the cap in its own module for clarity and simplicity (as is done for the UFS Atmosphere). While the details of implementation will be specific to each host model, the host model cap is responsible for the following general functions:

* Allocating memory for variables needed by physics

  * All variables needed to communicate between the host model and the physics, and all variables needed to communicate among physics schemes, need to be allocated by the host model. The latter, for example for interstitial variables used exclusively for communication between the physics schemes, are typically allocated in the cap. 


* Allocating the cdata structure(s)					

  * For the dynamic build, the cdata structure handles the data exchange between the host model and the physics schemes, while for the static build, cdata is utilized in a reduced capacity. 


* Calling the suite initialization subroutine				

  * The suite must be initialized using ``ccpp_init``.


* Populating the cdata structure(s)					

  * For the dynamic build, each variable required by the physics schemes must be added to the cdata structure (or to each element of a multi-dimensional cdata) on the host model side using subroutine ``ccpp_field_add``. This is an automated task accomplished by inserting a preprocessor directive at the top of the cap (before implicit none) to load the required modules and a second preprocessor directive after the ``cdata`` variable and the variables required by the physics schemes are allocated and after the call to ``ccpp_init`` for this ``cdata`` variable. For the static build, this step can be skipped because the autogenerated caps for the physics (groups and suite caps) are automatically given memory access to the host model variables and they can be used directly, without the need for a data structure containing pointers to the actual variables (which is what ``cdata`` is).
					
.. code-block:: fortran

 #include ccpp_modules.inc

 #include ccpp_fields.inc
					
* Note. The CCPP-Framework supports splitting physics schemes into different sets that are used in different parts of the host model. An example is the separation between slow and fast physics processes for the GFDL microphysics implemented in the UFS Atmosphere: while the slow physics are called as part of the usual model physics, the fast physics are integrated in the dynamical core. The separation of physics into different sets is determined in the CCPP prebuild configuration for each host model (see :numref:`Chapter %s <DynamicBuildCaps>`, and :numref:`Figure %s <ccpp_prebuild>`), which allows to create multiple include files (e.g. ``ccpp_fields_slow_physics.inc`` and ``ccpp_fields_fast_physics.inc`` that can be used by different ``cdata`` structures in different parts of the model). This is a highly advanced feature and developers seeking to take further advantage of it should consult with GMTB first.


* Providing interfaces to call the CCPP

  * The cap must provide functions or subroutines that can be called at the appropriate places in the host model time integration loop and that internally call ``ccpp_init``, ``ccpp_physics_init``, ``ccpp_physics_run``, ``ccpp_physics_finalize`` and ``ccpp_finalize``, and handle any errors returned See :ref:`Listing 6.3 <example_ccpp_host_cap>`. 

.. _example_ccpp_host_cap:

.. code-block:: fortran
 
 module example_ccpp_host_cap
  
   use ccpp_api, only: ccpp_t, ccpp_field_add, ccpp_init, ccpp_finalize, &
                ccpp_physics_init, ccpp_physics_run, ccpp_physics_finalize
   use iso_c_binding, only: c_loc
 ! Include auto-generated list of modules for ccpp
 #include "ccpp_modules.inc"
   implicit none
 ! CCPP data structure
   type(ccpp_t), save, target :: cdata
   public :: physics_init, physics_run, physics_finalize
 contains
  
   subroutine physics_init(ccpp_suite_name)
     character(len=*), intent(in) :: ccpp_suite_name
     integer :: ierr
     ierr = 0
     ! Initialize the CCPP framework, parse SDF
     call ccpp_init(ccpp_suite_name, cdata, ierr=ierr)
     if (ierr/=0) then
       write(*,'(a)') "An error occurred in ccpp_init"
       stop
     end if
 ! Include auto-generated list of calls to ccpp_field_add
 #include "ccpp_fields.inc"
     ! Initialize CCPP physics (run all _init routines)
     call ccpp_physics_init(cdata, ierr=ierr)
     ! error handling as above
   end subroutine physics_init
  
   subroutine physics_run(group, scheme)
     ! Optional arguments group and scheme can be used
     ! to run a group of schemes or an individual scheme
     ! defined in the SDF. Otherwise, run entire suite.
     character(len=*), optional, intent(in) :: group
     character(len=*), optional, intent(in) :: scheme
     integer :: ierr
     ierr = 0
     if (present(scheme)) then
        call ccpp_physics_run(cdata, scheme_name=scheme, ierr=ierr)
     else if (present(group)) then
        call ccpp_physics_run(cdata, group_name=group, ierr=ierr)
     else
        call ccpp_physics_run(cdata, ierr=ierr)
     end if
     ! error handling as above
   end subroutine physics_run
  
   subroutine physics_finalize()
     integer :: ierr
     ierr = 0
     ! Finalize CCPP physics (run all _finalize routines)
     call ccpp_physics_finalize(cdata, ierr=ierr)
     ! error handling as above
     call ccpp_finalize(cdata, ierr=ierr)
     ! error handling as above
   end subroutine physics_finalize
 end module example_ccpp_host_cap

*Listing 6.3: Fortran template for a CCPP host model cap
REF## --- also notes in google-doc --- needs updated example code!!!*

,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
SCM Host Cap
,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

The only build type available for the SCM is the dynamic build. The cap functions are mainly implemented in:

``gmtb-scm/scm/src/gmtb_scm.F90``

With smaller parts in:

``gmtb-scm/scm/src/gmtb_scm_type_defs.f90``

``gmtb-scm/scm/src/gmtb_scm_setup.f90``

``gmtb-scm/scm/src/gmtb_scm_time_integration.f90``


The host model cap is responsible for:

* Allocating memory for variables needed by physics 

  All variables and constants required by the physics are in the host-side metadata tables, ``arg_table_physics_type`` and ``arg_table_gmtb_scm_physical_constants``, which are implemented in ``gmtb_scm_type_defs.f90`` and ``gmtb_scm_physical_constants.f90``. To mimic the UFS Atmosphere and to hopefully reduce code maintenance, currently, the SCM uses GFS DDTs as sub-types within the physics DDT.

  In ``gmtb_scm_type_defs.f90``, the physics DDT has a create type-bound procedure (see subroutine ``physics_create`` and ``type physics_type``), which allocates GFS sub-DDTs and other physics variables and initializes them with zeros. ``physics%create`` is called from ``gmtb_scm.F90`` after the initial SCM state has been set up.

* Allocating the cdata structure 

  The SCM uses a one-dimensional ``cdata`` array for N independent columns, i.e. in ``gmtb_scm.F90``:

  ``allocate(cdata(scm_state%n_cols))``

* Calling the suite initialization subroutine 

  Within ``scm_state%n_cols`` loop in ``gmtb_scm.F90`` after initial SCM state setup and before first timestep, the suite initialization subroutine ``ccpp_init`` is called for each column with own instance of ``cdata``, and takes three arguments, the name of the runtime SDF, the name of the cdata variable that must be allocated at this point, and ierr. 
 
* Populating the cdata structure 

  Within the same ``scm_state%n_cols`` loop, but after the ``ccpp_init`` call, the ``cdata`` structure is filled in with real initialized values:

 * ``physics%Init_parm`` (GFS DDT for setting up suite) are filled in from ``scm_state%``

 * call ``GFS_suite_setup()``: similar to ``GFS_initialize()`` in the UFS Atmosphere, is called and includes:

  * ``%init/%create`` calls for GFS DDTs

  * initialization for other variables in physics DDT

  * ini calls for legacy non-ccpp schemes

 * call ``physics%associate()``: to associate pointers in physics DDT with targets in ``scm_state``, which contains variables that are modified by the SCM “dycore” (i.e. forcing).

 * Actual cdata fill in through ``ccpp_field_add`` calls:

  ``#include “ccpp_fields.inc”``

  This include file is auto-generated from ``ccpp/scripts/ccpp_prebuild.py``, which parses tables in ``gmtb_scm_type_defs.f90``.

* Providing interfaces to call the CCPP

 * Calling ``ccpp_physics_init()``

  Within the same ``scm_state%n_cols`` loop but after ``cdata`` is filled, the physics initialization routines (\*_init()) associated with the physics suite, group, and/or schemes are called at each column.

 * Calling ``ccpp_physics_run()``

  At the first timestep, if the forward scheme is selected (i.e. ``scm_state%time_scheme == 1``), call ``do_time_step()`` to apply forcing and ``ccpp_physics_run()`` calls at each column; if the leapfrog scheme is selected (i.e. ``scm_state%time_scheme == 2``), call ``ccpp_physics_run()`` directly at each column.

  At a later time integration, call ``do_time_step()`` to apply forcing and ``ccpp_physics_run()`` calls at each column. Since there is no need to execute anything between physics groups in the SCM, the ``ccpp_physics_run`` call is only given cdata and an error flag as arguments.

 * Calling ``ccpp_physics_finalize()`` and ``ccpp_finalize()``

  ``ccpp_physics_finalize()`` and ``ccpp_finalize()`` are called after the time loop at each column.

,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
UFS Atmosphere Host Cap
,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

For the UFS Atmosphere, there are slightly different versions of the host cap implementation depending on the desired build type (dynamic orstatic). As discussed in :numref:`Chapter %s <CCPPPreBuild>`, these modes are controlled via appropriate strings included in the MAKEOPTS build-time argument. Within the source code, the three modes are executed within appropriate pre-processor directive blocks:

For any build that uses CCPP (dynamic orstatic):

.. code-block:: fortran

 #ifdef CCPP
 #endif

For static (often nested within #ifdef CCPP):

.. code-block:: fortran

 #ifdef STATIC
 #endif

The following text describes how the host cap functions listed above are implemented for the dynamic build only. Where the other modes of operation differ in their implementation, it will be called out.

* Allocating memory for variables needed by physics

 * Within the atmos_model_init subroutine of atmos_model.F90, the following statement is executed

  ``allocate(IPD_Data)``

  ``IPD_Data`` is of ``IPD_data_type``, which is defined in ``IPD_typedefs.F90`` as a synonym for ``GFS_data_type`` defined in ``GFS_typedefs.F90``. This data type contains GFS-related DDTs (``GFS_statein_type``, ``GFS_stateout_type``, ``GFS_sfcprop_type``, etc.) as sub-types, which are defined in ``GFS_typedefs.F90``.

* Allocating the cdata structures

 * For the current implementation of the UFS Atmosphere, which uses a subset of fast physics processes tightly coupled to the dynamical core, three instances of ``cdata`` exist within the host model: ``cdata_tile`` to hold data for the fast physics, ``cdata_domain`` to hold data needed for all UFS Atmosphere blocks for the slow physics, and ``cdata_block``, an array of ``cdata`` DDTss with dimensions of (``number of blocks``, ``number of threads``) to contain data for individual block/thread combinations for the slow physics. All are defined as module-level variables in the ``CCPP_data module`` of ``CCPP_data.F90``. The ``cdata_block`` array is allocated (since the number of blocks and threads is unknown at compile-time) as part of the ‘init’ step of the ``CCPP_step subroutine`` in ``CCPP_driver.F90``. Note: Although the ``cdata`` containers are not used to hold the pointers to the physics variables for the static mode, they are still used to hold other CCPP-related information for that mode.

* Calling the suite initialization subroutine

 * Corresponding to the three instances of ``cdata`` described above, the ``ccpp_init`` subroutine is called within three different contexts, all originating from the ``atmos_model_init`` subroutine of ``atmos_model.F90``:

  * For ``cdata_tile`` (used for the fast physics), the ``ccpp_init`` call is made from the ``atmosphere_init`` subroutine of ``atmosphere.F90``. Note: when fast physics is used, this is the *first* call to ``ccpp_init``, so it reads in the SDF and initializes the suite in addition to setting up the fields for ``cdata_tile``.

  * For ``cdata_domain`` and ``cdata_block`` used in the rest of the physics, the ‘init’ step of the ``CCPP_step`` subroutine in ``CCPP_driver.F90`` is called. Within that subroutine, ``ccpp_init`` is called once to set up ``cdata_domain`` and within a loop for every block/thread combination to set up the components of the ``cdata_block`` array. Note: as mentioned in the CCPP API :numref:`Section %s <CCPP_API>`, when fast physics is used, the SDF has already been read and the suite is already setup, so this step is skipped and the suite information is simply copied from what was already initialized (``cdata_tile``) using the ``cdata_target`` optional argument.

* Populating the ``cdata`` structures

 * When the dynamic mode is used, the ``cdata`` structures are filled with pointers to variables that are used by physics and whose memory is allocated by the host model. This is done using ``ccpp_field_add`` statements contained in the autogenerated include files. For the fast physics, this include file is named ``ccpp_fields_fast_physics.inc`` and is placed after the call to ``ccpp_init`` for ``cdata_tile`` in the ``atmosphere_init`` subroutine of ``atmosphere.F90``. For populating ``cdata_domain`` and ``cdata_block``, IPD data types are initialized in the ``atmos_model_init`` subroutine of ``atmos_model.F90``. The ``Init_parm`` DDT is filled directly in this routine and ``IPD_initialize`` (pointing to ``GFS_initialize`` and for populating diagnostics and restart DDTs) is called in order to fill the GFS DDTs that are used in the physics. Once the IPD data types are filled, they are passed to the ‘init’ step of the ``CCPP_step`` subroutine in ``CCPP_driver.F90`` where ``ccpp_field_add`` statements are included in ``ccpp_fields_slow_physics.inc`` after the calls to ``ccpp_init`` for the ``cdata_domain`` and ``cdata_block`` containers.

 * Note: for the static mode, filling of the cdata containers with pointers to physics variables is not necessary. This is because the autogenerated caps for the physics groups (that contain calls to the member schemes) can fill in the argument variables without having to retrieve pointers to the actual data. This is possible because the host model metadata tables (that are known at ccpp_prebuild time) contain all the information needed about the location (DDTs and local names) to pass into the autogenerated caps for their direct use.

* Providing interfaces to call the CCPP

 * Calling ``ccpp_physics_init``

  * In order to call the initialization routines for the physics, ``ccpp_physics_init`` is called in the ``atmosphere_init`` subroutine of ``atmosphere.F90`` after the included ``ccpp_field_add`` calls for the fast physics. For the slow physics, the ‘physics_init’ step of the ``CCPP_step`` subroutine in ``CCPP_driver.F90`` is invoked immediately after the call to the ‘init’ step in the ``atmos_model_init`` subroutine of ``atmos_model.F90``. Within the ‘physics_init’ step,  calls to ``ccpp_physics_init`` for all blocks are executed.

  * Note: for the static mode, ``ccpp_physics_init`` is autogenerated and contained within ``ccpp_static_api.F90``. As mentioned in the :numref:`CCPP API Section %s <CCPP_API>` , it can be called to initialize groups as defined in the SDFs or the suite as a whole, depending on whether a group name is passed in as an optional argument.

 * Calling ``ccpp_physics_run``

  * For actually running the physics within the FV3 time loop, ``ccpp_physics_run`` is called from a couple of different places in the FV3 source code. For the fast physics, ``ccpp_physics_run`` is called for the fast physics group from the ``Lagrangian_to_Eulerian`` subroutine of ``fv_mapz.F90`` within the dynamical core. For the rest of the physics, the subroutine ``update_atmos_radiation_physics`` in ``atmos_model.F90`` is called as part of the FV3 time loop. Within that subroutine, the various physics steps (defined as groups within a SDF) are called one after the other. The ‘time_vary’ step of the ``CCPP_step`` subroutine within ``CCPP_driver.F90`` is called. Since this step is called over the entire domain, the call to ``ccpp_physics_run`` is done once using ``cdata_domain`` and the time_vary group.  The ‘radiation’, ‘physics’, and ‘stochastics’ steps of the ``CCPP_step`` subroutine are called next. For each of these steps within ``CCPP_step``, there is a loop over the number of blocks for calling ``ccpp_physics_run`` with the appropriate group and component of the ``cdata_block`` array for the current block and thread.

  * Note: The execution of calls to ``ccpp_physics_run`` is different for the three build types. For the static mode, ``ccpp_physics_run`` is called from ``ccpp_static_api.F90`` and contains autogenerated caps for groups and the suite as a whole as defined in the SDFs. 

 * calling ``ccpp_physics_finalize`` and ``ccpp_finalize``

  * At the conclusion of the FV3 time loop, calls to finalize the physics are executed. For the fast physics, ``ccpp_physics_finalize`` is called from the ``atmosphere_end`` subroutine of ``atmosphere.F90``. For the rest of the physics, the ‘finalize’ step of the ``CCPP_step`` subroutine in ``CCPP_driver.F90`` is called from the ``atmos_model_end`` subroutine in ``atmos_model.F90``. Within the ‘finalize’ step of ``CCPP_step``, calls for ``ccpp_physics_finalize`` and ``ccpp_finalize`` are executed for every thread and block for ``cdata_block``. Afterward, ``ccpp_finalize`` is called for ``cdata_domain`` and lastly, ``cdata_tile``. (That is, the calls to ``ccpp_finalize`` are in reverse order than the calls to ``ccpp_initialize``.) In addition, ``cdata_block`` is also deallocated in the ‘finalize’ step of ``CCPP_step``.

  * Note: for the static mode, ``ccpp_physics_finalize`` is autogenerated and contained within ``ccpp_static_api.F90``. As mentioned in the :numref:`CCPP API Section %s <CCPP_API>`, it can be called to finalize groups as defined in the current SDFs or the suite as a whole, depending on whether a group name is passed in as an optional argument.
