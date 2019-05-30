.. _CompliantPhysParams:

****************************************
CCPP-Compliant Physics Parameterizations
****************************************

The rules for a scheme to be considered CCPP-compliant are summarized in this section. It
should be noted that making a scheme CCPP-compliant is a necessary but not guaranteed step
for the acceptance of the scheme in the pool of supported CCPP-Physics. Acceptance is
dependent on scientific innovation, demonstrated value, and compliance with the rules
described below. The criteria for acceptance of a scheme into the CCPP is under development.

It is recommended that parameterizations be comprised of the smallest units that will be used.
For example, if a given set of deep and shallow convection schemes will always be called together
and in a pre-established order, it is acceptable to group them within a single scheme. However, if one
envisions that the deep and shallow convection schemes may someday operate independently, it is
recommended to code two separate schemes to allow more flexibility.  

Some schemes in the CCPP have been implemented using a driver as an entry point. In this context,
a driver is defined as a wrapper that sits on top of the actual scheme and provides the CCPP entry
points. In order to minimize the layers of code in the CCPP, the implementation of a driver is
discouraged, that is, it is preferable that the CCPP be composed of atomic parameterizations. One
example is the implementation of the MG microphysics, in which a simple entrypoint
leads to two versions of the scheme, MG2 and MG3.  A cleaner implementation would be to retire MG2
in favor of MG3, to put MG2 and MG3 as separate schemes, or to create a single scheme that can behave
as MG2 nd MG3 depending on namelist options.

The implementation of a driver is reasonable under the following circumstances:

* To preserve schemes that are also distributed outside of the CCPP. For example, the Thompson
  microphysics scheme is distributed both with the Weather Research and Forecasting (WRF) model
  and with the CCPP. Having a driver with CCPP directives allows the Thompson scheme to remain
  intact so that it can be synchronized between the WRF model and the CCPP distributions. See
  more in ``mp_thompson_hrrr.F90`` in the ``ccpp-physics/physics`` directory.

* To deal with optional arguments. A driver can check whether optional arguments have been
  provided by the host model to either write out a message and return an error code or call a
  subroutine with or without optional arguments. For example, see ``mp_thompson_hrrr.F90``,
  ``radsw_main.f``, or ``radlw_main.f`` in the ``ccpp-physics/physics`` directory.
 
* To perform unit conversions or array transformations, such as flipping the vertical direction
  and rearranging the index order, for example, ``cu_gf_driver.F90`` in the ``ccpp-physics/physics``
  directory.

Schemes in the CCPP are classified into two categories: primary schemes and interstitial schemes.
Primary schemes are the major parameterizations, such as PBL, microphysics, convection, radiation,
surface layer parameterizations, etc. Interstitial schemes are modularized pieces of code that
perform data preparation, diagnostics, or other “glue” functions and allow primary schemes to work
together as a suite. They can be categorized as “scheme-specific” or “suite-level”. Scheme-specific
interstitial schemes augment a specific primary scheme (to provide additional functionality).
Suite-level interstitial schemes provide additional functionality on top of a class of primary schemes,
connect two or more schemes together, or provide code for conversions, initializing sums, or applying
tendencies, for example. The rules and guidelines provided in the following sections apply both to
primary and interstitial schemes.

General Rules
=============
A CCPP-compliant scheme is in the form of Fortran modules. :ref:`Listing 1 <scheme_template>` contains
the template for a CCPP-compliant scheme (``ccpp/framework/doc/DevelopersGuide/scheme_template.F90``),
which includes four essential components: argument metadata tables, the *_init*, *_run*, and *_finalize*
subroutines.

.. _scheme_template:

.. literalinclude:: ../../DevelopersGuide/scheme_template.F90
   :language: fortran
   :lines: 78-125

*Listing 2.1: Fortran template for a CCPP-compliant scheme showing an argument table and
the _init, _run, and _finalize subroutines.*

More details are found below:

* Each scheme must be in its own module and must include three (*_init*, *_run*, and *_finalize*)
  subroutines (entry points). The module name and the subroutine names must be consistent with the
  scheme name. The *_init* and *_finalize* subroutines are run automatically when the CCPP-Physics
  are initialized and finalized, respectively. These two subroutines may be called more than once,
  depending on the host model’s parallelization strategy, and as such must be idempotent (the answer
  must be the same when the subroutine is called multiple times). The _run subroutine contains the
  code to execute the scheme.

* Each non-empty CCPP entrypoint subroutine requires a commented argument table 
  (:ref:`Listing 1 <scheme_template>`).  Empty subroutines do not require an argument table
  (e.g., `scheme_template_init` in :ref:`Listing 1 <scheme_template>`), since no variables need to be passed.

* The argument table contains the metadata of the variables required by the scheme. The table must
  precede the entry point subroutine (*_init*, *_run*, and *_finalize*) and must start with
  ``!> \section arg_table_subroutine_name Argument Table`` and end with a line containing only ``!!``

* The current metadata attributes of a variable include ``local_name``, ``standard_name``, ``long_name``,
  ``units``, ``rank``, ``type``, ``kind``, ``intent``, and ``optional`` (see more in section
  :ref:`DoxygenModules` of this chapter). 

* If the width of an argument table exceeds 250 characters, the table should be wrapped in C preprocessor directives:

.. code-block:: fortran

   #if 0
   !> \section arg_table_scheme_template_run Argument Table
   !> ...
   !!
   #endif

* For better readability, the columns in the argument table are aligned.

* All external information required by the scheme must be passed in via the argument list. Statements
  such as  ``‘use EXTERNAL_MODULE’`` should not be used for passing in data and all physical constants
  should go through the argument list.

* Note that standard names, variable names, module names, scheme names and subroutine names are all case sensitive.

* Interstitial modules (``scheme_pre`` and ``scheme_post``) can be included if any part of the physics
  scheme must be executed before (``_pre``) or after (``_post``) the ``module scheme`` defined above.
  These situations are described in more detail in Section 5.1.


Input/output Variable (argument) Rules
======================================

* Variables available for CCPP physics schemes are identified by their unique
  ``standard_name``. While an effort is made to comply with existing ``standard_name``
  definitions of the Climate and Forecast (CF) conventions (http://cfconventions.org), additional names
  are used in the CCPP (see below for further information).

* A list of available standard names and an example of naming conventions can be found in
  ``ccpp/framework/doc/DevelopersGuide/CCPP_VARIABLES_${HOST}.pdf``, where ``${HOST}`` is the 
  name of the host model.  Running the CCPP *prebuild* script (described in Chapter 3)
  will generate a LaTeX source file that can be compiled to produce
  a PDF file with all variables defined by the host model and requested by the physics schemes.

* A ``standard_name`` cannot be assigned to more than one local variable (``local_name``).
  The ``local_name`` of a variable can be chosen freely and does not have to match the
  ``local_name`` in the host model.

* All variable information (units, rank, index ordering) must match the specifications on
  the host model side, but sub-slices can be used/added in the host model. For example, when
  using the UFS Atmosphere as the host model, tendencies are split in ``GFS_typedefs.F90``
  so they can be used in the necessary physics scheme:

.. code-block:: fortran

   !! | IPD_Data(nb)%Intdiag%dt3dt(:,:,1) | cumulative_change_in_temperature_due_to_longwave_radiation
   !! | IPD_Data(nb)%Intdiag%dt3dt(:,:,2) | cumulative_change_in_temperature_due_to_shortwave_radiation_and_orographic_gravity_wave_drag
   !! | IPD_Data(nb)%Intdiag%dt3dt(:,:,3) | cumulative_change_in_temperature_due_to_PBL

* The two mandatory variables that any scheme-related subroutine must accept as ``intent(out)`` arguments are
  ``errmsg`` and ``errflg`` (see also coding rules in Section :ref:`CodingRules`).

* At present, only two types of variable definitions are supported by the CCPP-framework:

   * Standard Intrinsic Fortran variables are preferred (``character``, ``integer``, ``logical``, ``real``).
     For character variables, the length should be specified as ``*`` in order to allow the host model
     to specify the corresponding variable with a length of its own choice. All others can have a
     ``kind`` attribute of a ``kind`` type defined by the host model.
   * Derived data types (DDTs). While the use of DDTs is discouraged, some use cases may
     justify their application (e.g. DDTs for chemistry that contain tracer arrays or information on
     whether tracers are advected). It should be understood that use of DDTs within schemes
     forces their use in host models and potentially limits a scheme’s portability. Where possible,
     DDTs should be broken into components that could be usable for another scheme of the same type.

* It is preferable to have separate variables for physically-distinct quantities. For example,
  an array containing various cloud properties should be split into its individual 
  physically-distinct components to facilitate generality. An exception to this rule is if
  there is a need to perform the same operation on an array of otherwise physically-distinct
  variables. For example, tracers that undergo vertical diffusion can be combined into one array
  where necessary. This tactic should be avoided wherever possible, and is not acceptable merely
  as a convenience.

* If a scheme is to make use of CCPP’s subcycling capability, the loop counter can be obtained
  from CCPP as an ``intent(in)`` variable (see Listings 3.1 and 3.2 for a mandatory list of variables
  that are provided by the CCPP-Framework and/or the host model for this and other purposes). 

.. _CodingRules:

Coding Rules
============

* Code must comply to modern Fortran standards (Fortran 90/95/2003).

* Labeled ``end`` statements should be used for modules, subroutines and functions,
  for example, ``module scheme_template → end module scheme_template``.

* Implicit variable declarations are not allowed. The ``implicit none`` statement is mandatory and
  is preferable at the module-level so that it applies to all the subroutines in the module.

* All ``intent(out)`` variables must be set inside the subroutine, including the mandatory
  variables ``errflg`` and ``errmsg``.

* Decomposition-dependent host model data inside the module cannot be permanent,
  i.e. variables that contain domain-dependent data cannot be kept using the ``save`` attribute.

* ``goto`` statements are not alowed.

* ``common`` blocks are not allowed.

* Errors are handled by the host model using the two mandatory arguments ``errmsg`` and
  ``errflg``. In the event of an error, a meaningful error message should be assigned to ``errmsg``
  and set ``errflg`` to a value other than 0, for example:

.. code-block:: bash

   write (errmsg, ‘(*(a))’) ‘Logic error in scheme xyz: …’
   errflg = 1
   return

* Schemes are not allowed to abort/stop the program.

* Schemes are not allowed to perform I/O operations except for reading lookup tables
  or other information needed to initialize the scheme, including stdout and stderr.
  Diagnostic messages are tolerated, but should be minimal.

* Line lengths of up to 120 characters are suggested for better readability (exception: CCPP
  metadata argument tables).

Additional coding rules are listed under the *Coding Standards* section of the NOAA NGGPS
Overarching System team document on Code, Data, and Documentation Management for NOAA Environmental
Modeling System (NEMS) Modeling Applications and Suites (available at
https://docs.google.com/document/u/1/d/1bjnyJpJ7T3XeW3zCnhRLTL5a3m4_3XIAUeThUPWD9Tg/edit#heading=h.97v79689onyd). 

Parallel Programming Rules
==========================

Most often shared memory (OpenMP: Open Multi-Processing) and MPI (Message Passing Interface)
communication are done outside the physics in which case the physics looping and arrays already
take into account the sizes of the threaded tasks through their input indices and array
dimensions.  The following rules should be observed when including OpenMP or MPI communication
in a physics scheme:

* Shared-memory (OpenMP) parallelization inside a scheme is allowed with the restriction
  that the number of OpenMP threads to use is obtained from the host model as an ``intent(in)``
  argument in the argument list (Listings 3.1 and 3.2).

* MPI communication is allowed in the ``_init`` and ``_finalize`` phase for the purpose
  of computing, reading or writing scheme-specific data that is independent of the host
  model’s data decomposition. An example is the initial read of a lookup table of aerosol
  properties by one or more MPI processes, and its subsequent broadcast to all processes.
  Several restrictions apply:

   * The implementation of reading and writing of data must be scalable to perform
     efficiently from a few to millions of tasks.
   * The MPI communicator must be provided by the host model as an ``intent(in)``
     argument in the argument list (Listings 3.1 and 3.2).
   * The use of MPI_COMM_WORLD is not allowed.

* Calls to MPI and OpenMP functions, and the import of the MPI and OpenMP libraries,
  must be guarded by C preprocessor directives as illustrated in the following listing.
  OpenMP pragmas can be inserted without C preprocessor guards, since they are ignored
  by the compiler if the OpenMP compiler flag is omitted.

.. code-block:: fortran

   #ifdef MPI
     use mpi
   #endif
   #ifdef OPENMP
     use omp_lib
   #endif
   ...
   #ifdef MPI
     call MPI_BARRIER(mpicomm, ierr)
   #endif

   #ifdef OPENMP
     me = OMP_GET_THREAD_NUM()
   #else
     me = 0
   #endif

* For Fortran coarrays, consult with the GMTB helpdesk (gmtb-help@ucar.edu).

.. include:: ScientificDocRules.inc

.. Bibliography should go at the end of the last chapter

.. bibliography:: references.bib
