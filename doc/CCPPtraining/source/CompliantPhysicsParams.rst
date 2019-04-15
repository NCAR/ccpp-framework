.. _CompliantPhysParams:

****************************************
CCPP-Compliant Physics Parameterizations
****************************************

A basic description of the rules for a parameterization to be considered CCPP-compliant
is summarized in this section (see also :cite:`HeinzellerEtAl2018`).

It should be noted that making a scheme CCPP-compliant is a necessary step for acceptance of the
scheme in the pool of supported CCPP physics schemes, but does not guarantee it. Acceptance is subject to 
approval by a Governance committee and depends on scientific innovation, demonstrated added
value, and compliance with the rules described below. The criteria for acceptance of innovations into the
CCPP is under development. For further information, please contact the GMTB helpdesk at gmtb-help@ucar.edu.

It is recommended that parameterizations be comprised of the smallest units that will be used.
For example, if a given set of deep and shallow convection schemes will always be called together
and in a pre-established order, it is acceptable to group them within a single scheme. However, if one
envisions that the deep and shallow convection schemes may someday operate independently, they should
be coded as two separate schemes to allow for more flexibility.  

Some schemes in the CCPP have been implemented using a driver as an entry point. In this context,
a driver is defined as a wrapper that sits on top of the actual scheme and provides the CCPP entry
points. In order to minimize the layers of code in the CCPP, the implementation of a driver is
discouraged, that is, it is preferable that the CCPP be composed of atomic parameterizations. One
example is the implementation of the Morrison-Gettelman microphysics, in which a simple entrypoint
leads to two versions of the scheme, MG2 and MG3.  A cleaner implementation would be to retire MG2
in favor of MG3, to put MG2 and MG3 as separate schemes, or to create a single scheme that can behave
as MG2 nd MG3 depending on namelist options.

However, there are some reasons that may justify the implementation of a driver:

* To preserve schemes that are also distributed outside of the CCPP. For example, the Thompson
  microphysics scheme is distributed both with the WRF model and with the CCPP. Having a driver
  with CCPP directives allows the Thompson scheme to remain intact so that it can be synchronized
  between the WRF and CCPP distributions.

* To deal with optional arguments.
 
* To perform unit conversions and array transformations, such as flip the vertical direction
  and rearrange index order. Note that, in the future, these capabilities will be included
  in the CCPP-Framework so that schemes do not have to perform these operations.

Somewhere here include info on schemes that have options, such as MG2 and MG3 are in the same
scheme.  MYNN has many options.  So does GF.  It is a grey area what is defined as a new scheme
versus options within a scheme.


General rules
=============
:ref:`Listing 1 <scheme_template>` contains a Fortran template for a CCPP-compliant scheme, which can also
be found in ``ccpp/framework/doc/DevelopersGuide/scheme_template.F90``.

.. _scheme_template:

.. literalinclude:: ../../DevelopersGuide/scheme_template.F90
   :language: fortran
   :lines: 78-125

* Each scheme must be in its own module (module name = scheme name) and must have three entry
  points (subroutines) starting with the name of the module:

.. code-block:: fortran

   module scheme
     implicit none
     private
     public :: scheme_init,scheme_run,scheme_finalize
     contains
       subroutine scheme_init() 
       end subroutine scheme_init
       subroutine scheme_finalize()
       end subroutine scheme_finalize
       subroutine scheme_run()
       end subroutine scheme_run
   end module scheme

The ``_init`` and ``_finalize`` routines are run automatically when the CCPP physics are initialized and
finalized, respectively.  These routines may be called more than once, depending on the host model’s
parallelization strategy, and as such must be idempotent (that is, the answer must be the same when
the subroutine is called multiple times).

* Additional modules (``scheme_pre`` and ``scheme_post``) can be used if there is any part of the physics
  scheme that must be executed before or after the ``module scheme`` defined above.  These situations are 
  described in more detail in Section 5.2 and 6.1.2. If additional modules are included, they also
  must have three entry points:

.. code-block:: fortran

   module scheme_pre
     implicit none
     private
     public :: scheme_pre_init, scheme_pre_run, &       
               scheme_pre_finalize
     contains
       subroutine scheme_pre_init() 
       end subroutine scheme_pre_init
       subroutine scheme_pre_finalize()
       end subroutine scheme_pre_finalize
       subroutine scheme_pre_run()
       end subroutine scheme_pre_run
   end module scheme_pre

   module scheme_post
     implicit none
     private
     public :: scheme_post_init, scheme_post_run, &
               scheme_post_finalize
     contains
       subroutine scheme_post_init() 
       end subroutine scheme_post_init
       subroutine scheme_post_finalize()
       end subroutine scheme_post_finalize
       subroutine scheme_post_run()
       end subroutine scheme_post_run
   end module scheme_post

* All CCPP entrypoint schemes need to be accompanied by a table that describes the arguments to
  the subroutine (see example in :ref:`Listing 1 <scheme_template>`).  However, empty schemes
  (e.g., ``scheme_template_init`` in :ref:`Listing 1 <scheme_template>`) are excepted and need
  no argument table.

* The order of arguments in the table does not need to be the same as in the argument list of
  the subroutine, but that is preferable.

* The argument table must precede the entry point subroutine, and must start with
  ``!> \section arg_table_subroutine_name Argument Table`` and end with a line containing only ``!!``

* All external information required by the scheme must be passed in via the argument list.
   * Statements such as ``use EXTERNAL_MODULE`` should not be used for passing in data and
     all physical constants should go through the argument list.

* If the width of an argument table exceeds 250 characters, the argument table should be wrapped.
  in C preprocessor directives:

.. code-block:: fortran

   #if 0
   !> \section arg_table_scheme_template_run Argument Table...
   !!
   #endif

* For better readability, it is suggested to align the columns in the metadata table.

* Note that module names, scheme names and subroutine names are case sensitive.

Input/output variable (argument) rules
======================================

* Variables available for CCPP physics schemes are identified by their unique
  ``standard_name``. While an effort is made to comply with existing ``standard_name``
  definitions of the CF conventions (http://cfconventions.org), additional names
  are used in the CCPP (see below for further information).

* A list of available standard names and an example of naming conventions can be found in
  ``ccpp/framework/doc/DevelopersGuide/CCPP_VARIABLES_${HOST}.pdf``, where ``${HOST}`` is the 
  name of the host model.  Running the CCPP prebuild script (described in Chapter 3)
  will generate a LaTeX source file that can be compiled to produce
  a PDF file with all variables defined by the host model and requested by the physics schemes.

* A ``standard_name`` cannot be assigned to more than one local variable (``local_name``).
  The ``local_name`` of a variable can be chosen freely and does not have to match the
  ``local_name`` in the host model.

* All variable information (units, rank, index ordering) must match the specifications on
  the host model side, but sub-slices can be used/added in the host model. For example, in
  ``GFS_typedefs.F90``, tendencies can be split so they can be used in the necessary physics scheme:

.. code-block:: fortran

   !! | IPD_Data(nb)%Intdiag%dt3dt(:,:,1) | cumulative_change_in_temperature_due_to_longwave_radiation
   !! | IPD_Data(nb)%Intdiag%dt3dt(:,:,2) | cumulative_change_in_temperature_due_to_shortwave_radiation_and_orographic_gravity_wave_drag
   !! | IPD_Data(nb)%Intdiag%dt3dt(:,:,3) | cumulative_change_in_temperature_due_to_PBL

* The two mandatory variables that every scheme must accept as ``intent(out)`` arguments are
  ``errmsg`` and ``errflg`` (see also coding rules).

* At present, only two types of variable definitions are supported by the CCPP framework:
   * Standard Intrinsic Fortran variables are preferred (``character``, ``integer``, ``logical``, ``real``).
     For character variables, the length should be specified as ``\∗``. All others can have a kind attribute
     of a kind type defined by the host model.
   * Derived data types (DDTs). While the use of DDTs is discouraged in general, some use cases may
     justify their application (e.g. DDTs for chemistry that contain tracer arrays or information on
     whether tracers are advected). It should be understood that use of DDTs within schemes
     forces their use in host models and potentially limits a scheme’s generality. Where possible,
     DDTs should be broken into components that could be usable for another scheme of the same type.
     Where DDTs currently exist within CCPP-compliant schemes, they are likely there due to expediency
     concerns and should eventually be phased out.

* It is preferable to have separate variables for physically-distinct quantities. For example,
  an array containing various cloud properties should be split into its individual 
  physically-distinct components to facilitate generality. An exception to this rule is if
  there is a need to perform the same operation on an array of otherwise physically-distinct
  variables. For example, tracers that undergo vertical diffusion can be combined into one array
  where necessary. This tactic should be avoided wherever possible, and is not acceptable merely
  as a convenience.

* If a scheme is to make use of CCPP’s subcycling capability in the suite definition
  file (SDF; see also GMTB Single Column Model Technical Guide v2.1, chapter 6.1.3,
  https://dtcenter.org/gmtb/users/ccpp/docs), the loop counter can be obtained from CCPP as
  an ``intent(in)`` variable (see Listings 3.1 and 3.2 for a mandatory list of variables
  that are provided by the CCPP framework and/or the host model for this and other purposes). 

Coding rules
============

* Code must comply to modern Fortran standards (Fortran 90/95/2003).

* Labeled ``end`` statements should be used for modules, subroutines and functions, for example:
   * ``$module scheme_template → end module scheme_template``

* ``implicit none`` is not allowed.

* All ``intent(out)`` variables must be set inside the subroutine, including the mandatory
  variables ``errflg`` and ``errmsg``.

* Decomposition-dependent host model data inside the module cannot be permanent,
  i.e. variables that contain domain-dependent data cannot be kept using the ``save`` attribute.

* ``goto`` statements are not alowed.

* ``common`` blocks are not allowed.

* Errors are handled by the host model using the two mandatory arguments ``errmsg`` and
  ``errflg``. In the event of an error, a meaningful error message should be assigned to ``errmsg``
  and set errflg to a value other than 0, for example:

.. code-block:: bash

   write (errmsg, ‘(*(a))’) ‘Logic error in scheme xyz: …’
   errflg = 1
   return

* Schemes are not allowed to abort/stop the program.

* Schemes are not allowed to perform I/O operations (except for reading lookup tables
  or other information needed to initialize the scheme), including stdout/stderr.

* Line lengths of up to 120 characters are suggested for better readability (exception: CCPP
  metadata argument tables).

Additional coding rules are listed under the *Coding Standards* section of the NOAA NGGPS
Overarching System team document on Code, Data, and Documentation Management for NEMS 
Modeling Applications and Suites (available at
https://docs.google.com/document/u/1/d/1bjnyJpJ7T3XeW3zCnhRLTL5a3m4_3XIAUeThUPWD9Tg/edit#heading=h.97v79689onyd). 

Parallel programming rules
==========================

Most often shared memory (OpenMP) and MPI communication are done outside the physics in
which case the physics looping and arrays already take into account the sizes of the
threaded tasks through their input indices and array dimensions.  The following rules
should be observed when including OpenMP or MPI communication in a physics scheme:

* Shared-memory (OpenMP) parallelization inside a scheme is allowed with the restriction
  that the number of OpenMP threads to use is obtained from the host model as in ``intent(in)``
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
   * The use of MPI is restricted to global communications: ``barrier``, ``broadcast``, 
     ``gather``, ``scatter``, ``reduce``.
   * The use of MPI_COMM_WORLD is not allowed.
   * The use of point-to-point communication is not allowed.

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

.. This should go at the end of the last chapter

.. bibliography:: references.bib
