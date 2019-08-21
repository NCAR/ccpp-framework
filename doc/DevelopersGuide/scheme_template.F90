!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! CCPP-compliant physics scheme template
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! General rules:
!
! - scheme must be in its own module (module name = scheme name) and must
!   have three entry points (subroutines) starting with the name of the module:
!   module scheme_template -> subroutines scheme_template_{init,finalize,run}
! 
! - each .f or .F90 file with CCPP entry point scheme(s) must be accompanied by a 
!   .meta file containing metadata for the scheme(s) 
!
! - empty schemes (e.g., scheme_template_init below) do not need metadata
!
! - all external information required by the scheme must be passed in via the
!   argument list, i.e. NO 'use EXTERNAL_MODULE' statements
!
! Metadata rules:
!
! - refer to file scheme_template.meta for information about the metadata
!
! Input/output variable (argument) rules:
!
! - for a list of variables available for the specific host model, see files
!   doc/DevelopersGuide/CCPP_VARIABLES_XYZ.pdf, where XYZ is the name of the model
!
! - a standard_name cannot be assigned to more than one local variable (local_name)
!
! - all information (units, rank, index ordering) must match the specifications
!   on the host model side, but subslices can be used/added in the host model:
!   HOST MODEL: real, dimension(:,:,:,:) :: hydrometeors
!
!
! Coding rules:
!
! - code must comply to modern Fortran standards (Fortran 90/95/2003)
!
! - use labeled 'end' statements for modules, subroutines and functions
!   module scheme_template -> end module scheme_template
!
! - use implicit none
!
! - all intent(out) variables must be initialized properly inside the subroutine
!
! - NO permanent state inside the module, i.e. no variables carrying the 'save' attribute
!
! - NO 'goto' statements
!
! - errors are handled by the host model using the two mandatory arguments
!   errmsg and errflg; in the event of an error, assign a meaningful error
!   message to errmsg and set errflg to a value other than 0
!
! - schemes are NOT allowed to abort/stop the program
!
! - schemes are NOT allowed to perform I/O operations (except for reading
!   lookup tables / other information needed to initialize the scheme)
!
! - line lengths of 120 characters are suggested for better readibility
!   (exception: CCPP metadata argument tables)
!
! Parallel programming rules:
!
! - if OpenMP is used, the number of allowed threads must be provided by the
!   host model as an intent(in) argument in the argument list
!
! - if MPI is used, it is restricted to global communications: barrier, broadcast,
!   gather, scatter, reduction; the MPI communicator must be provided by the
!   host model as an intent(in) argument in the argument list
!   - do NOT use MPI_COMM_WORLD
!   - do NOT use any point-to-point communication
!
! - if Fortran coarrays are used, consult with the CCPP development team
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    module scheme_template

      contains

      subroutine scheme_template_init ()
      end subroutine scheme_template_init

      subroutine scheme_template_finalize()
      end subroutine scheme_template_finalize

      subroutine scheme_template_run (errmsg, errflg)

         implicit none

         !--- arguments
         ! add your arguments here
         character(len=*), intent(out)   :: errmsg
         integer,          intent(out)   :: errflg

         !--- local variables
         ! add your local variables here

         continue

         !--- initialize CCPP error handling variables
         errmsg = ''
         errflg = 0

         !--- initialize intent(out) variables
         ! initialize all intent(out) variables here

         !--- actual code
         ! add your code here

         ! in case of errors, set errflg to a value != 0,
         ! assign a meaningful message to errmsg and return

         return

      end subroutine scheme_template_run

    end module scheme_template
