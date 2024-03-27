!>\file opt_arg_scheme.F90
!! This file contains a opt_arg_scheme CCPP scheme that does nothing
!! except requesting the minimum, mandatory variables.

module opt_arg_scheme

   use, intrinsic :: iso_fortran_env, only: error_unit
   use ccpp_kinds, only: kind_phys

   implicit none

   private
   public :: opt_arg_scheme_timestep_init, &
             opt_arg_scheme_run, &
             opt_arg_scheme_timestep_finalize

   contains

!! \section arg_table_opt_arg_scheme_timestep_init Argument Table
!! \htmlinclude opt_arg_scheme_timestep_init.html
!!
   subroutine opt_arg_scheme_timestep_init(nx, var, opt_var, opt_var_2, errmsg, errflg)
      character(len=*),               intent(out) :: errmsg
      integer,                        intent(out) :: errflg
      integer,                        intent(in)  :: nx
      integer,                        intent(in)  :: var(:)
      integer,              optional, intent(out) :: opt_var(:)
      real(kind=kind_phys), optional, intent(out) :: opt_var_2(:)
      ! Initialize CCPP error handling variables
      errmsg = ''
      errflg = 0
      ! Initialize opt_var from var if opt_var if present
      if (present(opt_var)) then
         opt_var = 2*var
      end if
      ! Initialize opt_var_2 from var if opt_var_2 present
      if (present(opt_var_2)) then
         opt_var_2 = 3.0_kind_phys*var
      end if
   end subroutine opt_arg_scheme_timestep_init

!! \section arg_table_opt_arg_scheme_run Argument Table
!! \htmlinclude opt_arg_scheme_run.html
!!
   subroutine opt_arg_scheme_run(nx, var, opt_var, opt_var_2, errmsg, errflg)
      character(len=*),               intent(out)   :: errmsg
      integer,                        intent(out)   :: errflg
      integer,                        intent(in)    :: nx
      integer,                        intent(in)    :: var(:)
      integer,              optional, intent(inout) :: opt_var(:)
      real(kind=kind_phys), optional, intent(inout) :: opt_var_2(:)
      ! Initialize CCPP error handling variables
      errmsg = ''
      errflg = 0
      ! Update opt_var from var if opt_var present
      if (present(opt_var)) then
         opt_var = 3*var
      end if
      ! Update opt_var_2 from var if opt_var_2 present
      if (present(opt_var_2)) then
         opt_var_2 = 4.0_kind_phys*var
      end if
   end subroutine opt_arg_scheme_run

!! \section arg_table_opt_arg_scheme_timestep_finalize Argument Table
!! \htmlinclude opt_arg_scheme_timestep_finalize.html
!!
   subroutine opt_arg_scheme_timestep_finalize(nx, var, opt_var, opt_var_2, errmsg, errflg)
      character(len=*),               intent(out)   :: errmsg
      integer,                        intent(out)   :: errflg
      integer,                        intent(in)    :: nx
      integer,                        intent(inout) :: var(:)
      integer,              optional, intent(in)    :: opt_var(:)
      real(kind=kind_phys), optional, intent(inout) :: opt_var_2(:)
      ! Initialize CCPP error handling variables
      errmsg = ''
      errflg = 0
      ! Update var from opt_var if opt_var is present
      if (present(opt_var)) then
         var = 4*opt_var
      else
         var = 7*var
      end if
      ! Update opt_var_2 if present
      if (present(opt_var_2)) then
         opt_var_2 = opt_var_2 + 5.0_kind_phys
      end if
   end subroutine opt_arg_scheme_timestep_finalize

end module opt_arg_scheme
