program test_opt_arg

   use, intrinsic :: iso_fortran_env, only: output_unit, error_unit

   use ccpp_types, only: ccpp_t
   use data, only: cdata, nx, flag_for_opt_arg, std_arg, opt_arg, opt_arg_2

   use ccpp_static_api, only: ccpp_physics_init, &
                              ccpp_physics_timestep_init, &
                              ccpp_physics_run, &
                              ccpp_physics_timestep_finalize, &
                              ccpp_physics_finalize

   implicit none

   character(len=*), parameter :: ccpp_suite = 'opt_arg_suite'
   integer :: ierr

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! CCPP init step                                 !
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   cdata%blk_no = 1
   cdata%thrd_no = 1

   std_arg = 1
   flag_for_opt_arg = .true.
   allocate(opt_arg(nx))
   allocate(opt_arg_2(nx))

   ! std_arg must all be 1, opt_arg must all be 0
   write(output_unit,'(a)') "After ccpp_init:                   check std_arg(:)==1, opt_arg(:)==0, opt_arg_2(:)==0"
   if (.not. all(std_arg   .eq. 1)) write(error_unit,'(a,3i3)') "Error after ccpp_init: std_arg=", std_arg
   if (.not. all(opt_arg   .eq. 0)) write(error_unit,'(a,3i3)') "Error after ccpp_init: opt_arg=", opt_arg
   if (.not. all(opt_arg_2 .eq. 0)) write(error_unit,'(a,3i3)') "Error after ccpp_init: opt_arg_2=", opt_arg_2

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! CCPP physics init step                         !
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   call ccpp_physics_init(cdata, suite_name=trim(ccpp_suite), ierr=ierr)
   if (ierr/=0) then
      write(error_unit,'(a)') "An error occurred in ccpp_physics_init:"
      write(error_unit,'(a)') trim(cdata%errmsg)
      stop 1
   end if

   ! std_arg must all be 1, opt_arg must all be 0
   write(output_unit,'(a)') "After ccpp_physics_init:           check std_arg(:)==1 and opt_arg(:)==0"
   if (.not. all(std_arg .eq. 1)) write(error_unit,'(a,3i3)') "Error after ccpp_physics_init: std_arg=", std_arg
   if (.not. all(opt_arg .eq. 0)) write(error_unit,'(a,3i3)') "Error after ccpp_physics_init: opt_arg=", opt_arg

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! CCPP physics timestep init step                !
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   call ccpp_physics_timestep_init(cdata, suite_name=trim(ccpp_suite), ierr=ierr)
   if (ierr/=0) then
      write(error_unit,'(a)') "An error occurred in ccpp_physics_timestep_init:"
      write(error_unit,'(a)') trim(cdata%errmsg)
      stop 1
   end if

   ! std_arg must all be 1, opt_arg must all be 2
   write(output_unit,'(a)') "After ccpp_physics_timestep_init:  check std_arg(:)==1 and opt_arg(:)==2"
   if (.not. all(std_arg .eq. 1)) write(error_unit,'(a,3i3)') "Error after ccpp_physics_timestep_init: std_arg=", std_arg
   if (.not. all(opt_arg .eq. 2)) write(error_unit,'(a,3i3)') "Error after ccpp_physics_timestep_init: opt_arg=", opt_arg

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! CCPP physics run step                          !
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   call ccpp_physics_run(cdata, suite_name=trim(ccpp_suite), ierr=ierr)
   if (ierr/=0) then
      write(error_unit,'(a)') "An error occurred in ccpp_physics_run:"
      write(error_unit,'(a)') trim(cdata%errmsg)
      stop 1
   end if

   ! std_arg must all be 1, opt_arg must all be 3
   write(output_unit,'(a)') "After ccpp_physics_run:            check std_arg(:)==1 and opt_arg(:)==3"
   if (.not. all(std_arg .eq. 1)) write(error_unit,'(a,3i3)') "Error after ccpp_physics_run: std_arg=", std_arg
   if (.not. all(opt_arg .eq. 3)) write(error_unit,'(a,3i3)') "Error after ccpp_physics_run: opt_arg=", opt_arg

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! CCPP physics timestep finalize step            !
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   deallocate(opt_arg)
   flag_for_opt_arg = .false.

   call ccpp_physics_timestep_finalize(cdata, suite_name=trim(ccpp_suite), ierr=ierr)
   if (ierr/=0) then
      write(error_unit,'(a)') "An error occurred in ccpp_physics_timestep_init:"
      write(error_unit,'(a)') trim(cdata%errmsg)
      stop 1
   end if

   ! std_arg must all be 7, opt_arg no longer allocated
   write(output_unit,'(a)') "After ccpp_physics_timestep_final: check std_arg(:)==7; opt_arg not allocated"
   if (.not. all(std_arg .eq. 7)) write(error_unit,'(a,3i3)') "Error after ccpp_physics_timestep_final: std_arg=", std_arg

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! CCPP physics finalize step                     !
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   call ccpp_physics_finalize(cdata, suite_name=trim(ccpp_suite), ierr=ierr)
   if (ierr/=0) then
      write(error_unit,'(a)') "An error occurred in ccpp_physics_timestep_init:"
      write(error_unit,'(a)') trim(cdata%errmsg)
      stop 1
   end if

   ! std_arg must all be 7, opt_arg no longer allocated
   write(output_unit,'(a)') "After ccpp_physics_timestep_final: check std_arg(:)==7; opt_arg not allocated"
   if (.not. all(std_arg .eq. 7)) write(error_unit,'(a,3i3)') "Error after ccpp_physics_timestep_final: std_arg=", std_arg

end program test_opt_arg
