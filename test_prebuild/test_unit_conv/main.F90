program test_unit_conv

   use, intrinsic :: iso_fortran_env, only: error_unit

   use ccpp_types, only: ccpp_t
   use data, only: ncols, nspecies
   use data, only: cdata, data_array, opt_array_flag

   use ccpp_static_api, only: ccpp_physics_init, &
                              ccpp_physics_timestep_init, &
                              ccpp_physics_run, &
                              ccpp_physics_timestep_finalize, &
                              ccpp_physics_finalize

   implicit none

   character(len=*), parameter :: ccpp_suite = 'unit_conv_suite'
   integer :: ierr
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! CCPP init step                                 !
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   ! For physics running over the entire domain,
   ! ccpp_thread_number and ccpp_chunk_number are
   ! set to 1, indicating that arrays are to be sent
   ! following their dimension specification in the
   ! metadata (must match horizontal_dimension).
   cdata%thrd_no  = 1
   cdata%chunk_no = 1
   cdata%thrd_cnt = 1

   data_array = 1.0_8
   opt_array_flag = .true.

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! CCPP physics init step                         !
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   call ccpp_physics_init(cdata, suite_name=trim(ccpp_suite), ierr=ierr)
   if (ierr/=0) then
      write(error_unit,'(a)') "An error occurred in ccpp_physics_init:"
      write(error_unit,'(a)') trim(cdata%errmsg)
      stop 1
   end if

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! CCPP physics timestep init step                !
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   call ccpp_physics_timestep_init(cdata, suite_name=trim(ccpp_suite), ierr=ierr)
   if (ierr/=0) then
      write(error_unit,'(a)') "An error occurred in ccpp_physics_timestep_init:"
      write(error_unit,'(a)') trim(cdata%errmsg)
      stop 1
   end if

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! CCPP physics run step                          !
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   call ccpp_physics_run(cdata, suite_name=trim(ccpp_suite), ierr=ierr)
   if (ierr/=0) then
      write(error_unit,'(a)') "An error occurred in ccpp_physics_run:"
      write(error_unit,'(a)') trim(cdata%errmsg)
      stop 1
   end if

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! CCPP physics timestep finalize step            !
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   call ccpp_physics_timestep_finalize(cdata, suite_name=trim(ccpp_suite), ierr=ierr)
   if (ierr/=0) then
      write(error_unit,'(a)') "An error occurred in ccpp_physics_timestep_finalize:"
      write(error_unit,'(a)') trim(cdata%errmsg)
      stop 1
   end if

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! CCPP physics finalize step                     !
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   call ccpp_physics_finalize(cdata, suite_name=trim(ccpp_suite), ierr=ierr)
   if (ierr/=0) then
      write(error_unit,'(a)') "An error occurred in ccpp_physics_timestep_init:"
      write(error_unit,'(a)') trim(cdata%errmsg)
      stop 1
   end if

contains

end program test_unit_conv
