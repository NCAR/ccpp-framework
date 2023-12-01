program test_blocked_data

   use ccpp_types, only: ccpp_t
   use data, only: nblks, blksz, ncols
   use data, only: ccpp_data_domain, ccpp_data_blocks, &
                   blocked_data_type, blocked_data_instance

   use ccpp_static_api, only: ccpp_physics_init, &
                              ccpp_physics_timestep_init, &
                              ccpp_physics_run, &
                              ccpp_physics_timestep_finalize, &
                              ccpp_physics_finalize

   implicit none

   character(len=*), parameter :: ccpp_suite = 'blocked_data_suite'
   integer :: ib, ierr
   type(ccpp_t), pointer :: cdata => null()

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! CCPP init step                                 !
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   ! For physics running over the entire domain, block and thread
   ! number are not used; set to safe values
   ccpp_data_domain%blk_no = 1
   ccpp_data_domain%thrd_no = 1

   ! Loop over all blocks and threads for ccpp_data_blocks
   do ib=1,nblks
      ! Assign the correct block numbers, only one thread
      ccpp_data_blocks(ib)%blk_no = ib
      ccpp_data_blocks(ib)%thrd_no = 1
   end do

   do ib=1,size(blocked_data_instance)
      allocate(blocked_data_instance(ib)%array_data(blksz(ib)))
      write(0,'(2(a,i3))') "Allocated array_data for block", ib, " to size", size(blocked_data_instance(ib)%array_data)
   end do

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! CCPP physics init step                         !
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   cdata => ccpp_data_domain
   call ccpp_physics_init(cdata, suite_name=trim(ccpp_suite), ierr=ierr)
   if (ierr/=0) then
      write(0,'(a)') "An error occurred in ccpp_physics_init:"
      write(0,'(a)') trim(cdata%errmsg)
      stop 1
   end if

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! CCPP physics timestep init step                !
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   cdata => ccpp_data_domain
   call ccpp_physics_timestep_init(cdata, suite_name=trim(ccpp_suite), ierr=ierr)
   if (ierr/=0) then
      write(0,'(a)') "An error occurred in ccpp_physics_timestep_init:"
      write(0,'(a)') trim(cdata%errmsg)
      stop 1
   end if

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! CCPP physics run step                          !
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   do ib=1,nblks
      cdata => ccpp_data_blocks(ib)
      call ccpp_physics_run(cdata, suite_name=trim(ccpp_suite), ierr=ierr)
      if (ierr/=0) then
         write(0,'(a,i3,a)') "An error occurred in ccpp_physics_run for block", ib, ":"
         write(0,'(a)') trim(cdata%errmsg)
         stop 1
      end if
   end do

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! CCPP physics timestep finalize step            !
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   cdata => ccpp_data_domain
   call ccpp_physics_timestep_finalize(cdata, suite_name=trim(ccpp_suite), ierr=ierr)
   if (ierr/=0) then
      write(0,'(a)') "An error occurred in ccpp_physics_timestep_init:"
      write(0,'(a)') trim(cdata%errmsg)
      stop 1
   end if

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! CCPP physics finalize step                     !
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   cdata => ccpp_data_domain
   call ccpp_physics_finalize(cdata, suite_name=trim(ccpp_suite), ierr=ierr)
   if (ierr/=0) then
      write(0,'(a)') "An error occurred in ccpp_physics_timestep_init:"
      write(0,'(a)') trim(cdata%errmsg)
      stop 1
   end if

contains

end program test_blocked_data