program test_chunked_data

   use, intrinsic :: iso_fortran_env, only: error_unit

   use ccpp_types, only: ccpp_t
   use data, only: nchunks, chunksize, ncols
   use data, only: ccpp_data_domain, ccpp_data_chunks, &
                   chunked_data_type, chunked_data_instance

   use ccpp_static_api, only: ccpp_physics_init, &
                              ccpp_physics_timestep_init, &
                              ccpp_physics_run, &
                              ccpp_physics_timestep_finalize, &
                              ccpp_physics_finalize

   implicit none

   character(len=*), parameter :: ccpp_suite = 'chunked_data_suite'
   integer :: ic, ierr
   type(ccpp_t), pointer :: cdata => null()

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! CCPP init step                                 !
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   ! For physics running over the entire domain,
   ! ccpp_thread_number and ccpp_chunk_number are
   ! set to 0, indicating that arrays are to be sent
   ! following their dimension specification in the
   ! metadata (must match horizontal_dimension).
   ccpp_data_domain%thrd_no = 0
   ccpp_data_domain%chunk_no = 0

   ! Loop over all blocks and threads for ccpp_data_chunks
   do ic=1,nchunks
      ! Assign the correct block numbers, only one thread
      ccpp_data_chunks(ic)%chunk_no = ic
      ccpp_data_chunks(ic)%thrd_no = 1
   end do

   call chunked_data_instance%create(ncols)
   write(error_unit,'(2(a,i3))') "Chunked_data_instance%array_data to size", size(chunked_data_instance%array_data)

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! CCPP physics init step                         !
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   cdata => ccpp_data_domain
   call ccpp_physics_init(cdata, suite_name=trim(ccpp_suite), ierr=ierr)
   if (ierr/=0) then
      write(error_unit,'(a)') "An error occurred in ccpp_physics_init:"
      write(error_unit,'(a)') trim(cdata%errmsg)
      stop 1
   end if

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! CCPP physics timestep init step                !
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   cdata => ccpp_data_domain
   call ccpp_physics_timestep_init(cdata, suite_name=trim(ccpp_suite), ierr=ierr)
   if (ierr/=0) then
      write(error_unit,'(a)') "An error occurred in ccpp_physics_timestep_init:"
      write(error_unit,'(a)') trim(cdata%errmsg)
      stop 1
   end if

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! CCPP physics run step                          !
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   do ic=1,nchunks
      cdata => ccpp_data_chunks(ic)
      call ccpp_physics_run(cdata, suite_name=trim(ccpp_suite), ierr=ierr)
      if (ierr/=0) then
         write(error_unit,'(a,i3,a)') "An error occurred in ccpp_physics_run for block", ic, ":"
         write(error_unit,'(a)') trim(cdata%errmsg)
         stop 1
      end if
   end do

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! CCPP physics timestep finalize step            !
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   cdata => ccpp_data_domain
   call ccpp_physics_timestep_finalize(cdata, suite_name=trim(ccpp_suite), ierr=ierr)
   if (ierr/=0) then
      write(error_unit,'(a)') "An error occurred in ccpp_physics_timestep_init:"
      write(error_unit,'(a)') trim(cdata%errmsg)
      stop 1
   end if

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! CCPP physics finalize step                     !
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   cdata => ccpp_data_domain
   call ccpp_physics_finalize(cdata, suite_name=trim(ccpp_suite), ierr=ierr)
   if (ierr/=0) then
      write(error_unit,'(a)') "An error occurred in ccpp_physics_timestep_init:"
      write(error_unit,'(a)') trim(cdata%errmsg)
      stop 1
   end if

contains

end program test_chunked_data