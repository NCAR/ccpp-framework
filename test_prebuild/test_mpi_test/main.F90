program test_mpi

   use, intrinsic :: iso_fortran_env, only: error_unit

#if defined MPI_F08
   use mpi_f08
#elif defined MPI_F90
   use mpi
#endif

   use ccpp_types, only: ccpp_t
   use data, only: ncols
   use data, only: ccpp_data, ccpp_mpi_comm

   use ccpp_static_api, only: ccpp_physics_init, &
                              ccpp_physics_timestep_init, &
                              ccpp_physics_run, &
                              ccpp_physics_timestep_finalize, &
                              ccpp_physics_finalize

   implicit none

   character(len=*), parameter :: ccpp_suite = 'mpi_test_suite'
   integer :: ib, ierr, mpi_size, mpi_rank

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! MPI init step                                  !
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#if defined MPI_F08 || defined MPI_F90
   call mpi_init(ierr)
   ccpp_mpi_comm = mpi_comm_world
   call mpi_comm_size(mpi_comm_world, mpi_size, ierr)
   call mpi_comm_rank(mpi_comm_world, mpi_rank, ierr)
#else
   ccpp_mpi_comm = 0
   mpi_size = 1
   mpi_rank = 0
#endif

   write(error_unit,'(a,i0,a,i0)') "test_mpi: process: ", mpi_rank+1, " of ", mpi_size

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! CCPP init step                                 !
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   ! Block and thread number are not used; set to safe values
   ccpp_data%blk_no = 1
   ccpp_data%thrd_no = 1

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! CCPP physics init step                         !
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   call ccpp_physics_init(ccpp_data, suite_name=trim(ccpp_suite), ierr=ierr)
   if (ierr/=0) then
      write(error_unit,'(a)') "An error occurred in ccpp_physics_init:"
      write(error_unit,'(a)') trim(ccpp_data%errmsg)
      stop 1
   end if

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! CCPP physics timestep init step                !
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   call ccpp_physics_timestep_init(ccpp_data, suite_name=trim(ccpp_suite), ierr=ierr)
   if (ierr/=0) then
      write(error_unit,'(a)') "An error occurred in ccpp_physics_timestep_init:"
      write(error_unit,'(a)') trim(ccpp_data%errmsg)
      stop 1
   end if

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! CCPP physics run step                          !
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   call ccpp_physics_run(ccpp_data, suite_name=trim(ccpp_suite), ierr=ierr)
   if (ierr/=0) then
      write(error_unit,'(a)') "An error occurred in ccpp_physics_run:"
      write(error_unit,'(a)') trim(ccpp_data%errmsg)
      stop 1
   end if

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! CCPP physics timestep finalize step            !
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   call ccpp_physics_timestep_finalize(ccpp_data, suite_name=trim(ccpp_suite), ierr=ierr)
   if (ierr/=0) then
      write(error_unit,'(a)') "An error occurred in ccpp_physics_timestep_finalize:"
      write(error_unit,'(a)') trim(ccpp_data%errmsg)
      stop 1
   end if

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! CCPP physics finalize step                     !
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   call ccpp_physics_finalize(ccpp_data, suite_name=trim(ccpp_suite), ierr=ierr)
   if (ierr/=0) then
      write(error_unit,'(a)') "An error occurred in ccpp_physics_finalize:"
      write(error_unit,'(a)') trim(ccpp_data%errmsg)
      stop 1
   end if

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! MPI finalize step                              !
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#if defined MPI_F08 || defined MPI_F90
   call mpi_finalize(ierr)
#endif

contains

end program test_mpi