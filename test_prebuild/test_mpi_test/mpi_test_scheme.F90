!>\file mpi_test_scheme.F90
!! This file contains a mpi_test_scheme CCPP scheme that does nothing
!! except requesting the minimum, mandatory variables for testing.

module mpi_test_scheme

   use, intrinsic :: iso_fortran_env, only: error_unit

#if defined MPI_F08
#define ccpp_mpi_comm_type type(mpi_comm)
#else
#define ccpp_mpi_comm_type integer
#endif

#if defined MPI_F08
    use mpi_f08
#elif defined MPI_F90
    use mpi
#endif

   implicit none

   private
   public :: mpi_test_scheme_init

   contains

!! \section arg_table_mpi_test_scheme_init Argument Table
!! \htmlinclude mpi_test_scheme_init.html
!!
   subroutine mpi_test_scheme_init(ncols, ccpp_mpi_comm, errmsg, errflg)
      ! Interface variables
      integer, intent(in)               :: ncols
      ccpp_mpi_comm_type, intent(in)    :: ccpp_mpi_comm
      character(len=*), intent(out)     :: errmsg
      integer,          intent(out)     :: errflg
      ! Local variables
      integer :: mpi_rank, mpi_test, ierr

      ! Initialize CCPP error handling variables
      errmsg = ''
      errflg = 0

      ! The MPI_F08 and MPI_F90 blocks are identical except the debug output
#if defined MPI_F08
      call mpi_comm_rank(ccpp_mpi_comm, mpi_rank, ierr)
      mpi_test = mpi_rank
      call mpi_bcast(mpi_test, 1, MPI_INTEGER, 0, ccpp_mpi_comm, ierr)
      write(error_unit,'(a,i3)') 'In mpi_test_scheme_init, MPI_F08 branch: MPI root has rank', mpi_test
#elif defined MPI_F90
      call mpi_comm_rank(ccpp_mpi_comm, mpi_rank, ierr)
      mpi_test = mpi_rank
      call mpi_bcast(mpi_test, 1, MPI_INTEGER, 0, ccpp_mpi_comm, ierr)
      write(error_unit,'(a,i3)') 'In mpi_test_scheme_init, MPI_F90 branch: MPI root has rank', mpi_test
#else
      write(error_unit,'(a,i3)') 'In mpi_test_scheme_init, NO MPI branch'
#endif

   end subroutine mpi_test_scheme_init

end module mpi_test_scheme
