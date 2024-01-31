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
      integer, intent(in)               :: ncols
      ccpp_mpi_comm_type, intent(in)    :: ccpp_mpi_comm
      character(len=*), intent(out)     :: errmsg
      integer,          intent(out)     :: errflg

      ! Initialize CCPP error handling variables
      errmsg = ''
      errflg = 0
#if defined MPI_F08
      write(error_unit,'(a,i3)') 'In mpi_test_scheme_init, MPI_F08 branch'
#elif defined MPI_F90
      write(error_unit,'(a,i3)') 'In mpi_test_scheme_init, MPI_F90 branch'
#else
      write(error_unit,'(a,i3)') 'In mpi_test_scheme_init, NO MPI branch'
#endif

   end subroutine mpi_test_scheme_init

end module mpi_test_scheme
