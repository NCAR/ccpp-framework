module data

!! \section arg_table_data Argument Table
!! \htmlinclude data.html
!!
    use ccpp_types,    only: ccpp_t

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

    public ncols
    public ccpp_data
    public ccpp_mpi_comm

    integer, parameter :: ncols = 1
    type(ccpp_t), target :: ccpp_data
    ccpp_mpi_comm_type :: ccpp_mpi_comm

end module data
