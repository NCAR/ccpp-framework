module data

!! \section arg_table_data Argument Table
!! \htmlinclude data.html
!!
    use ccpp_types,    only: ccpp_t

    implicit none

    private

    public nchunks, chunksize, chunk_begin, chunk_end, ncols
    public ccpp_data_domain, ccpp_data_chunks, chunked_data_type, chunked_data_instance

    integer, parameter :: nchunks = 4
    type(ccpp_t), target :: ccpp_data_domain
    type(ccpp_t), dimension(nchunks), target :: ccpp_data_chunks

    integer, parameter, dimension(nchunks) :: chunksize = (/6,6,6,3/)
    integer, parameter, dimension(nchunks) :: chunk_begin = (/1,7,13,19/)
    integer, parameter, dimension(nchunks) :: chunk_end = (/6,12,18,21/)
    integer, parameter :: ncols = sum(chunksize)

!! \section arg_table_chunked_data_type
!! \htmlinclude chunked_data_type.html
!!
   type chunked_data_type
      integer, dimension(:), allocatable :: array_data
   contains
      procedure :: create  => chunked_data_create
   end type chunked_data_type

   type(chunked_data_type) :: chunked_data_instance

contains

   subroutine chunked_data_create(chunked_data_instance, ncol)
      class(chunked_data_type), intent(inout) :: chunked_data_instance
      integer, intent(in) :: ncol
      allocate(chunked_data_instance%array_data(ncol))
   end subroutine chunked_data_create

end module data
