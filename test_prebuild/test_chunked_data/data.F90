module data

!! \section arg_table_data Argument Table
!! \htmlinclude data.html
!!
    use ccpp_types,    only: ccpp_t

    implicit none

    private

    public nblks, blksz, ncols
    public ccpp_data_domain, ccpp_data_blocks, chunked_data_type, chunked_data_instance

    integer, parameter :: nblks = 4
    type(ccpp_t), target :: ccpp_data_domain
    type(ccpp_t), dimension(nblks), target :: ccpp_data_blocks

    integer, parameter, dimension(nblks) :: blksz = (/6,6,6,3/)
    integer, parameter :: ncols = sum(blksz)

!! \section arg_table_chunked_data_type
!! \htmlinclude chunked_data_type.html
!!
   type chunked_data_type
      integer, dimension(:), allocatable :: array_data
   contains
      procedure :: create  => chunked_data_create
   end type chunked_data_type

   type(chunked_data_type), dimension(nblks) :: chunked_data_instance

contains

   subroutine chunked_data_create(chunked_data_instance, ncol)
      class(chunked_data_type), intent(inout) :: chunked_data_instance
      integer, intent(in) :: ncol
      allocate(chunked_data_instance%array_data(ncol))
   end subroutine chunked_data_create

end module data
