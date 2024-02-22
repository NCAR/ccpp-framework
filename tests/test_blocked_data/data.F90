module data

!! \section arg_table_data Argument Table
!! \htmlinclude data.html
!!
    use ccpp_types,    only: ccpp_t

    implicit none

    private

    public nblks, blksz, ncols
    public ccpp_data_domain, ccpp_data_blocks, blocked_data_type, blocked_data_instance

    integer, parameter :: nblks = 4
    type(ccpp_t), target :: ccpp_data_domain
    type(ccpp_t), dimension(nblks), target :: ccpp_data_blocks

    integer, parameter, dimension(nblks) :: blksz = (/6,6,6,3/)
    integer, parameter :: ncols = sum(blksz)

!! \section arg_table_blocked_data_type
!! \htmlinclude blocked_data_type.html
!!
   type blocked_data_type
      integer, dimension(:), allocatable :: array_data
   contains
      procedure :: create  => blocked_data_create
   end type blocked_data_type

   type(blocked_data_type), dimension(nblks) :: blocked_data_instance

contains

   subroutine blocked_data_create(blocked_data_instance, ncol)
      class(blocked_data_type), intent(inout) :: blocked_data_instance
      integer, intent(in) :: ncol
      allocate(blocked_data_instance%array_data(ncol))
   end subroutine blocked_data_create

end module data
