module data

!! \section arg_table_data Argument Table
!! \htmlinclude data.html
!!
    use ccpp_kinds, only : kind_phys
    use ccpp_types,    only: ccpp_t

    implicit none

    private

    public ncols, nspecies
    public cdata, data_array, opt_array_flag

    integer, parameter :: ncols = 4
    integer, parameter :: nspecies = 2
    type(ccpp_t), target :: cdata
    !real(kind_phys), dimension(1:ncols,1:nspecies), target :: data_array
    real(kind_phys), dimension(1:ncols,1:nspecies) :: data_array
    logical :: opt_array_flag

end module data
