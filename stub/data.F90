module data

!! \section arg_table_data Argument Table
!! \htmlinclude data.html
!!

    use ccpp_types,    only: ccpp_t

    implicit none

    private

    public ccpp_data

    type(ccpp_t), save, target :: ccpp_data

end module data
