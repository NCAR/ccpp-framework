module data

!! \section arg_table_data Argument Table
!! \htmlinclude data.html
!!
   use ccpp_types, only: ccpp_t
   use ccpp_kinds, only: kind_phys

   implicit none

   private

   public cdata, nx, flag_for_opt_arg, std_arg, opt_arg, opt_arg_2

   type(ccpp_t), target               :: cdata
   integer, parameter                 :: nx = 3
   logical                            :: flag_for_opt_arg

   integer,              dimension(nx)             :: std_arg
   integer,              dimension(:), allocatable :: opt_arg
   real(kind=kind_phys), dimension(:), allocatable :: opt_arg_2

end module data
