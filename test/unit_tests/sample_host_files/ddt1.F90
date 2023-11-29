module ddt1

   use ccpp_kinds, only: kind_phys

   private
   implicit none

   !! \section arg_table_ddt1_t
   !! \htmlinclude ddt1_t.html
   !!
   type, public :: ddt1_t
      integer,                 public  :: num_vars = 0
      real(kind_phys), allocatable     :: vars(:,:,:)

   end type ddt1_t

end module ddt1
