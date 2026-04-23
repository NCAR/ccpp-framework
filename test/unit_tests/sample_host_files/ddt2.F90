module ddt2

   use ccpp_kinds, only: kind_phys

   private
   implicit none

   !! \section arg_table_ddt1_t
   !! \htmlinclude ddt1_t.html
   !!
   type, public :: ddt1_t
      real, pointer :: undocumented_array(:) => NULL()
   end type ddt1_t

   !! \section arg_table_ddt2_t
   !! \htmlinclude ddt2_t.html
   !!
   type, public :: ddt2_t
      integer,                 public  :: num_vars = 0
      real(kind_phys), allocatable     :: vars(:,:,:)

   end type ddt2_t

end module ddt2
