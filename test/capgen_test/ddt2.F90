module ddt2

   USE ccpp_kinds, ONLY: kind_phys

   implicit none

   type ty_ddt2
      integer :: foo
      real(kind_phys) :: bar
   end type ty_ddt2

end module ddt2
