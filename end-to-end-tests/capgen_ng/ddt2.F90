module ddt2

  use ccpp_kinds, only: kind_phys

  implicit none

  type ty_ddt2
    integer :: foo
    real(kind=kind_phys) :: bar
  end type ty_ddt2

end module ddt2
