module setup_coeffs
  use ccpp_kinds, only: kind_phys
  implicit none

  public :: setup_coeffs_timestep_init

contains
  !> \section arg_table_setup_coeffs_timestep_init  Argument Table
  !! \htmlinclude arg_table_setup_coeffs_timestep_init.html
  !!
  subroutine setup_coeffs_timestep_init(coeffs, errmsg, errflg)

    real(kind_phys),    intent(inout) :: coeffs(:)
    character(len=512), intent(out)   :: errmsg
    integer,            intent(out)   :: errflg

    errmsg = ''
    errflg = 0

    coeffs(:) = 1._kind_phys

  end subroutine setup_coeffs_timestep_init

end module setup_coeffs
