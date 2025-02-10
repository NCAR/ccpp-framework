module rad_sw
  use ccpp_kinds, only: kind_phys
  use mod_rad_ddt, only: ty_rad_sw

  implicit none
  private

  public :: rad_sw_run

contains

  !> \section arg_table_rad_sw_run  Argument Table
  !! \htmlinclude arg_table_rad_sw_run.html
  !!
  subroutine rad_sw_run(ncol, fluxSW, errmsg, errflg)

    integer,            intent(in)    :: ncol
    type(ty_rad_sw),    intent(inout) :: fluxSW(:)
    character(len=512), intent(out)   :: errmsg
    integer,            intent(out)   :: errflg

    ! Locals
    integer :: icol

    errmsg = ''
    errflg = 0

    do icol=1,ncol
       fluxSW(icol)%sfc_up_sw   = 100._kind_phys
       fluxSW(icol)%sfc_down_sw = 400._kind_phys
    enddo

  end subroutine rad_sw_run

end module rad_sw
