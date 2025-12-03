module rad_lw
  use ccpp_kinds, only: kind_phys
  use mod_rad_ddt, only: ty_rad_lw

  implicit none
  private

  public :: rad_lw_run

contains

  !> \section arg_table_rad_lw_run  Argument Table
  !! \htmlinclude arg_table_rad_lw_run.html
  !!
  subroutine rad_lw_run(ncol, fluxLW, errmsg, errflg)

    integer,            intent(in)    :: ncol
    type(ty_rad_lw),    intent(inout) :: fluxLW(:)
    character(len=512), intent(out)   :: errmsg
    integer,            intent(out)   :: errflg

    ! Locals
    integer :: icol

    errmsg = ''
    errflg = 0

    do icol=1,ncol
       fluxLW(icol)%sfc_up_lw   = 300._kind_phys
       fluxLW(icol)%sfc_down_lw = 50._kind_phys
    enddo

  end subroutine rad_lw_run

end module rad_lw
