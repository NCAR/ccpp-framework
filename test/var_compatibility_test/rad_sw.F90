module rad_sw
  use ccpp_kinds, only: kind_phys

  implicit none
  private

  public :: rad_sw_run

contains

  !> \section arg_table_rad_sw_run  Argument Table
  !! \htmlinclude arg_table_rad_sw_run.html
  !!
  subroutine rad_sw_run(ncol, sfc_up_sw, sfc_down_sw, errmsg, errflg)

    integer,            intent(in)    :: ncol
    real(kind_phys),    intent(inout) :: sfc_up_sw(:)
    real(kind_phys),    intent(inout) :: sfc_down_sw(:)
    character(len=512), intent(out)   :: errmsg
    integer,            intent(out)   :: errflg

    ! Locals
    integer :: icol

    errmsg = ''
    errflg = 0

    do icol=1,ncol
       sfc_up_sw(icol)   = 100._kind_phys
       sfc_down_sw(icol) = 400._kind_phys
    enddo

  end subroutine rad_sw_run

end module rad_sw
