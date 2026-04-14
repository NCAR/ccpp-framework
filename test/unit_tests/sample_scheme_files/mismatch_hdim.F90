! Test parameterization with no vertical level
!

module mismatch_hdim

  use ccpp_kinds, only: kind_phys

  implicit none
  private

  public :: mismatch_hdim_init
  public :: mismatch_hdim_run

contains

  !> \section arg_table_mismatch_hdim_run  Argument Table
  !! \htmlinclude arg_table_mismatch_hdim_run.html
  !!
  subroutine mismatch_hdim_run(tsfc, errmsg, errflg)

    real(kind=kind_phys), intent(inout) :: tsfc(:)
    character(len=512), intent(out) :: errmsg
    integer, intent(out) :: errflg

    errmsg = ''
    errflg = 0

    tsfc = tsfc - 1.0_kind_phys

  end subroutine mismatch_hdim_run

  !> \section arg_table_mismatch_hdim_init  Argument Table
  !! \htmlinclude arg_table_mismatch_hdim_init.html
  !!
  subroutine mismatch_hdim_init(tsfc, errmsg, errflg)

    real(kind=kind_phys), intent(inout) :: tsfc(:)
    character(len=512), intent(out) :: errmsg
    integer, intent(out) :: errflg

    tsfc = tsfc + 1.0_kind_phys

    errmsg = ''
    errflg = 0

  end subroutine mismatch_hdim_init

end module mismatch_hdim
