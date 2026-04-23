! Test parameterization with no vertical level
!

MODULE mismatch_hdim

  USE ccpp_kinds, ONLY: kind_phys

  IMPLICIT NONE
  PRIVATE

  PUBLIC :: mismatch_hdim_init
  PUBLIC :: mismatch_hdim_run

CONTAINS

  !> \section arg_table_mismatch_hdim_run  Argument Table
  !! \htmlinclude arg_table_mismatch_hdim_run.html
  !!
  subroutine mismatch_hdim_run(tsfc, errmsg, errflg)

    real(kind_phys),    intent(inout) :: tsfc(:)
    character(len=512), intent(out)   :: errmsg
    integer,            intent(out)   :: errflg

    errmsg = ''
    errflg = 0

    tsfc = tsfc-1.0_kind_phys

  END SUBROUTINE mismatch_hdim_run

  !> \section arg_table_mismatch_hdim_init  Argument Table
  !! \htmlinclude arg_table_mismatch_hdim_init.html
  !!
  subroutine mismatch_hdim_init (tsfc, errmsg, errflg)

    real(kind_phys),    intent(inout) :: tsfc(:)
    character(len=512), intent(out)   :: errmsg
    integer,            intent(out)   :: errflg

    tsfc = tsfc+1.0_kind_phys

    errmsg = ''
    errflg = 0

  end subroutine mismatch_hdim_init

END MODULE mismatch_hdim
