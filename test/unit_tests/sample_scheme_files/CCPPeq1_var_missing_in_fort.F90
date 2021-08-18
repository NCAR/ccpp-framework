! Test parameterization with no vertical level
!

MODULE CCPPeq1_var_missing_in_fort

  USE ccpp_kinds, ONLY: kind_phys

  IMPLICIT NONE
  PRIVATE

  PUBLIC :: CCPPeq1_var_missing_in_fort_run

CONTAINS

  !> \section arg_table_CCPPeq1_var_missing_in_fort_run  Argument Table
  !! \htmlinclude arg_table_CCPPeq1_var_missing_in_fort_run.html
  !!
  subroutine CCPPeq1_var_missing_in_fort_run (foo, &
#ifndef CCPP
                                     bar, &
#endif
                                     errmsg, errflg)

    integer,            intent(in)    :: foo
#ifndef CCPP
    real(kind_phys),    intent(in)    :: bar
#endif
    character(len=512), intent(out)   :: errmsg
    integer,            intent(out)   :: errflg

    ! This routine currently does nothing

    errmsg = ''
    errflg = 0

  end subroutine CCPPeq1_var_missing_in_fort_run

END MODULE CCPPeq1_var_missing_in_fort
