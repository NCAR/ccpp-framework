! Test parameterization with no vertical level
!

MODULE CCPPeq1_var_in_fort_meta

  USE ccpp_kinds, ONLY: kind_phys

  IMPLICIT NONE
  PRIVATE

  PUBLIC :: CCPPeq1_var_in_fort_meta_run

CONTAINS

  !> \section arg_table_CCPPeq1_var_in_fort_meta_run  Argument Table
  !! \htmlinclude arg_table_CCPPeq1_var_in_fort_meta_run.html
  !!
  subroutine CCPPeq1_var_in_fort_meta_run (foo, &
#ifdef CCPP
                                     bar, &
#endif
                                     errmsg, errflg)

    integer,            intent(in)    :: foo
#ifdef CCPP
    real(kind_phys),    intent(in)    :: bar
#endif
    character(len=512), intent(out)   :: errmsg
    integer,            intent(out)   :: errflg

    ! This routine currently does nothing

    errmsg = ''
    errflg = 0

  end subroutine CCPPeq1_var_in_fort_meta_run

END MODULE CCPPeq1_var_in_fort_meta
