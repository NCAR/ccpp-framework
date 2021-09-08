! Test parameterization with no vertical level
!

MODULE CCPPnotset_var_missing_in_meta

  USE ccpp_kinds, ONLY: kind_phys

  IMPLICIT NONE
  PRIVATE

  PUBLIC :: CCPPnotset_var_missing_in_meta_run

CONTAINS

  !> \section arg_table_CCPPnotset_var_missing_in_meta_run  Argument Table
  !! \htmlinclude arg_table_CCPPnotset_var_missing_in_meta_run.html
  !!
  subroutine CCPPnotset_var_missing_in_meta_run (foo, &
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

  end subroutine CCPPnotset_var_missing_in_meta_run

END MODULE CCPPnotset_var_missing_in_meta
