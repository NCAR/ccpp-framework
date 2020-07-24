! Test parameterization with no vertical level
!

MODULE preproc_defs_test1

  USE ccpp_kinds, ONLY: kind_phys

  IMPLICIT NONE
  PRIVATE

  PUBLIC :: preproc_defs_test1_run

CONTAINS

  !> \section arg_table_preproc_defs_test1_run  Argument Table
  !! \htmlinclude arg_table_preproc_defs_test1_run.html
  !!
  subroutine preproc_defs_test1_run (foo, &
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

  end subroutine preproc_defs_test1_run

END MODULE preproc_defs_test1
