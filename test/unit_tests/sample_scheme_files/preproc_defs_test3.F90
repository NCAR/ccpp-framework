! Test parameterization with no vertical level
!

MODULE preproc_defs_test3

  USE ccpp_kinds, ONLY: kind_phys

  IMPLICIT NONE
  PRIVATE

  PUBLIC :: preproc_defs_test3_run

CONTAINS

  !> \section arg_table_preproc_defs_test3_run  Argument Table
  !! \htmlinclude arg_table_preproc_defs_test3_run.html
  !!
  subroutine preproc_defs_test3_run (foo, &
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

  end subroutine preproc_defs_test3_run

END MODULE preproc_defs_test3
