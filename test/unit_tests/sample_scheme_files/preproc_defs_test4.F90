! Test parameterization with no vertical level
!

MODULE preproc_defs_test4

  USE ccpp_kinds, ONLY: kind_phys

  IMPLICIT NONE
  PRIVATE

  PUBLIC :: preproc_defs_test4_init

CONTAINS

  !> \section arg_table_preproc_defs_test4_init  Argument Table
  !! \htmlinclude arg_table_preproc_defs_test4_init.html
  !!
  subroutine preproc_defs_test4_init (foo, &
#if CCPP > 1
                                     bar, &
#endif
                                     errmsg, errflg)

    integer,            intent(in)    :: foo
#if CCPP > 1
    real(kind_phys),    intent(in)    :: bar
#endif
    character(len=512), intent(out)   :: errmsg
    integer,            intent(out)   :: errflg

    ! This routine currently does nothing

    errmsg = ''
    errflg = 0

  end subroutine preproc_defs_test4_init

END MODULE preproc_defs_test4
