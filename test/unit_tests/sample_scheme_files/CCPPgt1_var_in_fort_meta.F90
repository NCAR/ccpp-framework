! Test parameterization with no vertical level
!

MODULE CCPPgt1_var_in_fort_meta

  USE ccpp_kinds, ONLY: kind_phys

  IMPLICIT NONE
  PRIVATE

  PUBLIC :: CCPPgt1_var_in_fort_meta_init

CONTAINS

  !> \section arg_table_CCPPgt1_var_in_fort_meta_init  Argument Table
  !! \htmlinclude arg_table_CCPPgt1_var_in_fort_meta_init.html
  !!
  subroutine CCPPgt1_var_in_fort_meta_init (foo, &
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

  end subroutine CCPPgt1_var_in_fort_meta_init

END MODULE CCPPgt1_var_in_fort_meta
