! Test parameterization with no vertical level
!

MODULE invalid_subr_stmnt

  USE ccpp_kinds, ONLY: kind_phys

  IMPLICIT NONE
  PRIVATE

  PUBLIC :: invalid_subr_stmnt_init

CONTAINS

  !> \section arg_table_invalid_subr_stmnt_init  Argument Table
  !! \htmlinclude arg_table_invalid_subr_stmnt_init.html
  !!
  subroutine invalid_subr_stmnt_init (woohoo, errflg)

    character(len=512),      intent(out)   :: errmsg
    integer,                 intent(out)   :: errflg

    ! This routine currently does nothing

    errmsg = ''
    errflg = 0

  end subroutine invalid_subr_stmnt_init

END MODULE invalid_subr_stmnt
