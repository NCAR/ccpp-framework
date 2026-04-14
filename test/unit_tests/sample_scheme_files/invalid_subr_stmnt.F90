! Test parameterization with no vertical level
!

module invalid_subr_stmnt

  use ccpp_kinds, only: kind_phys

  implicit none
  private

  public :: invalid_subr_stmnt_init

contains

  !> \section arg_table_invalid_subr_stmnt_init  Argument Table
  !! \htmlinclude arg_table_invalid_subr_stmnt_init.html
  !!
  subroutine invalid_subr_stmnt_init(woohoo, errflg)

    character(len=512), intent(out) :: errmsg
    integer, intent(out) :: errflg

    ! This routine currently does nothing

    errmsg = ''
    errflg = 0

  end subroutine invalid_subr_stmnt_init

end module invalid_subr_stmnt
