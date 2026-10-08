module suite_lifecycle

  implicit none
  private

  public :: suite_lifecycle_init, suite_lifecycle_final

contains

  !> \section arg_table_suite_lifecycle_init Argument Table
  !! \htmlinclude arg_table_suite_lifecycle_init.html
  !!
  subroutine suite_lifecycle_init(counter, errmsg, errflg)
    character(len=*), intent(out) :: errmsg
    integer, intent(out) :: errflg
    integer, intent(inout) :: counter
    errmsg = ''
    errflg = 0
    counter = counter + 1
  end subroutine suite_lifecycle_init

  !> \section arg_table_suite_lifecycle_final Argument Table
  !! \htmlinclude arg_table_suite_lifecycle_final.html
  !!
  subroutine suite_lifecycle_final(counter, errmsg, errflg)
    character(len=*), intent(out) :: errmsg
    integer, intent(out) :: errflg
    integer, intent(inout) :: counter
    errmsg = ''
    errflg = 0
    counter = counter + 1
  end subroutine suite_lifecycle_final

end module suite_lifecycle
