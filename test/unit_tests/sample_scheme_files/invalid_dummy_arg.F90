! Test parameterization with no vertical level
!

module invalid_dummy_arg

  use ccpp_kinds, only: kind_phys

  implicit none
  private

  public :: invalid_dummy_arg_run

contains

  !> \section arg_table_invalid_dummy_arg_run  Argument Table
  !! \htmlinclude arg_table_invalid_dummy_arg_run.html
  !!
  subroutine invalid_dummy_arg_run(foo, timestep, temp_prev, temp_layer, qv, ps, &
      errmsg, errflg)

    integer, intent(in) :: foo
    real(kind=kind_phys), intent(in) :: timestep
    real(kind=kind_phys), intent(inout) :: qv(:)
    real(kind=kind_phys), intent(inout) :: ps(:)
    real(kind=kind_phys), intent(in) :: woohoo(:)
    real(kind=kind_phys), intent(inout) :: temp_layer(foo)
    character(len=512), intent(out) :: errmsg
    integer, intent(out) :: errflg
    !----------------------------------------------------------------

    integer :: col_index

    errmsg = ''
    errflg = 0

    do col_index = 1, foo
      temp_layer(col_index) = temp_layer(col_index) + temp_prev(col_index)
      qv(col_index) = qv(col_index) + 1.0_kind_phys
    end do

  end subroutine invalid_dummy_arg_run

end module invalid_dummy_arg
