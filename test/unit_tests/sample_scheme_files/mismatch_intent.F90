! Test parameterization with no vertical level
!

module mismatch_intent

  use ccpp_kinds, only: kind_phys

  implicit none
  private

  public :: mismatch_intent_init
  public :: mismatch_intent_run
  public :: mismatch_intent_finalize

contains

  !> \section arg_table_mismatch_intent_run  Argument Table
  !! \htmlinclude arg_table_mismatch_intent_run.html
  !!
  subroutine mismatch_intent_run(foo, timestep, temp_prev, temp_layer, qv, ps, &
      errmsg, errflg)

    integer, intent(in) :: foo
    real(kind=kind_phys), intent(in) :: timestep
    real(kind=kind_phys), intent(inout) :: qv(:)
    real(kind=kind_phys), intent(inout) :: ps(:)
    real(kind=kind_phys), intent(in) :: temp_prev(:)
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

  end subroutine mismatch_intent_run

  !> \section arg_table_mismatch_intent_init  Argument Table
  !! \htmlinclude arg_table_mismatch_intent_init.html
  !!
  subroutine mismatch_intent_init(errmsg, errflg)

    character(len=512), intent(out) :: errmsg
    integer, intent(out) :: errflg

    ! This routine currently does nothing

    errmsg = ''
    errflg = 0

  end subroutine mismatch_intent_init

  !> \section arg_table_mismatch_intent_finalize  Argument Table
  !! \htmlinclude arg_table_mismatch_intent_finalize.html
  !!
  subroutine mismatch_intent_finalize(errmsg, errflg)

    character(len=512), intent(out) :: errmsg
    integer, intent(out) :: errflg

    ! This routine currently does nothing

    errmsg = ''
    errflg = 0

  end subroutine mismatch_intent_finalize

end module mismatch_intent
