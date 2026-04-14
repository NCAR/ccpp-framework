! Test parameterization with no vertical level
!

module missing_fort_header

  use ccpp_kinds, only: kind_phys

  implicit none
  private

  public :: missing_fort_header_init
  public :: missing_fort_header_run
  public :: missing_fort_header_finalize

contains

  !> \section fort_header_missing_arg_table_run  Argument Table
  !! \htmlinclude fort_header_missing_arg_table_run.html
  !!
  subroutine missing_fort_header_run(foo, timestep, temp_prev, temp_layer, qv, ps, &
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

  end subroutine missing_fort_header_run

  !> \section fort_header_missing_arg_table_init  Argument Table
  !! \htmlinclude fort_header_missing_arg_table_init.html
  !!
  subroutine missing_fort_header_init(errmsg, errflg)

    character(len=512), intent(out) :: errmsg
    integer, intent(out) :: errflg

    ! This routine currently does nothing

    errmsg = ''
    errflg = 0

  end subroutine missing_fort_header_init

  !!
  subroutine missing_fort_header_finalize(errmsg, errflg)

    character(len=512), intent(out) :: errmsg
    integer, intent(out) :: errflg

    ! This routine currently does nothing

    errmsg = ''
    errflg = 0

  end subroutine missing_fort_header_finalize

end module missing_fort_header
