module temp_calc_adjust

  implicit none
  private

contains

  ! init has wrong arg count: missing errflg
  subroutine temp_calc_adjust_init(im, errmsg)
    integer,          intent(in)  :: im
    character(len=*), intent(out) :: errmsg
  end subroutine temp_calc_adjust_init

  ! run has a renamed arg (tempo instead of temp)
  subroutine temp_calc_adjust_run(im, timestep, tempo, errmsg, errflg)
    integer,  intent(in)    :: im
    real,     intent(in)    :: timestep
    real,     intent(inout) :: tempo(:,:)
    character(len=*), intent(out) :: errmsg
    integer,          intent(out) :: errflg
  end subroutine temp_calc_adjust_run

  subroutine temp_calc_adjust_final(errmsg, errflg)
    character(len=*), intent(out) :: errmsg
    integer,          intent(out) :: errflg
  end subroutine temp_calc_adjust_final

end module temp_calc_adjust
