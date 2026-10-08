module temp_calc_adjust

  implicit none
  private

contains

  subroutine temp_calc_adjust_init(im, errmsg, errflg)
    integer,          intent(in)  :: im
    character(len=*), intent(out) :: errmsg
    integer,          intent(out) :: errflg
    errmsg = ''
    errflg = 0
  end subroutine temp_calc_adjust_init

  subroutine temp_calc_adjust_run(im, timestep, temp, &
                                   errmsg, errflg)
    use ccpp_kinds, only: kind_phys
    integer,              intent(in)    :: im
    real(kind=kind_phys), intent(in)    :: timestep
    real(kind=kind_phys), intent(inout) :: temp(:,:)
    character(len=*),     intent(out)   :: errmsg
    integer,              intent(out)   :: errflg
    errmsg = ''
    errflg = 0
  end subroutine temp_calc_adjust_run

  subroutine temp_calc_adjust_final(errmsg, errflg)
    character(len=*), intent(out) :: errmsg
    integer,          intent(out) :: errflg
    errmsg = ''
    errflg = 0
  end subroutine temp_calc_adjust_final

end module temp_calc_adjust
