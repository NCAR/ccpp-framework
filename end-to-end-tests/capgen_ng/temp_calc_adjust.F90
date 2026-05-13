!Test parameterization with no vertical level and hanging intent(out) variable
!

module temp_calc_adjust

  use ccpp_kinds, only: kind_phys

  implicit none
  private

  public :: temp_calc_adjust_register
  public :: temp_calc_adjust_init
  public :: temp_calc_adjust_run
  public :: temp_calc_adjust_final

contains

! codee format off
!> \section arg_table_temp_calc_adjust_register  Argument Table
!! \htmlinclude arg_table_temp_calc_adjust_register.html
!!
  SUBROUTINE temp_calc_adjust_register(dim_inter, errmsg, errflg)
! codee format on
    integer, intent(out) :: dim_inter
    character(len=512), intent(out) :: errmsg
    integer, intent(out) :: errflg

    errflg = 0
    errmsg = ''
    dim_inter = 3
  end subroutine temp_calc_adjust_register

  !> \section arg_table_temp_calc_adjust_run  Argument Table
  !! \htmlinclude arg_table_temp_calc_adjust_run.html
  !!
  subroutine temp_calc_adjust_run(nbox, timestep, temp_level, temp_calc, &
      errmsg, errflg)

    integer, intent(in) :: nbox
    real(kind=kind_phys), intent(in) :: timestep
    real(kind=kind_phys), intent(in) :: temp_level(:, :)
    real(kind=kind_phys), intent(out) :: temp_calc(:, :)
    character(len=512), intent(out) :: errmsg
    integer, intent(out) :: errflg
    !----------------------------------------------------------------

    integer :: col_index
    real(kind=kind_phys) :: bar = 1.0_kind_phys

    errmsg = ''
    errflg = 0

    call temp_calc_adjust_nested_subroutine(temp_calc)
    if (check_foo()) then
      call foo(bar)
    end if

  contains

    elemental subroutine temp_calc_adjust_nested_subroutine(temp)

      real(kind=kind_phys), intent(out) :: temp
      !-------------------------------------------------------------

      temp = 1.0_kind_phys

    end subroutine temp_calc_adjust_nested_subroutine

    subroutine foo(bar)
      real(kind=kind_phys), intent(inout) :: bar
      bar = bar + 1.0_kind_phys

    end subroutine foo

    logical function check_foo()
      check_foo = .true.
    end function check_foo

  end subroutine temp_calc_adjust_run

  !> \section arg_table_temp_calc_adjust_init  Argument Table
  !! \htmlinclude arg_table_temp_calc_adjust_init.html
  !!
  subroutine temp_calc_adjust_init(errmsg, errflg)

    character(len=512), intent(out) :: errmsg
    integer, intent(out) :: errflg

    ! This routine currently does nothing

    errmsg = ''
    errflg = 0

  end subroutine temp_calc_adjust_init

  !> \section arg_table_temp_calc_adjust_final  Argument Table
  !! \htmlinclude arg_table_temp_calc_adjust_final.html
  !!
  subroutine temp_calc_adjust_final(errmsg, errflg)

    character(len=512), intent(out) :: errmsg
    integer, intent(out) :: errflg

    ! This routine currently does nothing

    errmsg = ''
    errflg = 0

  end subroutine temp_calc_adjust_final

end module temp_calc_adjust
