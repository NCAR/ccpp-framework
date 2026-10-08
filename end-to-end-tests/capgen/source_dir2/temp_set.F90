!Test 3D parameterization
!

module temp_set

  use ccpp_kinds, only: kind_phys, &
      kind_temp

  implicit none
  private

  public :: temp_set_init
  public :: temp_set_timestep_init
  public :: temp_set_run
  public :: temp_set_final

contains

  !> \section arg_table_temp_set_run  Argument Table
  !! \htmlinclude arg_table_temp_set_run.html
  !!
  subroutine temp_set_run(ncol, lev, timestep, temp_level, temp_diag, temp, ps, &
      to_promote, promote_pcnst, slev_lbound, soil_levs, var_array, errmsg, errflg)
    !----------------------------------------------------------------
    implicit none
    !----------------------------------------------------------------

    integer, intent(in) :: ncol, lev, slev_lbound
    real(kind=kind_phys), intent(out) :: temp(:, :)
    real(kind=kind_phys), intent(in) :: timestep
    real(kind=kind_phys), intent(in) :: ps(:)
    real(kind=kind_phys), intent(inout) :: temp_level(:, :)
    real(kind=kind_phys), intent(inout) :: temp_diag(:, :)
    real(kind=kind_phys), intent(inout) :: soil_levs(slev_lbound:)
    real(kind=kind_phys), intent(inout) :: var_array(:, :, :, :)
    real(kind=kind_temp), intent(out) :: to_promote(:, :)
    real(kind=kind_phys), intent(out) :: promote_pcnst(:)
    character(len=512), intent(out) :: errmsg
    integer, intent(out) :: errflg
    !----------------------------------------------------------------
    integer :: ilev

    integer :: col_index
    integer :: lev_index
    real(kind=kind_phys) :: internal_scalar_var

    errmsg = ''
    errflg = 0

    ilev = size(temp_level, 2)
    if (ilev /= (lev + 1)) then
      errflg = 1
      errmsg = 'Invalid value for ilev, must be lev+1'
      return
    end if

    do col_index = 1, ncol
      do lev_index = 1, lev
        temp(col_index, lev_index) = (temp_level(col_index, lev_index) &
            + temp_level(col_index, lev_index + 1)) / 2.0_kind_phys
      end do
    end do

    var_array(:, :, :, :) = 1._kind_phys

    !
    internal_scalar_var = soil_levs(slev_lbound)
    internal_scalar_var = soil_levs(0)

  end subroutine temp_set_run

  !> \section arg_table_temp_set_init  Argument Table
  !! \htmlinclude arg_table_temp_set_init.html
  !!
  !subroutine temp_set_init(temp_inc_in, fudge, temp_inc_set, errmsg, errflg)
  subroutine temp_set_init(temp_inc_in, temp_inc_set, errmsg, errflg)

    real(kind=kind_phys), intent(in) :: temp_inc_in
    !real(kind=kind_phys), intent(in) :: fudge
    real(kind=kind_phys), intent(out) :: temp_inc_set
    character(len=512), intent(out) :: errmsg
    integer, intent(out) :: errflg

    temp_inc_set = temp_inc_in

    errmsg = ''
    errflg = 0

  end subroutine temp_set_init

  !> \section arg_table_temp_set_timestep_init  Argument Table
  !! \htmlinclude arg_table_temp_set_timestep_init.html
  !!
  subroutine temp_set_timestep_init(ncol, temp_inc, temp_level, &
      errmsg, errflg)

    integer, intent(in) :: ncol
    real(kind=kind_phys), intent(in) :: temp_inc
    real(kind=kind_phys), intent(inout) :: temp_level(:, :)
    character(len=512), intent(out) :: errmsg
    integer, intent(out) :: errflg

    errmsg = ''
    errflg = 0

    temp_level = temp_level + temp_inc

  end subroutine temp_set_timestep_init

  !> \section arg_table_temp_set_final  Argument Table
  !! \htmlinclude arg_table_temp_set_final.html
  !!
  subroutine temp_set_final(errmsg, errflg)

    character(len=512), intent(out) :: errmsg
    integer, intent(out) :: errflg

    ! This routine currently does nothing

    errmsg = ''
    errflg = 0

  end subroutine temp_set_final

end module temp_set
