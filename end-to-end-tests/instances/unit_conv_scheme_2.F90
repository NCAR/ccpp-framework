!>\file unit_conv_scheme_2.F90
!! This file contains a unit_conv_scheme_2 CCPP scheme that does nothing
!! except requesting the minimum, mandatory variables.

module unit_conv_scheme_2

  use, intrinsic :: iso_fortran_env, only: error_unit
  use ccpp_kinds, only : kind_phys
  implicit none

  private
  public :: unit_conv_scheme_2_run

  ! This is for unit testing only
  real(kind=kind_phys), parameter, dimension(1:2) :: target_values = (/-1.0E-3_kind_phys, 1.0E-3_kind_phys/)
  real(kind=kind_phys), parameter, dimension(1:2) :: target_values2 = (/-42.0_kind_phys, 42.0_kind_phys/)

contains

  !! \section arg_table_unit_conv_scheme_2_run Argument Table
  !! \htmlinclude unit_conv_scheme_2_run.html
  !!
  subroutine unit_conv_scheme_2_run(instance, data_array, data_array2, data_array_opt, errmsg, errflg)
    character(len=*), intent(out) :: errmsg
    integer, intent(out) :: errflg
    integer, intent(in) :: instance
    real(kind=kind_phys), intent(inout) :: data_array(:)
    real(kind=kind_phys), intent(inout) :: data_array2(:)
    real(kind=kind_phys), intent(inout), optional :: data_array_opt(:)

    ! Initialize CCPP error handling variables
    errmsg = ''
    errflg = 0

    ! Check values in data array
    write(error_unit, '(a,e12.4)') &
        'In unit_conv_scheme_2_run: checking min/max values of data array to be approximately ', &
        target_values(instance)
    if (abs(minval(data_array) - target_values(instance)) > 0.01_kind_phys .or. &
        abs(maxval(data_array) - target_values(instance)) > 0.01_kind_phys) then
      write(errmsg, '(3(a,e12.4),a)') &
          "Error in unit_conv_scheme_2_run, expected values for data_array of approximately ", &
          target_values(instance), "  but got [ ", minval(data_array), " : ", maxval(data_array), " ]"
      errflg = 1
      return
    end if
    ! Check values in data array2
    write(error_unit, '(a,e12.4)') &
        'In unit_conv_scheme_2_run: checking min/max values of data array 2 to be approximately ', &
        target_values2(instance)
    if (abs(minval(data_array2) - target_values2(instance)) > 0.01_kind_phys .or. &
        abs(maxval(data_array2) - target_values2(instance)) > 0.01_kind_phys) then
      write(errmsg, '(3(a,e12.4),a)') &
          "Error in unit_conv_scheme_2_run, expected values for data array 2 of approximately ", &
          target_values2(instance), "  but got [ ", minval(data_array2), " : ", maxval(data_array2), " ]"
      errflg = 1
      return
    end if
    ! Check for presence of optional data array, then check its values
    write(error_unit, '(a)') 'In unit_conv_scheme_2_run: checking for presence of optional data array'
    if (instance==1) then
      if (.not. present(data_array_opt)) then
        write(errmsg, '(a)') 'Error in unit_conv_scheme_2_run, optional data array expected but not present'
        write(errmsg, '(a,i0)') 'for instance ', instance
        errflg = 1
        return
      end if
      write(error_unit, '(a,e12.4)') &
          'In unit_conv_scheme_2_run: checking min/max values of optional data array to be approximately ', target_values(instance)
      if (abs(minval(data_array_opt) - target_values(instance)) > 0.01_kind_phys .or. &
          abs(maxval(data_array_opt) - target_values(instance)) > 0.01_kind_phys) then
        write(errmsg, '(3(a,e12.4),a)') 'Error in unit_conv_scheme_2_run, expected values of approximately ', &
            target_values(instance), '  but got [ ', minval(data_array_opt), ' : ', maxval(data_array_opt), ' ]'
        errflg = 1
        return
      end if
    else if (instance==2 .and. present(data_array_opt)) then
      write(errmsg, '(a)') 'Error in unit_conv_scheme_2_run, optional data array not expected but present'
      write(errmsg, '(a,i0)') 'for instance ', instance
      errflg = 1
      return
    end if

  end subroutine unit_conv_scheme_2_run

end module unit_conv_scheme_2
