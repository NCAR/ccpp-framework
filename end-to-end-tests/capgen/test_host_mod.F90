module test_host_mod

  use ccpp_kinds, only: kind_phys
  use test_host_data, only: physics_state, &
      allocate_physics_state

  implicit none
  public

  !> \section arg_table_test_host_mod  Argument Table
  !! \htmlinclude arg_table_test_host_host.html
  !!
  integer, parameter :: ncols = 10
  integer, parameter :: pver = 5
  integer, parameter :: pverp = 6
  integer, parameter :: pcnst = 2
  integer, parameter :: slevs = 4
  integer, parameter :: slev_lbound = -3
  integer, parameter :: slev_ubound = 0
  integer, parameter :: diagdimstart = 2
  integer, parameter :: index_qv = 1
  logical, parameter :: config_var = .true.
  real(kind=kind_phys), allocatable :: temp_midpoints(:, :)
  real(kind=kind_phys) :: temp_interfaces(ncols, pverp)
  real(kind=kind_phys) :: temp_diag(ncols, 6)
  real(kind=kind_phys) :: coeffs(ncols)
  real(kind=kind_phys) :: var_array(ncols, 2, 4, 6)
  real(kind=kind_phys), dimension(diagdimstart:ncols, diagdimstart:pver) :: &
      diag1, &
      diag2
  real(kind=kind_phys) :: dt
  real(kind=kind_phys), parameter :: temp_inc = 0.05_kind_phys
  type(physics_state) :: phys_state
  integer :: num_model_times = -1
  integer, allocatable :: model_times(:)

  integer, parameter :: num_time_steps = 2
  real(kind=kind_phys), parameter :: tolerance = 1.0e-13_kind_phys
  real(kind=kind_phys) :: tint_save(ncols, pverp)

  public :: init_data
  public :: compare_data
  public :: check_model_times

contains

  subroutine init_data()

    integer :: col
    integer :: lev
    integer :: cind
    integer :: offsize

    ! Allocate and initialize temperature
    allocate(temp_midpoints(ncols, pver))
    temp_midpoints = 0.0_kind_phys
    cind = 1
    do lev = 1, pverp
      offsize = ((cind - 1) * (ncols * pver)) + ((lev - 1) * ncols)
      do col = 1, ncols
        temp_interfaces(col, lev) = real(offsize + col, kind=kind_phys)
        tint_save(col, lev) = temp_interfaces(col, lev)
      end do
    end do
    ! Allocate and initialize state
    call allocate_physics_state(ncols, pver, pcnst, slev_lbound, slev_ubound, phys_state)
    do cind = 1, pcnst
      do lev = 1, pver
        offsize = ((cind - 1) * (ncols * pver)) + ((lev - 1) * ncols)
        do col = 1, ncols
          phys_state%q(col, lev, cind) = real(offsize + col, kind=kind_phys)
        end do
      end do
    end do

  end subroutine init_data

  logical function check_model_times()

    check_model_times = (num_model_times > 0)
    if (check_model_times) then
      check_model_times = (size(model_times) == num_model_times)
      if (.not. check_model_times) then
        write(6, '(2(a,i0))') 'model_times size mismatch, ', &
            size(model_times), ' should be ', num_model_times
      end if
    else
      write(6, '(a,i0,a)') 'num_model_times mismatch, ', num_model_times, &
          ' should be greater than zero'
    end if

  end function check_model_times

  logical function compare_data()

    integer :: col
    integer :: lev
    integer :: cind
    integer :: offsize
    logical :: need_header
    real(kind=kind_phys) :: avg
    integer, parameter :: cincrements(pcnst) = (/ 1, 0 /)
    real(kind=kind_phys) :: total_test
    real(kind=kind_phys), parameter :: total_ref = 6730.0_kind_phys

    compare_data = .true.

    total_test = 0.0_kind_phys
    need_header = .true.
    do lev = 1, pver
      do col = 1, ncols
        avg = (tint_save(col, lev) + tint_save(col, lev + 1))
        avg = 1.0_kind_phys + (avg / 2.0_kind_phys)
        avg = avg + (temp_inc * num_time_steps)
        total_test = total_test + avg
        if (abs((temp_midpoints(col, lev) - avg) / avg) > tolerance) then
          if (need_header) then
            write(6, '("  COL  LEV      T MIDPOINTS        EXPECTED")')
            need_header = .false.
          end if
          write(6, '(2i5,2(3x,es15.7))') col, lev, &
              temp_midpoints(col, lev), avg
          compare_data = .false.
        end if
      end do
    end do
    ! Check constituents
    need_header = .true.
    do cind = 1, pcnst
      do lev = 1, pver
        offsize = ((cind - 1) * (ncols * pver)) + ((lev - 1) * ncols)
        do col = 1, ncols
          avg = real(offsize + col + (cincrements(cind) * num_time_steps), &
              kind=kind_phys)
          total_test = total_test + avg
          if (abs((phys_state%q(col, lev, cind) - avg) / avg) > &
              tolerance) then
            if (need_header) then
              write(6, '(2(2x,a),3x,a,10x,a,14x,a)') &
                  'COL', 'LEV', 'C#', 'Q', 'EXPECTED'
              need_header = .false.
            end if
            write(6, '(3i5,2(3x,es15.7))') col, lev, cind, &
                phys_state%q(col, lev, cind), avg
            compare_data = .false.
          end if
        end do
      end do
    end do
    if (abs((total_test - total_ref) / total_ref) > tolerance) then
      write(6, '(a,e12.4)') 'TOTAL REFERENCE:     ', total_ref
      write(6, '(a,e12.4)') 'TOTAL TEST:          ', total_test
      write(6, '(2(a,e12.4))') 'REL.DIFF > TOLERANCE:', &
          abs((total_test - total_ref) / total_ref), ' >', tolerance
      compare_data = .false.
    else
      write(0, '(a,e12.4)') 'TOTAL REFERENCE:     ', total_ref
      write(0, '(a,e12.4)') 'TOTAL TEST:          ', total_test
      write(0, '(2(a,e12.4))') 'REL.DIFF < TOLERANCE:', &
          abs((total_test - total_ref) / total_ref), ' <', tolerance
    end if
  end function compare_data

end module test_host_mod
