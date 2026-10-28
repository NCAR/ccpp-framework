module test_host_mod

  use ccpp_kinds, only: kind_phys
  use test_host_data, only: physics_state, &
      allocate_physics_state

  implicit none
  public

  !> \section arg_table_test_host_mod  Argument Table
  !! \htmlinclude arg_table_test_host_host.html
  !!
  integer, parameter :: ncols = 12
  integer, parameter :: pver = 4
  type(physics_state), target :: phys_state
  real(kind=kind_phys) :: effrs(ncols, pver)
  logical, parameter :: has_ice = .true.
  logical, parameter :: has_graupel = .true.

  public :: init_data
  public :: compare_data

contains

  subroutine init_data()

    ! Allocate and initialize state
    call allocate_physics_state(ncols, pver, phys_state, has_graupel, has_ice)
    phys_state%effrr = 1.0E-3 ! 1000 microns, in meter
    phys_state%effrl = 1.0E-4 ! 100 microns, in meter
    phys_state%scalar_var = 1.0 ! in m
    phys_state%scalar_vara = 273.15 ! in K
    phys_state%scalar_varb = 1013.0 ! in mb
    phys_state%scalar_varc = 380 ! in ppmv
    effrs = 5.0E-4 ! 500 microns, in meter
    if (has_graupel) then
      phys_state%effrg = 2.5E-4 ! 250 microns, in meter
      phys_state%ncg = 40
    end if
    if (has_ice) then
      phys_state%effri = 5.0E-5 ! 50 microns, in meter
      phys_state%nci = 80
    end if
    phys_state%tke = 10.0 !J kg-1
    phys_state%tke2 = 42.0 !J kg-1

  end subroutine init_data

  logical function compare_data()

    real(kind=kind_phys), parameter :: effrr_expected = 1.0E-3 ! 1000 microns, in meter
    real(kind=kind_phys), parameter :: effrl_expected = 5.0E-5 ! 50 microns, in meter
    real(kind=kind_phys), parameter :: effri_expected = 7.5E-5 ! 75 microns, in meter
    real(kind=kind_phys), parameter :: effrs_expected = 5.3E-4 ! 530 microns, in meter
    real(kind=kind_phys), parameter :: scalar_expected = 2.0E3 ! 2 km, in meter
    real(kind=kind_phys), parameter :: tke_expected = 10.0 ! 10 J kg-1
    real(kind=kind_phys), parameter :: tolerance = 1.0E-6 ! used as scaling factor for expected value
    real(kind=kind_phys), parameter :: sfc_up_sw_expected = 100. ! W/m2
    real(kind=kind_phys), parameter :: sfc_down_sw_expected = 400. ! W/m2
    real(kind=kind_phys), parameter :: sfc_up_lw_expected = 300. ! W/m2
    real(kind=kind_phys), parameter :: sfc_down_lw_expected = 50. ! W/m2

    compare_data = .true.

    if (maxval(abs(phys_state%effrr - effrr_expected)) > tolerance * effrr_expected) then
      write(6, '(a,e16.7,a,e16.7)') 'Error: max diff of phys_state%effrr from expected value exceeds tolerance: ', &
          maxval(abs(phys_state%effrr - effrr_expected)), ' > ', tolerance * effrr_expected
      compare_data = .false.
    end if

    if (maxval(abs(phys_state%effrl - effrl_expected)) > tolerance * effrl_expected) then
      write(6, '(a,e16.7,a,e16.7)') 'Error: max diff of phys_state%effrl from expected value exceeds tolerance: ', &
          maxval(abs(phys_state%effrl - effrl_expected)), ' > ', tolerance * effrl_expected
      compare_data = .false.
    end if

    if (maxval(abs(phys_state%effri - effri_expected)) > tolerance * effri_expected) then
      write(6, '(a,e16.7,a,e16.7)') 'Error: max diff of phys_state%effri from expected value exceeds tolerance: ', &
          maxval(abs(phys_state%effri - effri_expected)), ' > ', tolerance * effri_expected
      compare_data = .false.
    end if

    if (maxval(abs(effrs - effrs_expected)) > tolerance * effrs_expected) then
      write(6, '(a,e16.7,a,e16.7)') 'Error: max diff of            effrs from expected value exceeds tolerance: ', &
          maxval(abs(effrs - effrs_expected)), ' > ', tolerance * effrs_expected
      compare_data = .false.
    end if

    if (abs(phys_state%scalar_var - scalar_expected) > tolerance * scalar_expected) then
      write(6, '(a,e16.7,a,e16.7)') &
          'Error: max diff of            scalar_var from expected value exceeds tolerance: ', &
          abs(phys_state%scalar_var - scalar_expected), ' > ', tolerance * scalar_expected
      compare_data = .false.
    end if

    if (abs(phys_state%tke - tke_expected) > tolerance * tke_expected) then
      write(6, '(a,e16.7,a,e16.7)') 'Error: max diff of            tke from expected value exceeds tolerance: ', &
          abs(phys_state%tke - tke_expected), ' > ', tolerance * tke_expected
      compare_data = .false.
    end if

    if (maxval(abs(phys_state%fluxsw%sfc_up_sw - sfc_up_sw_expected)) > tolerance * sfc_up_sw_expected) then
      write(6, '(a,e16.7,a,e16.7)') &
          'Error: max diff of            sfc_up_sw from expected value exceeds tolerance: ', &
          abs(phys_state%fluxsw%sfc_up_sw - sfc_up_sw_expected), ' > ', tolerance * sfc_up_sw_expected
      compare_data = .false.
    end if

    if (maxval(abs(phys_state%fluxsw%sfc_down_sw - sfc_down_sw_expected)) > tolerance * sfc_down_sw_expected) then
      write(6, '(a,e16.7,a,e16.7)') &
          'Error: max diff of            sfc_down_sw from expected value exceeds tolerance: ', &
          abs(phys_state%fluxsw%sfc_down_sw - sfc_down_sw_expected), ' > ', tolerance * sfc_down_sw_expected
      compare_data = .false.
    end if

    if (maxval(abs(phys_state%fluxlw%sfc_up_lw - sfc_up_lw_expected)) > tolerance * sfc_up_lw_expected) then
      write(6, '(a,e16.7,a,e16.7)') &
          'Error: max diff of            sfc_up_lw from expected value exceeds tolerance: ', &
          abs(phys_state%fluxlw%sfc_up_lw - sfc_up_lw_expected), ' > ', tolerance * sfc_up_lw_expected
      compare_data = .false.
    end if

    if (maxval(abs(phys_state%fluxlw%sfc_down_lw - sfc_down_lw_expected)) > tolerance * sfc_down_lw_expected) then
      write(6, '(a,e16.7,a,e16.7)') &
          'Error: max diff of            sfc_down_lw from expected value exceeds tolerance: ', &
          abs(phys_state%fluxlw%sfc_down_lw - sfc_down_lw_expected), ' > ', tolerance * sfc_down_lw_expected
      compare_data = .false.
    end if

  end function compare_data

end module test_host_mod
