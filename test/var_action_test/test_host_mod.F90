module test_host_mod

   use ccpp_kinds,     only: kind_phys
   use test_host_data, only: physics_state, allocate_physics_state

   implicit none
   public

   !> \section arg_table_test_host_mod  Argument Table
   !! \htmlinclude arg_table_test_host_host.html
   !!
   integer,         parameter   :: ncols = 12
   integer,         parameter   :: pver = 4
   type(physics_state)          :: phys_state
   real(kind_phys)              :: effrs(ncols, pver)

   public :: init_data
   public :: compare_data

contains

  subroutine init_data()

    ! Allocate and initialize state
    call allocate_physics_state(ncols, pver, phys_state)
    phys_state%effrr = 1.0E-3 ! 1000 microns, in meter
    phys_state%effrl = 1.0E-4 ! 100 microns, in meter
    phys_state%effri = 5.0E-5 ! 50 microns, in meter
    effrs            = 5.0E-4 ! 500 microns, in meter

  end subroutine init_data

  logical function compare_data()

    real(kind_phys), parameter :: effrr_expected = 1.0E-3 ! 1000 microns, in meter
    real(kind_phys), parameter :: effrl_expected = 5.0E-5 ! 50 microns, in meter
    real(kind_phys), parameter :: effri_expected = 7.5E-5 ! 75 microns, in meter
    real(kind_phys), parameter :: effrs_expected = 5.1E-4 ! 510 microns, in meter
    real(kind_phys), parameter :: tolerance = 1.0E-3   ! used as scaling factor for expected value

    compare_data = .true.

    if (maxval(abs(phys_state%effrr - effrr_expected)) > tolerance*effrr_expected) then
        write(6, '(a,e16.7,a,e16.7)') 'Error: max diff of phys_state%effrr from expected value exceeds tolerance: ', &
                                      maxval(abs(phys_state%effrr - effrr_expected)), ' > ', tolerance*effrr_expected
        compare_data = .false.
    end if

    if (maxval(abs(phys_state%effrl - effrl_expected)) > tolerance*effrl_expected) then
        write(6, '(a,e16.7,a,e16.7)') 'Error: max diff of phys_state%effrl from expected value exceeds tolerance: ', &
                                      maxval(abs(phys_state%effrl - effrl_expected)), ' > ', tolerance*effrl_expected
        compare_data = .false.
    end if

    if (maxval(abs(phys_state%effri - effri_expected)) > tolerance*effri_expected) then
        write(6, '(a,e16.7,a,e16.7)') 'Error: max diff of phys_state%effri from expected value exceeds tolerance: ', &
                                      maxval(abs(phys_state%effri - effri_expected)), ' > ', tolerance*effri_expected
        compare_data = .false.
    end if

    if (maxval(abs(           effrs - effrs_expected)) > tolerance*effrs_expected) then
        write(6, '(a,e16.7,a,e16.7)') 'Error: max diff of            effrs from expected value exceeds tolerance: ', &
                                      maxval(abs(           effrs - effrs_expected)), ' > ', tolerance*effrs_expected
        compare_data = .false.
    end if

  end function compare_data

end module test_host_mod
