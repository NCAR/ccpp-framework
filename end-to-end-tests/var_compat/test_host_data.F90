module test_host_data

  use ccpp_kinds, only: kind_phys
  use mod_rad_ddt, only: ty_rad_lw, &
      ty_rad_sw

  implicit none
  private

  !> \section arg_table_physics_state  Argument Table
  !! \htmlinclude arg_table_physics_state.html
  type physics_state
    real(kind=kind_phys), dimension(:, :), allocatable :: &
        effrr, & ! effective radius of cloud rain
        effrl, & ! effective radius of cloud liquid water
        effri, & ! effective radius of cloud ice
        effrg, & ! effective radius of cloud graupel
        ncg, & ! number concentration of cloud graupel
        nci ! number concentration of cloud ice
    real(kind=kind_phys) :: scalar_var
    type(ty_rad_lw), dimension(:), allocatable :: &
        fluxlw ! Longwave radiation fluxes
    type(ty_rad_sw) :: &
        fluxsw ! Shortwave radiation fluxes
    real(kind=kind_phys) :: scalar_vara
    real(kind=kind_phys) :: scalar_varb
    real(kind=kind_phys) :: tke, tke2
    integer :: scalar_varc
    integer :: scheme_order
    integer :: num_subcycles
  end type physics_state

  public :: physics_state
  public :: allocate_physics_state

contains

  subroutine allocate_physics_state(cols, levels, state, has_graupel, has_ice)
    integer, intent(in) :: cols
    integer, intent(in) :: levels
    type(physics_state), intent(out) :: state
    logical, intent(in) :: has_graupel
    logical, intent(in) :: has_ice

    if (allocated(state%effrr)) then
      deallocate(state%effrr)
    end if
    allocate(state%effrr(cols, levels))

    if (allocated(state%effrl)) then
      deallocate(state%effrl)
    end if
    allocate(state%effrl(cols, levels))

    if (has_ice) then
      if (allocated(state%effri)) then
        deallocate(state%effri)
      end if
      allocate(state%effri(cols, levels))
    end if

    if (has_graupel) then
      if (allocated(state%effrg)) then
        deallocate(state%effrg)
      end if
      allocate(state%effrg(cols, levels))

      if (allocated(state%ncg)) then
        deallocate(state%ncg)
      end if
      allocate(state%ncg(cols, levels))
    end if

    if (has_ice) then
      if (allocated(state%nci)) then
        deallocate(state%nci)
      end if
      allocate(state%nci(cols, levels))
    end if

    if (allocated(state%fluxlw)) then
      deallocate(state%fluxlw)
    end if
    allocate(state%fluxlw(cols))

    if (associated(state%fluxsw%sfc_up_sw)) then
      nullify(state%fluxsw%sfc_up_sw)
    end if
    allocate(state%fluxsw%sfc_up_sw(cols))

    if (associated(state%fluxsw%sfc_down_sw)) then
      nullify(state%fluxsw%sfc_down_sw)
    end if
    allocate(state%fluxsw%sfc_down_sw(cols))

    ! Initialize scheme counter.
    state%scheme_order = 1
    ! Initialize subcycle counter.
    state%num_subcycles = 3

  end subroutine allocate_physics_state

end module test_host_data
