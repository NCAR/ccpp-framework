module test_host_data

  use ccpp_kinds, only: kind_phys

  !> \section arg_table_physics_state  Argument Table
  !! \htmlinclude arg_table_physics_state.html
  type physics_state
     real(kind_phys), dimension(:,:), allocatable :: &
          effrr,                                     & ! effective radius of cloud rain
          effrl,                                     & ! effective radius of cloud liquid water
          effri,                                     & ! effective radius of cloud ice
          effrg,                                     & ! effective radius of cloud graupel
          ncg,                                       & ! number concentration of cloud graupel
          nci                                          ! number concentration of cloud ice
     real(kind_phys) :: scalar_var
  end type physics_state

  public allocate_physics_state

contains

  subroutine allocate_physics_state(cols, levels, state, has_graupel, has_ice)
    integer,             intent(in)  :: cols
    integer,             intent(in)  :: levels
    type(physics_state), intent(out) :: state
    logical,             intent(in)  :: has_graupel
    logical,             intent(in)  :: has_ice

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
    endif

    if (has_graupel) then
       if (allocated(state%effrg)) then
          deallocate(state%effrg)
       end if
       allocate(state%effrg(cols, levels))

       if (allocated(state%ncg)) then
          deallocate(state%ncg)
       end if
       allocate(state%ncg(cols, levels))
    endif

    if (has_ice) then
       if (allocated(state%nci)) then
          deallocate(state%nci)
       end if
       allocate(state%nci(cols, levels))
    endif

  end subroutine allocate_physics_state

end module test_host_data
