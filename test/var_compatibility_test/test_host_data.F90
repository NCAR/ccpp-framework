module test_host_data

  use ccpp_kinds, only: kind_phys

  !> \section arg_table_physics_state  Argument Table
  !! \htmlinclude arg_table_physics_state.html
  type physics_state
     real(kind_phys), dimension(:,:), allocatable :: &
          effrr,                                     & ! effective radius of cloud rain
          effrl,                                     & ! effective radius of cloud liquid water
          effri,                                     & ! effective radius of cloud ice
          effrg                                        ! effective radius of cloud graupel
  end type physics_state

  public allocate_physics_state

contains

  subroutine allocate_physics_state(cols, levels, state, has_graupel)
    integer,             intent(in)  :: cols
    integer,             intent(in)  :: levels
    type(physics_state), intent(out) :: state
    logical,             intent(in)  :: has_graupel

    if (allocated(state%effrr)) then
       deallocate(state%effrr)
    end if
    allocate(state%effrr(cols, levels))

    if (allocated(state%effrl)) then
       deallocate(state%effrl)
    end if
    allocate(state%effrl(cols, levels))

    if (allocated(state%effri)) then
       deallocate(state%effri)
    end if
    allocate(state%effri(cols, levels))

    if (has_graupel) then
       if (allocated(state%effrg)) then
          deallocate(state%effrg)
       end if
       allocate(state%effrg(cols, levels))
    endif

  end subroutine allocate_physics_state

end module test_host_data
