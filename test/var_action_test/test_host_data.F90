module test_host_data

  use ccpp_kinds, only: kind_phys

  !> \section arg_table_physics_state  Argument Table
  !! \htmlinclude arg_table_physics_state.html
  type physics_state
     real(kind_phys), dimension(:,:), allocatable :: &
          effrr,                                     & ! effective radius of cloud rain
          effrl,                                     & ! effective radius of cloud liquid water
          effri                                        ! effective radius of cloud ice
  end type physics_state

  public allocate_physics_state

contains

  subroutine allocate_physics_state(cols, levels, state)
    integer,             intent(in)  :: cols
    integer,             intent(in)  :: levels
    type(physics_state), intent(out) :: state

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

  end subroutine allocate_physics_state

end module test_host_data
