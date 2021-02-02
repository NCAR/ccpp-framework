module test_host_data

  use ccpp_kinds, only: kind_phys

  !> \section arg_table_physics_state  Argument Table
  !! \htmlinclude arg_table_physics_state.html
  type physics_state
     real(kind_phys), dimension(:), allocatable ::   &
          ps                                           ! surface pressure
     real(kind_phys), dimension(:,:), allocatable :: &
          temp                                         ! temperature
     real(kind_phys), dimension(:,:,:),allocatable :: &
          q         ! constituent mixing ratio (kg/kg moist or dry air depending on type)
  end type physics_state

  public allocate_physics_state

contains

  subroutine allocate_physics_state(cols, levels, constituents, state)
    integer,             intent(in)  :: cols
    integer,             intent(in)  :: levels
    integer,             intent(in)  :: constituents
    type(physics_state), intent(out) :: state

    if (allocated(state%ps)) then
       deallocate(state%ps)
    end if
    allocate(state%ps(cols))
    if (allocated(state%temp)) then
       deallocate(state%temp)
    end if
    allocate(state%temp(cols, levels))
    if (allocated(state%q)) then
       deallocate(state%q)
    end if
    allocate(state%q(cols, levels, constituents))

  end subroutine allocate_physics_state

end module test_host_data
