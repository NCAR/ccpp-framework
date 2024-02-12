module test_host_data

  use ccpp_kinds, only: kind_phys

  !> \section arg_table_physics_state  Argument Table
  !! \htmlinclude arg_table_physics_state.html
  type physics_state
     real(kind_phys), allocatable :: ps(:)              ! surface pressure
     real(kind_phys), allocatable :: temp(:,:)          ! temperature
     real(kind_phys), dimension(:,:,:), pointer :: q => NULL() ! constituent array
  end type physics_state

  public allocate_physics_state

contains

  subroutine allocate_physics_state(cols, levels, constituents, state)
    integer,             intent(in)  :: cols
    integer,             intent(in)  :: levels
    real(kind_phys), pointer         :: constituents(:,:,:)
    type(physics_state), intent(out) :: state

    if (allocated(state%ps)) then
       deallocate(state%ps)
    end if
    allocate(state%ps(cols))
    if (allocated(state%temp)) then
       deallocate(state%temp)
    end if
    allocate(state%temp(cols, levels))
    if (associated(state%q)) then
       ! Do not deallocate (we do not own this array)
       nullify(state%q)
    end if
    ! Point to the advected constituents array
    state%q => constituents

  end subroutine allocate_physics_state

end module test_host_data
