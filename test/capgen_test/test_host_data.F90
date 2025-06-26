module test_host_data

  use ccpp_kinds, only: kind_phys

  implicit none
  private

  !> \section arg_table_physics_state  Argument Table
  !! \htmlinclude arg_table_physics_state.html
  type physics_state
     real(kind_phys), dimension(:), allocatable ::   &
          ps,                                        & ! surface pressure
          soil_levs                                    ! soil temperature (cm)
     real(kind_phys), dimension(:,:), allocatable :: &
          u,                                         & ! zonal wind (m/s)
          v,                                         & ! meridional wind (m/s)
          pmid                                         ! midpoint pressure (Pa)
     real(kind_phys), dimension(:,:,:),allocatable :: &
          q         ! constituent mixing ratio (kg/kg moist or dry air depending on type)
  end type physics_state

  public :: physics_state
  public :: allocate_physics_state

contains

  subroutine allocate_physics_state(cols, levels, constituents, lbnd_slev, ubnd_slev, state)
    integer,             intent(in)  :: cols
    integer,             intent(in)  :: levels
    integer,             intent(in)  :: constituents
    integer,             intent(in)  :: lbnd_slev, ubnd_slev
    type(physics_state), intent(out) :: state

    if (allocated(state%ps)) then
       deallocate(state%ps)
    end if
    allocate(state%ps(cols))
    if (allocated(state%u)) then
       deallocate(state%u)
    end if
    allocate(state%u(cols, levels))
    if (allocated(state%v)) then
       deallocate(state%v)
    end if
    allocate(state%v(cols, levels))
    if (allocated(state%pmid)) then
       deallocate(state%pmid)
    end if
    allocate(state%pmid(cols, levels))
    if (allocated(state%q)) then
       deallocate(state%q)
    end if
    allocate(state%q(cols, levels, constituents))
    if (allocated(state%soil_levs)) then
       deallocate(state%soil_levs)
    end if
    allocate(state%soil_levs(lbnd_slev:ubnd_slev))
    
  end subroutine allocate_physics_state
end module test_host_data
