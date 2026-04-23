module test_host_data

  use ccpp_kinds, only: kind_phys

  implicit none

  !> \section arg_table_physics_state  Argument Table
  !! \htmlinclude arg_table_physics_state.html
  type physics_state
     real(kind_phys), allocatable :: ps(:)              ! surface pressure
     real(kind_phys), allocatable :: temp(:,:)          ! temperature
     real(kind_phys), dimension(:,:,:), pointer :: q => NULL() ! constituent array
  end type physics_state

  !> \section arg_table_test_host_data  Argument Table
  !! \htmlinclude arg_table_test_host_data.html
  integer, public, parameter :: num_consts = 3
  character(len=32), public, parameter :: std_name_array(num_consts) = (/     &
       'specific_humidity            ',                                       &
       'cloud_liquid_dry_mixing_ratio',                                       &
       'cloud_ice_dry_mixing_ratio   ' /)
  character(len=32), public, parameter :: const_std_name = std_name_array(1)

  integer            :: const_inds(num_consts) = -1 ! test array access from suite
  integer            :: const_index = -1            ! test scalar access from suite

  public :: allocate_physics_state
  public :: check_constituent_indices

contains

  subroutine check_constituent_indices(test_index, test_indices, errmsg, errflg)
     ! Check constituent indices against what was found by suite
     ! indices are passed in rather than looked up to avoid a dependency loop
      ! Dummy arguments
     integer,          intent(in)  :: test_index      ! scalar const index from host
     integer,          intent(in)  :: test_indices(:) ! array_test_indices from host
     character(len=*), intent(out) :: errmsg
     integer,          intent(out) :: errflg

     ! Local variable
     integer :: indx
     integer :: emstrt

     errflg = 0
     errmsg = ''
     if (test_index /= const_index) then
        emstrt = len_trim(errmsg) + 1
        write(errmsg(emstrt:), '(2a,i0,a,i0)') 'const_index_check for ',       &
             const_std_name, test_index, ' /= ', const_index
        errflg = errflg + 1
     end if
     do indx = 1, num_consts
        if (test_indices(indx) /= const_inds(indx)) then
           emstrt = len_trim(errmsg) + 1
           if (len_trim(errmsg) > 0) then
              write(errmsg(emstrt:), '(", ")')
              emstrt = emstrt + 2
           end if
           write(errmsg(emstrt:), '(2a,i0,a,i0)') 'const_indices_check for ', &
                std_name_array(indx), test_indices(indx), ' /= ', const_inds(indx)
           errflg = errflg + 1
        end if
     end do

     ! Reset for next test
     const_index = -1
     const_inds = -1

  end subroutine check_constituent_indices

  subroutine allocate_physics_state(cols, levels, constituents, state)
    integer,             intent(in)  :: cols
    integer,             intent(in)  :: levels
    real(kind_phys), pointer         :: constituents(:,:,:)
    type(physics_state), intent(out) :: state

    if (allocated(state%ps)) then
       deallocate(state%ps)
    end if
    allocate(state%ps(cols))
    state%ps = 0.0_kind_phys
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
