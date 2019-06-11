module example_ccpp_host_cap

  use ccpp_api,           only: ccpp_t, ccpp_init, ccpp_finalize
  use ccpp_static_api,    only: ccpp_physics_init, ccpp_physics_run,     &
                                ccpp_physics_finalize

  implicit none

  ! CCPP data structure
  type(ccpp_t), save, target :: cdata

  public :: physics_init, physics_run, physics_finalize

contains

  subroutine physics_init(ccpp_suite_name)
    character(len=*), intent(in) :: ccpp_suite_name
    integer :: ierr
    ierr = 0

    ! Initialize the CCPP framework, parse SDF
    call ccpp_init(trim(ccpp_suite_name), cdata, ierr=ierr)
    if (ierr/=0) then
      write(*,'(a)') "An error occurred in ccpp_init"
      stop
    end if

    ! Initialize CCPP physics (run all _init routines)
    call ccpp_physics_init(cdata, suite_name=trim(ccpp_suite_name),      &
                           ierr=ierr)
    ! error handling as above

  end subroutine physics_init
  
  subroutine physics_run(ccpp_suite_name, group)
    ! Optional argument group can be used to run a group of schemes      &
    ! defined in the SDF. Otherwise, run entire suite.
    character(len=*),           intent(in) :: ccpp_suite_name
    character(len=*), optional, intent(in) :: group

    integer :: ierr
    ierr = 0
    
    if (present(group)) then
       call ccpp_physics_run(cdata, suite_name=trim(ccpp_suite_name),    &
                             group_name=group, ierr=ierr)
    else
       call ccpp_physics_run(cdata, suite_name=trim(ccpp_suite_name),    &
                             ierr=ierr)
    end if
    ! error handling as above

  end subroutine physics_run
  
  subroutine physics_finalize(ccpp_suite_name)
    character(len=*), intent(in) :: ccpp_suite_name
    integer :: ierr
    ierr = 0

    ! Finalize CCPP physics (run all _finalize routines)
    call ccpp_physics_finalize(cdata, suite_name=trim(ccpp_suite_name),  &
                               ierr=ierr)
    ! error handling as above
    call ccpp_finalize(cdata, ierr=ierr)
    ! error handling as above

  end subroutine physics_finalize

end module example_ccpp_host_cap
