module example_ccpp_host_cap

  use ccpp_api, only: ccpp_t, ccpp_field_add, ccpp_init, ccpp_finalize, &
               ccpp_physics_init, ccpp_physics_run, ccpp_physics_finalize
  use iso_c_binding, only: c_loc
! Include auto-generated list of modules for ccpp
#include "ccpp_modules.inc"

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
    call ccpp_init(ccpp_suite_name, cdata, ierr=ierr)
    if (ierr/=0) then
      write(*,'(a)') "An error occurred in ccpp_init"
      stop
    end if
! Include auto-generated list of calls to ccpp_field_add
#include "ccpp_fields.inc"
    ! Initialize CCPP physics (run all _init routines)
    call ccpp_physics_init(cdata, ierr=ierr)
    ! error handling as above

  end subroutine physics_init

  subroutine physics_run(group, scheme)
    ! Optional arguments group and scheme can be used
    ! to run a group of schemes or an individual scheme
    ! defined in the SDF. Otherwise, run entire suite.
    character(len=*), optional, intent(in) :: group
    character(len=*), optional, intent(in) :: scheme

    integer :: ierr
    ierr = 0

    if (present(scheme)) then
       call ccpp_physics_run(cdata, scheme_name=scheme, ierr=ierr)
    else if (present(group)) then
       call ccpp_physics_run(cdata, group_name=group, ierr=ierr)
    else
       call ccpp_physics_run(cdata, ierr=ierr)
    end if
    ! error handling as above

  end subroutine physics_run

  subroutine physics_finalize()
    integer :: ierr
    ierr = 0

    ! Finalize CCPP physics (run all _finalize routines)
    call ccpp_physics_finalize(cdata, ierr=ierr)
    ! error handling as above
    call ccpp_finalize(cdata, ierr=ierr)
    ! error handling as above

  end subroutine physics_finalize

end module example_ccpp_host_cap
