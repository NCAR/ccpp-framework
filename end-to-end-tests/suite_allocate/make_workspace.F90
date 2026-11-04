!>\file make_workspace.F90
!! Producer scheme: allocates and fills a suite-owned scratch workspace.
!! The workspace standard name is not provided by the host, so capgen
!! promotes it to a suite-owned, scheme-allocated (allocatable=True) variable
!! stored in ccpp_<suite>_data. suite_data_init_fields skips its allocation;
!! this scheme owns it; final_fields frees it.

module make_workspace

  use ccpp_kinds, only: kind_phys

  implicit none

  private
  public :: make_workspace_run

contains

  !! \section arg_table_make_workspace_run Argument Table
  !! \htmlinclude make_workspace_run.html
  !!
  subroutine make_workspace_run(nw, work, errmsg, errflg)
    integer,                           intent(in)  :: nw
    real(kind=kind_phys), allocatable, intent(out) :: work(:)
    character(len=*),                  intent(out) :: errmsg
    integer,                           intent(out) :: errflg

    integer :: i

    errmsg = ''
    errflg = 0

    ! intent(out) on an allocatable dummy auto-deallocates on entry, so this is
    ! safe to call repeatedly (the persistent suite-data component is reset here).
    allocate(work(nw))
    do i = 1, nw
      work(i) = real(i, kind_phys)
    end do
  end subroutine make_workspace_run

end module make_workspace
