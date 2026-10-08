!>\file use_workspace.F90
!! Consumer scheme: reads the suite-owned scratch workspace allocated by
!! make_workspace and reduces it into a host-owned scalar. Receiving the
!! suite-owned allocatable component through a plain (non-allocatable) dummy
!! exercises capgen passing the whole allocated component to a consumer.

module use_workspace

  use ccpp_kinds, only: kind_phys

  implicit none

  private
  public :: use_workspace_timestep_init, use_workspace_run

contains

  !! \section arg_table_use_workspace_timestep_init Argument Table
  !! \htmlinclude use_workspace_timestep_init.html
  !!
  subroutine use_workspace_timestep_init(nw, errmsg, errflg)
    integer,          intent(out) :: nw
    character(len=*), intent(out) :: errmsg
    integer,          intent(out) :: errflg

    errmsg = ''
    errflg = 0

    ! Own the suite workspace dimension here, in timestep_init -- a phase that
    ! runs AFTER ccpp_init/suite_data_init_fields. make_workspace (listed earlier
    ! but executed in the later run phase) allocates work(nw) using this value,
    ! which is exactly why scratch_workspace_field must be allocatable=True:
    ! init_fields cannot size it.
    nw = 4
  end subroutine use_workspace_timestep_init

  !! \section arg_table_use_workspace_run Argument Table
  !! \htmlinclude use_workspace_run.html
  !!
  subroutine use_workspace_run(work, checksum, errmsg, errflg)
    real(kind=kind_phys), intent(in)  :: work(:)
    real(kind=kind_phys), intent(out) :: checksum
    character(len=*),     intent(out) :: errmsg
    integer,              intent(out) :: errflg

    errmsg = ''
    errflg = 0

    checksum = sum(work)
  end subroutine use_workspace_run

end module use_workspace
