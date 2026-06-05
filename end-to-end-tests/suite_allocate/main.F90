program test_suite_allocate

  use, intrinsic :: iso_fortran_env, only: output_unit, &
      error_unit

  use ccpp_kinds, only: kind_phys

  use data, only: checksum

  use test_host_ccpp_cap, only: ccpp_register, &
      ccpp_init, &
      ccpp_physics_init, &
      ccpp_physics_timestep_init, &
      ccpp_physics_run, &
      ccpp_physics_timestep_final, &
      ccpp_physics_final, &
      ccpp_final

  implicit none

  character(len=*), parameter :: ccpp_suite = 'suite_allocate_suite'
  character(len=512) :: errmsg
  integer :: errflg
  ! Must match the value use_workspace sets in its timestep_init phase.
  integer, parameter :: expected_size = 4
  real(kind=kind_phys) :: expected
  real(kind=kind_phys), parameter :: tol = 1.0e-6_kind_phys

  ! use_workspace sets workspace_dimension = expected_size in timestep_init;
  ! make_workspace fills work(i) = i in run, so the consumer's sum is N*(N+1)/2.
  expected = real(expected_size * (expected_size + 1) / 2, kind_phys)
  checksum = -1.0_kind_phys

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! CCPP register step                             !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  call ccpp_register(suite_name=trim(ccpp_suite), errmsg=errmsg, errflg=errflg)
  call check_err('ccpp_register', errflg, errmsg)

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! CCPP init step (suite_data_init_fields runs;   !
  ! it must SKIP the allocatable suite var)        !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  call ccpp_init(suite_name=trim(ccpp_suite), errmsg=errmsg, errflg=errflg)
  call check_err('ccpp_init', errflg, errmsg)

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! CCPP physics init / timestep init steps        !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  call ccpp_physics_init(lb=1, ub=1, nthreads=1, nphys_threads=1, thread_num=1, &
      suite_name=trim(ccpp_suite), group_name='all', errmsg=errmsg, errflg=errflg)
  call check_err('ccpp_physics_init', errflg, errmsg)

  call ccpp_physics_timestep_init(lb=1, ub=1, nthreads=1, nphys_threads=1, thread_num=1, &
      suite_name=trim(ccpp_suite), group_name='all', errmsg=errmsg, errflg=errflg)
  call check_err('ccpp_physics_timestep_init', errflg, errmsg)

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! CCPP physics run step: producer allocates the  !
  ! suite-owned workspace, consumer reduces it     !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  call ccpp_physics_run(lb=1, ub=1, nthreads=1, nphys_threads=1, thread_num=1, &
      suite_name=trim(ccpp_suite), group_name='all', errmsg=errmsg, errflg=errflg)
  call check_err('ccpp_physics_run', errflg, errmsg)

  if (abs(checksum - expected) > tol) then
    write(error_unit, '(a,f0.6,a,f0.6)') &
        "Error after ccpp_physics_run: workspace_checksum=", checksum, &
        " expected ", expected
    stop 1
  end if
  write(output_unit, '(a,f0.6)') &
      "PASS: After ccpp_physics_run: suite-owned allocatable workspace summed to ", checksum

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! CCPP physics timestep final / final steps      !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  call ccpp_physics_timestep_final(lb=1, ub=1, nthreads=1, nphys_threads=1, thread_num=1, &
      suite_name=trim(ccpp_suite), group_name='all', errmsg=errmsg, errflg=errflg)
  call check_err('ccpp_physics_timestep_final', errflg, errmsg)

  call ccpp_physics_final(lb=1, ub=1, nthreads=1, nphys_threads=1, thread_num=1, &
      suite_name=trim(ccpp_suite), group_name='all', errmsg=errmsg, errflg=errflg)
  call check_err('ccpp_physics_final', errflg, errmsg)

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! CCPP finalize step (final_fields frees the     !
  ! suite-owned allocatable var; guarded)          !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  call ccpp_final(suite_name=trim(ccpp_suite), errmsg=errmsg, errflg=errflg)
  call check_err('ccpp_final', errflg, errmsg)

  write(output_unit, '(a)') "PASS: suite_allocate test completed"

contains

  subroutine check_err(phase, errflg, errmsg)
    character(len=*), intent(in) :: phase
    integer,          intent(in) :: errflg
    character(len=*), intent(in) :: errmsg
    if (errflg /= 0) then
      write(error_unit, '(a)') "An error occurred in " // trim(phase) // ":"
      write(error_unit, '(a)') trim(errmsg)
      stop 1
    end if
  end subroutine check_err

end program test_suite_allocate
