program test_opt_arg

  use, intrinsic :: iso_fortran_env, only: output_unit, &
      error_unit

  use data, only: nx, &
      flag_for_opt_arg, &
      std_arg, &
      opt_arg, &
      opt_arg_2

  use test_host_ccpp_cap, only: ccpp_register, &
      ccpp_init, &
      ccpp_physics_init, &
      ccpp_physics_timestep_init, &
      ccpp_physics_run, &
      ccpp_physics_timestep_final, &
      ccpp_physics_final, &
      ccpp_final

  implicit none

  character(len=*), parameter :: ccpp_suite = 'opt_arg_suite'
  character(len=512) :: errmsg
  integer :: errflg

  std_arg = 1
  flag_for_opt_arg = .true.
  allocate(opt_arg(nx))
  allocate(opt_arg_2(nx))
  ! capgen does not default-initialize host data; the host must.  Zero these
  ! so the post-ccpp_init checks below test real wiring, not stale memory.
  opt_arg = 0
  opt_arg_2 = 0

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! CCPP register step                             !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  call ccpp_register(suite_name=trim(ccpp_suite), errmsg=errmsg, errflg=errflg)
  if (errflg/=0) then
    write(error_unit, '(a)') "An error occurred in ccpp_register:"
    write(error_unit, '(a)') trim(errmsg)
    stop 1
  end if

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! CCPP init step                                 !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  call ccpp_init(suite_name=trim(ccpp_suite), errmsg=errmsg, errflg=errflg)
  if (errflg/=0) then
    write(error_unit, '(a)') "An error occurred in ccpp_init:"
    write(error_unit, '(a)') trim(errmsg)
    stop 1
  end if

  ! std_arg must all be 1, opt_arg must all be 0
  write(output_unit, '(a)') "After ccpp_init: check std_arg(:)==1, opt_arg(:)==0, opt_arg_2(:)==0"
  if (.not. all(std_arg == 1)) write(error_unit, '(a,3i3)') "Error after ccpp_init: std_arg=", std_arg
  if (.not. all(opt_arg == 0)) write(error_unit, '(a,3i3)') "Error after ccpp_init: opt_arg=", opt_arg
  if (.not. all(opt_arg_2 == 0)) write(error_unit, '(a,3es13.5)') "Error after ccpp_init: opt_arg_2=", opt_arg_2

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! CCPP physics init step                         !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  call ccpp_physics_init(lb=1, ub=1, nthreads=1, nphys_threads=1, thread_num=1, suite_name=trim(ccpp_suite), group_name='all', errmsg=errmsg, errflg=errflg)
  if (errflg/=0) then
    write(error_unit, '(a)') "An error occurred in ccpp_physics_init:"
    write(error_unit, '(a)') trim(errmsg)
    stop 1
  end if

  ! std_arg must all be 1, opt_arg must all be 0
  write(output_unit, '(a)') "PASS: After ccpp_physics_init:           check std_arg(:)==1 and opt_arg(:)==0"
  if (.not. all(std_arg == 1)) write(error_unit, '(a,3i3)') "Error after ccpp_physics_init: std_arg=", std_arg
  if (.not. all(opt_arg == 0)) write(error_unit, '(a,3i3)') "Error after ccpp_physics_init: opt_arg=", opt_arg

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! CCPP physics timestep init step                !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  call ccpp_physics_timestep_init(lb=1, ub=1, nthreads=1, nphys_threads=1, thread_num=1, suite_name=trim(ccpp_suite), group_name='all', errmsg=errmsg, errflg=errflg)
  if (errflg/=0) then
    write(error_unit, '(a)') "An error occurred in ccpp_physics_timestep_init:"
    write(error_unit, '(a)') trim(errmsg)
    stop 1
  end if

  ! std_arg must all be 1, opt_arg must all be 2
  write(output_unit, '(a)') "PASS: After ccpp_physics_timestep_init:  check std_arg(:)==1 and opt_arg(:)==2"
  if (.not. all(std_arg == 1)) write(error_unit, '(a,3i3)') "Error after ccpp_physics_timestep_init: std_arg=", std_arg
  if (.not. all(opt_arg == 2)) write(error_unit, '(a,3i3)') "Error after ccpp_physics_timestep_init: opt_arg=", opt_arg

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! CCPP physics run step                          !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  call ccpp_physics_run(lb=1, ub=1, nthreads=1, nphys_threads=1, thread_num=1, suite_name=trim(ccpp_suite), group_name='all', errmsg=errmsg, errflg=errflg)
  if (errflg/=0) then
    write(error_unit, '(a)') "An error occurred in ccpp_physics_run:"
    write(error_unit, '(a)') trim(errmsg)
    stop 1
  end if

  ! std_arg must all be 1, opt_arg must all be 3
  write(output_unit, '(a)') "PASS: After ccpp_physics_run:            check std_arg(:)==1 and opt_arg(:)==3"
  if (.not. all(std_arg == 1)) write(error_unit, '(a,3i3)') "Error after ccpp_physics_run: std_arg=", std_arg
  if (.not. all(opt_arg == 3)) write(error_unit, '(a,3i3)') "Error after ccpp_physics_run: opt_arg=", opt_arg

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! CCPP physics timestep finalize step            !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  deallocate(opt_arg)
  flag_for_opt_arg = .false.

  call ccpp_physics_timestep_final(lb=1, ub=1, nthreads=1, nphys_threads=1, thread_num=1, suite_name=trim(ccpp_suite), group_name='all', errmsg=errmsg, errflg=errflg)
  if (errflg/=0) then
    write(error_unit, '(a)') "An error occurred in ccpp_physics_timestep_final:"
    write(error_unit, '(a)') trim(errmsg)
    stop 1
  end if

  ! std_arg must all be 7, opt_arg no longer allocated
  write(output_unit, '(a)') "PASS: After ccpp_physics_timestep_final: check std_arg(:)==7; opt_arg not allocated"
  if (.not. all(std_arg == 7)) write(error_unit, '(a,3i3)') "Error after ccpp_physics_timestep_final: std_arg=", std_arg

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! CCPP physics finalize step                     !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  call ccpp_physics_final(lb=1, ub=1, nthreads=1, nphys_threads=1, thread_num=1, suite_name=trim(ccpp_suite), group_name='all', errmsg=errmsg, errflg=errflg)
  if (errflg/=0) then
    write(error_unit, '(a)') "An error occurred in ccpp_physics_final:"
    write(error_unit, '(a)') trim(errmsg)
    stop 1
  end if

  ! std_arg must all be 7, opt_arg no longer allocated
  write(output_unit, '(a)') "PASS: After ccpp_physics_final:          check std_arg(:)==7; opt_arg not allocated"
  if (.not. all(std_arg == 7)) write(error_unit, '(a,3i3)') "Error after ccpp_physics_final: std_arg=", std_arg

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! CCPP finalize step                             !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  call ccpp_final(suite_name=trim(ccpp_suite), errmsg=errmsg, errflg=errflg)
  if (errflg/=0) then
    write(error_unit, '(a)') "An error occurred in ccpp_final:"
    write(error_unit, '(a)') trim(errmsg)
    stop 1
  end if

end program test_opt_arg
