program test_chunked_data

  use, intrinsic :: iso_fortran_env, only: error_unit

  use data, only: nchunks, &
      chunksize, &
      chunk_begin, &
      chunk_end, &
      ncols, &
      nchunk
  use data, only: chunked_data_type, &
      chunked_data_instance
  
  use test_host_ccpp_cap, only: ccpp_register, &
      ccpp_init, &
      ccpp_physics_init, &
      ccpp_physics_timestep_init, &
      ccpp_physics_run, &
      ccpp_physics_timestep_final, &
      ccpp_physics_final, &
      ccpp_final

  implicit none

  character(len=*), parameter :: ccpp_suite = 'chunked_data_suite'
  integer :: ic, ierr
  integer :: lb, ub
  integer :: errflg
  character(len=512) :: errmsg

  call chunked_data_instance%create(ncols)

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

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! CCPP physics init step                         !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  call ccpp_physics_init(lb=1, ub=ncols, nthreads=1, nphys_threads=1, thread_num=1, suite_name=trim(ccpp_suite), group_name='all', errmsg=errmsg, errflg=errflg)
  if (errflg/=0) then
    write(error_unit, '(a)') "An error occurred in ccpp_physics_init:"
    write(error_unit, '(a)') trim(errmsg)
    stop 1
  end if

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! CCPP physics timestep init step                !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !cdata => ccpp_data_domain
  call ccpp_physics_timestep_init(lb=1, ub=ncols, nthreads=1, nphys_threads=1, thread_num=1, suite_name=trim(ccpp_suite), group_name='all', errmsg=errmsg, errflg=errflg)
  if (errflg/=0) then
    write(error_unit, '(a)') "An error occurred in ccpp_physics_timestep_init:"
    write(error_unit, '(a)') trim(errmsg)
    stop 1
  end if

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! CCPP physics run step                          !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  do nchunk = 1, nchunks
    lb=chunk_begin(nchunk)
    ub=chunk_end(nchunk)
    call ccpp_physics_run(lb=lb, ub=ub, nthreads=1, nphys_threads=1, thread_num=1, suite_name=trim(ccpp_suite), group_name='all', errmsg=errmsg, errflg=errflg)
    if (errflg/=0) then
      write(error_unit, '(a,i3,a)') "An error occurred in ccpp_physics_run for chunk", nchunk, ":"
      write(error_unit, '(a)') trim(errmsg)
      stop 1
    end if
  end do

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! CCPP physics timestep finalize step            !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !cdata => ccpp_data_domain
  call ccpp_physics_timestep_final(lb=1, ub=ncols, nthreads=1, nphys_threads=1, thread_num=1, suite_name=trim(ccpp_suite), group_name='all', errmsg=errmsg, errflg=errflg)
  if (errflg/=0) then
    write(error_unit, '(a)') "An error occurred in ccpp_physics_timestep_finalize:"
    write(error_unit, '(a)') trim(errmsg)
    stop 1
  end if

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! CCPP physics finalize step                     !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !cdata => ccpp_data_domain
  call ccpp_physics_final(lb=1, ub=ncols, nthreads=1, nphys_threads=1, thread_num=1, suite_name=trim(ccpp_suite), group_name='all', errmsg=errmsg, errflg=errflg)
  if (errflg/=0) then
    write(error_unit, '(a)') "An error occurred in ccpp_physics_finalize:"
    write(error_unit, '(a)') trim(errmsg)
    stop 1
  end if

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! CCPP final step                                !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  call ccpp_final(suite_name=trim(ccpp_suite), errmsg=errmsg, errflg=errflg)
  call chunked_data_instance%destroy()

end program test_chunked_data
