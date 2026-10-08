program test_unit_conv

  use, intrinsic :: iso_fortran_env, only: error_unit
#ifdef _OPENMP
  use omp_lib
#endif

  use data, only: ncols, &
      nspecies, ninstances
  use data, only: instance_data

  use test_host_ccpp_cap, only: ccpp_register, &
      ccpp_init, &
      ccpp_physics_init, &
      ccpp_physics_timestep_init, &
      ccpp_physics_run, &
      ccpp_physics_timestep_final, &
      ccpp_physics_final, &
      ccpp_final

  implicit none

  character(len=*), parameter :: ccpp_suite = 'unit_conv_suite'
  ! An updated ccpp_validator.py should detect this - metadata has len=512
  character(len=256) :: errmsg
  integer :: errflg
  integer :: nphys_threads
  integer :: ins

  !data_array = 1.0_8
  !data_array2 = 42.0_8
  !opt_array_flag = .true.

  instance_data(1)%data_array = -1.0_8
  instance_data(1)%data_array2 = -42.0_8
  instance_data(1)%opt_array_flag = .true.

  instance_data(2)%data_array = +1.0_8
  instance_data(2)%data_array2 = +42.0_8
  instance_data(2)%opt_array_flag = .false.

  ! Use OpenMP threading in physics (internally)
#ifdef _OPENMP
  nphys_threads = omp_get_max_threads()
#else
  nphys_threads = 1
#endif

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! CCPP register step                             !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  do ins=1,ninstances
    call ccpp_register(suite_name=ccpp_suite, &
        instance=ins, ninstances=ninstances, &
        errmsg=errmsg, errflg=errflg)
    if (errflg/=0) then
      write(error_unit, '(a)') "An error occurred in ccpp_register:"
      write(error_unit, '(a)') trim(errmsg)
      stop 1
    end if
  end do

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! CCPP init step                                 !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  do ins=1,ninstances
    call ccpp_init(suite_name=ccpp_suite, &
        instance=ins, ninstances=ninstances, &
        errmsg=errmsg, errflg=errflg)
    if (errflg/=0) then
      write(error_unit, '(a)') "An error occurred in ccpp_init:"
      write(error_unit, '(a)') trim(errmsg)
      write(error_unit, '(a,i0)') "instance: ", ins
      stop 1
    end if
  end do

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! CCPP physics init step                         !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  do ins=1,ninstances
    call ccpp_physics_init( &
        suite_name=ccpp_suite, group_name='all', &
        instance=ins, ninstances=ninstances, &
        thread_num=1, nthreads=1, nphys_threads=nphys_threads, &
        lb=1, ub=ncols, errmsg=errmsg, errflg=errflg)
    if (errflg/=0) then
      write(error_unit, '(a)') "An error occurred in ccpp_physics_init:"
      write(error_unit, '(a)') trim(errmsg)
      write(error_unit, '(a,i0)') "instance: ", ins
      stop 1
    end if
  end do

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! CCPP physics timestep init step                !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  do ins=1,ninstances
    call ccpp_physics_timestep_init( &
        suite_name=ccpp_suite, group_name='all', &
        instance=ins, ninstances=ninstances, &
        thread_num=1, nthreads=1, nphys_threads=nphys_threads, &
        lb=1, ub=ncols, errmsg=errmsg, errflg=errflg)
    if (errflg/=0) then
      write(error_unit, '(a)') "An error occurred in ccpp_physics_timestep_init:"
      write(error_unit, '(a)') trim(errmsg)
      write(error_unit, '(a,i0)') "instance: ", ins
      stop 1
    end if
  end do
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! CCPP physics run step                          !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  do ins=1,ninstances
    call ccpp_physics_run( &
        suite_name=ccpp_suite, group_name='all', &
        instance=ins, ninstances=ninstances, &
        thread_num=1, nthreads=1, nphys_threads=nphys_threads, &
        lb=1, ub=ncols, errmsg=errmsg, errflg=errflg)
    if (errflg/=0) then
      write(error_unit, '(a)') "An error occurred in ccpp_physics_run:"
      write(error_unit, '(a)') trim(errmsg)
      write(error_unit, '(a,i0)') "instance: ", ins
      stop 1
    end if
  end do

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! CCPP physics timestep final step               !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  do ins=1,ninstances
    call ccpp_physics_timestep_final( &
        suite_name=ccpp_suite, group_name='all', &
        instance=ins, ninstances=ninstances, &
        thread_num=1, nthreads=1, nphys_threads=nphys_threads, &
        lb=1, ub=ncols, errmsg=errmsg, errflg=errflg)
    if (errflg/=0) then
      write(error_unit, '(a)') "An error occurred in ccpp_physics_timestep_final:"
      write(error_unit, '(a)') trim(errmsg)
      write(error_unit, '(a,i0)') "instance: ", ins
      stop 1
    end if
  end do

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! CCPP physics final step                        !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  do ins=1,ninstances
    call ccpp_physics_final( &
        suite_name=ccpp_suite, group_name='all', &
        instance=ins, ninstances=ninstances, &
        thread_num=1, nthreads=1, nphys_threads=nphys_threads, &
        lb=1, ub=ncols, errmsg=errmsg, errflg=errflg)
    if (errflg/=0) then
      write(error_unit, '(a)') "An error occurred in ccpp_physics_final:"
      write(error_unit, '(a)') trim(errmsg)
      write(error_unit, '(a,i0)') "instance: ", ins
      stop 1
    end if
  end do
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! CCPP final step                                !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  do ins=1,ninstances
    call ccpp_final(suite_name=ccpp_suite, &
        instance=ins, ninstances=ninstances, &
        errmsg=errmsg, errflg=errflg)
    if (errflg/=0) then
      write(error_unit, '(a)') "An error occurred in ccpp_final:"
      write(error_unit, '(a)') trim(errmsg)
      write(error_unit, '(a,i0)') "instance: ", ins
      stop 1
    end if
  end do

end program test_unit_conv
