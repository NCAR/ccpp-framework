program test_instances_advection

  use, intrinsic :: iso_fortran_env, only: error_unit
#ifdef _OPENMP
  use omp_lib
#endif

  use ccpp_kinds, only: kind_phys
  use ccpp_constituent_prop_mod, only: ccpp_constituent_properties_t

  use data, only: ncols, pver, ninstances, dt, tfreeze, num_time_steps
  use data, only: phys_state, qv_init, index_qv
  use data, only: allocate_physics_state, init_qv, set_index_qv, &
      verify_results

  use test_host_ccpp_cap, only: ccpp_register, ccpp_init, ccpp_final
  use test_host_ccpp_cap, only: ccpp_physics_init, ccpp_physics_run, &
      ccpp_physics_timestep_init, ccpp_physics_timestep_final, &
      ccpp_physics_final
  use test_host_ccpp_cap, only: ccpp_register_constituents, &
      ccpp_initialize_constituents, ccpp_deallocate_dynamic_constituents
  use test_host_ccpp_cap, only: ccpp_const_get_index, ccpp_constituents_array
  use test_host_ccpp_cap, only: ccpp_number_constituents

  implicit none

  character(len=*), parameter :: ccpp_suite = 'cld_suite'
  character(len=512) :: errmsg
  integer :: errcode
  integer :: nphys_threads
  integer :: ins
  integer :: tstep
  integer :: num_consts
  integer :: idx
  type(ccpp_constituent_properties_t), target, allocatable :: host_consts(:)
  real(kind=kind_phys), pointer :: constituents_ptr(:, :, :)

  ! Use OpenMP threading in physics (internally) where available.
#ifdef _OPENMP
  nphys_threads = omp_get_max_threads()
#else
  nphys_threads = 1
#endif

  !-----------------------------------------------------------------
  ! 1. CCPP register: per-instance, populates per-suite dynamic
  !    constituent buffer on first instance, idempotent thereafter.
  !-----------------------------------------------------------------
  do ins = 1, ninstances
    call ccpp_register(suite_name=ccpp_suite, &
        instance=ins, ninstances=ninstances, &
        errmsg=errmsg, errcode=errcode)
    if (errcode /= 0) then
      write(error_unit, '(2a)') 'ccpp_register failed: ', trim(errmsg)
      stop 1
    end if
  end do

  !-----------------------------------------------------------------
  ! 2. Build the host-registered constituent list (specific_humidity)
  !    and call ccpp_register_constituents per instance.  Each call
  !    consumes the host_consts array (new_field sets const_index on
  !    the input objects), so we re-instantiate per instance.
  !-----------------------------------------------------------------
  do ins = 1, ninstances
    if (allocated(host_consts)) deallocate(host_consts)
    allocate(host_consts(1))
    call host_consts(1)%instantiate( &
        std_name='water_vapor_specific_humidity', &
        long_name='Water vapor specific humidity', &
        diag_name='QV', units='kg kg-1', &
        vertical_dim='vertical_layer_dimension', advected=.true., &
        default_value=0.0_kind_phys, mixing_ratio_type='wet', &
        errcode=errcode, errmsg=errmsg)
    if (errcode /= 0) then
      write(error_unit, '(2a)') 'instantiate failed: ', trim(errmsg)
      stop 1
    end if
    call ccpp_register_constituents(host_constituents=host_consts, &
        instance=ins, ninstances=ninstances, &
        errmsg=errmsg, errcode=errcode)
    if (errcode /= 0) then
      write(error_unit, '(a,i0,2a)') &
          'ccpp_register_constituents failed for instance ', ins, &
          ': ', trim(errmsg)
      stop 1
    end if
  end do

  !-----------------------------------------------------------------
  ! 3. ccpp_initialize_constituents per instance — allocates the
  !    per-instance constituent storage.
  !-----------------------------------------------------------------
  do ins = 1, ninstances
    call ccpp_initialize_constituents(ncols=ncols, num_layers=pver, &
        instance=ins, errmsg=errmsg, errcode=errcode)
    if (errcode /= 0) then
      write(error_unit, '(a,i0,2a)') &
          'ccpp_initialize_constituents failed for instance ', ins, &
          ': ', trim(errmsg)
      stop 1
    end if
  end do

  !-----------------------------------------------------------------
  ! 4. Resolve the qv constituent index and number of constituents
  !    (identical across instances).
  !-----------------------------------------------------------------

  call ccpp_const_get_index(stdname='water_vapor_specific_humidity', &
      const_index=idx, instance=1, errcode=errcode, errmsg=errmsg)
  if (errcode /= 0) then
    write(error_unit, '(2a)') 'ccpp_const_get_index(qv) failed: ', &
        trim(errmsg)
    stop 1
  end if
  call set_index_qv(idx)

  call ccpp_number_constituents(num_flds=num_consts, instance=1, &
      errcode=errcode, errmsg=errmsg)
  if (errcode /= 0) then
    write(error_unit, '(2a)') 'ccpp_number_constituents failed: ', &
        trim(errmsg)
    stop 1
  end if

  !-----------------------------------------------------------------
  ! 5. For each instance, wire phys_state(ins) to its constituent
  !    storage and seed distinct initial qv values.
  !-----------------------------------------------------------------

  do ins = 1, ninstances
    constituents_ptr => ccpp_constituents_array(ins)
    call allocate_physics_state(ins, constituents_ptr)
    call init_qv(ins)
  end do

  !-----------------------------------------------------------------
  ! 6. ccpp_init and ccpp_physics_init per instance.
  !-----------------------------------------------------------------
  do ins = 1, ninstances
    call ccpp_init(suite_name=ccpp_suite, &
        instance=ins, ninstances=ninstances, &
        errmsg=errmsg, errcode=errcode)
    if (errcode /= 0) then
      write(error_unit, '(a,i0,2a)') 'ccpp_init failed for instance ', &
          ins, ': ', trim(errmsg)
      stop 1
    end if
  end do
  do ins = 1, ninstances
    call ccpp_physics_init(suite_name=ccpp_suite, group_name='physics', &
        lb=1, ub=ncols, thread_num=1, nthreads=1, &
        nphys_threads=nphys_threads, &
        instance=ins, ninstances=ninstances, &
        errmsg=errmsg, errcode=errcode)
    if (errcode /= 0) then
      write(error_unit, '(a,i0,2a)') &
          'ccpp_physics_init failed for instance ', ins, ': ', trim(errmsg)
      stop 1
    end if
  end do

  !-----------------------------------------------------------------
  ! 7. Timestep loop.
  !-----------------------------------------------------------------
  do tstep = 1, num_time_steps
    do ins = 1, ninstances
      call ccpp_physics_timestep_init(suite_name=ccpp_suite, &
          group_name='physics', lb=1, ub=ncols, &
          thread_num=1, nthreads=1, nphys_threads=nphys_threads, &
          instance=ins, ninstances=ninstances, &
          errmsg=errmsg, errcode=errcode)
      if (errcode /= 0) then
        write(error_unit, '(a,i0,2a)') &
            'ccpp_physics_timestep_init failed for instance ', ins, &
            ': ', trim(errmsg)
        stop 1
      end if
    end do
    do ins = 1, ninstances
      call ccpp_physics_run(suite_name=ccpp_suite, group_name='physics', &
          lb=1, ub=ncols, thread_num=1, nthreads=1, &
          nphys_threads=nphys_threads, &
          instance=ins, ninstances=ninstances, &
          errmsg=errmsg, errcode=errcode)
      if (errcode /= 0) then
        write(error_unit, '(a,i0,2a)') &
            'ccpp_physics_run failed for instance ', ins, ': ', trim(errmsg)
        stop 1
      end if
    end do
    do ins = 1, ninstances
      call ccpp_physics_timestep_final(suite_name=ccpp_suite, &
          group_name='physics', lb=1, ub=ncols, &
          thread_num=1, nthreads=1, nphys_threads=nphys_threads, &
          instance=ins, ninstances=ninstances, &
          errmsg=errmsg, errcode=errcode)
      if (errcode /= 0) then
        write(error_unit, '(a,i0,2a)') &
            'ccpp_physics_timestep_final failed for instance ', ins, &
            ': ', trim(errmsg)
        stop 1
      end if
    end do
  end do

  !-----------------------------------------------------------------
  ! 8. ccpp_physics_final per instance.
  !-----------------------------------------------------------------
  do ins = 1, ninstances
    call ccpp_physics_final(suite_name=ccpp_suite, group_name='physics', &
        lb=1, ub=ncols, thread_num=1, nthreads=1, &
        nphys_threads=nphys_threads, &
        instance=ins, ninstances=ninstances, &
        errmsg=errmsg, errcode=errcode)
    if (errcode /= 0) then
      write(error_unit, '(a,i0,2a)') &
          'ccpp_physics_final failed for instance ', ins, ': ', trim(errmsg)
      stop 1
    end if
  end do

  !-----------------------------------------------------------------
  ! 9. Verify per-instance results BEFORE constituent teardown.
  !    phys_state(:)%q points into the framework's per-instance
  !    constituent storage; ccpp_deallocate_dynamic_constituents
  !    frees that storage, so any verification must run first.
  !-----------------------------------------------------------------
  if (.not. verify_results(num_consts)) then
    write(6, '(a)') 'FAIL: per-instance + constituents test'
    stop 1
  end if

  !-----------------------------------------------------------------
  ! 10. Teardown.
  !-----------------------------------------------------------------
  do ins = 1, ninstances
    call ccpp_final(suite_name=ccpp_suite, &
        instance=ins, ninstances=ninstances, &
        errmsg=errmsg, errcode=errcode)
    if (errcode /= 0) then
      write(error_unit, '(a,i0,2a)') 'ccpp_final failed for instance ', &
          ins, ': ', trim(errmsg)
      stop 1
    end if
  end do
  do ins = 1, ninstances
    call ccpp_deallocate_dynamic_constituents(instance=ins)
  end do
  deallocate(host_consts)

  write(6, '(a)') 'PASS: per-instance + constituents test'
  stop 0

end program test_instances_advection
