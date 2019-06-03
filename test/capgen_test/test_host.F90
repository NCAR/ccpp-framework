
module test_prog

  use ccpp_kinds, only: kind_phys

  implicit none
  private

  public test_host

CONTAINS

  !> \section arg_table_test_host  Argument Table
  !! \htmlinclude arg_table_test_host.html
  !!
  subroutine test_host(retval)

    use test_host_data,     only: allocate_physics_state
    use test_host_mod,      only: ncols, pver, pcnst, phys_state, num_time_steps
    use test_host_ccpp_cap, only: test_host_ccpp_physics_initialize
    use test_host_ccpp_cap, only: test_host_ccpp_physics_timestep_initial
    use test_host_ccpp_cap, only: test_host_ccpp_physics_run
    use test_host_ccpp_cap, only: test_host_ccpp_physics_timestep_final
    use test_host_ccpp_cap, only: test_host_ccpp_physics_finalize
    use test_host_ccpp_cap, only: ccpp_physics_suite_list
    use test_host_ccpp_cap, only: ccpp_physics_suite_part_list
    use test_host_mod,      only: init_temp, compare_temp, check_model_times

    logical, intent(out)            :: retval

    integer                         :: col_start, col_end
    integer                         :: index
    integer                         :: time_step
    character(len=128), allocatable :: part_names(:)
    character(len=512)              :: errmsg
    integer                         :: errflg

    ! Initialize our 'data'
    call allocate_physics_state(ncols, pver, pcnst, phys_state)
    call init_temp()

    ! Use the suite information to setup the run
    call test_host_ccpp_physics_initialize('temp_suite', errmsg, errflg)
    if (errflg /= 0) then
      write(6, *) trim(errmsg)
    end if
    call test_host_ccpp_physics_initialize('ddt_suite', errmsg, errflg)
    if (errflg /= 0) then
      write(6, *) trim(errmsg)
    end if

    ! Loop over time steps
    do time_step = 1, num_time_steps
       ! Initialize the timestep
       if (errflg == 0) then
          call test_host_ccpp_physics_timestep_initial('temp_suite',          &
               errmsg, errflg)
          if (errflg /= 0) then
             write(6, *) trim(errmsg)
          end if
       end if
       if (errflg == 0) then
          call test_host_ccpp_physics_timestep_initial('ddt_suite',           &
               errmsg, errflg)
          if (errflg /= 0) then
             write(6, *) trim(errmsg)
          end if
       end if

       do col_start = 1, ncols, 5
          if (errflg /= 0) then
             continue
          end if
          col_end = MIN(col_start + 4, ncols)

          call test_host_ccpp_physics_run('temp_suite', 'physics',            &
               col_start, col_end, errmsg, errflg)
          if (errflg /= 0) then
             write(6, *) trim(errmsg)
             call ccpp_physics_suite_part_list('temp_suite', part_names,      &
                  errmsg, errflg)
             write(6, *) 'Available suite parts are:'
             do index = 1, size(part_names)
                write(6, *) trim(part_names(index))
             end do
          end if
          if (errflg == 0) then
             call test_host_ccpp_physics_run('ddt_suite', 'data_prep',        &
                  col_start, col_end, errmsg, errflg)
             if (errflg /= 0) then
                write(6, *) trim(errmsg)
                call ccpp_physics_suite_part_list('temp_suite', part_names,   &
                     errmsg, errflg)
                write(6, *) 'Available suite parts are:'
                do index = 1, size(part_names)
                   write(6, *) trim(part_names(index))
                end do
             end if
          end if
       end do

       if (errflg == 0) then
          call test_host_ccpp_physics_timestep_final('temp_suite',            &
               errmsg, errflg)
       end if
       if (errflg == 0) then
          call test_host_ccpp_physics_timestep_final('ddt_suite',             &
               errmsg, errflg)
       end if
    end do ! End time step loop

    if (errflg == 0) then
      call test_host_ccpp_physics_finalize('temp_suite', errmsg, errflg)
      if (errflg /= 0) then
        write(6, *) trim(errmsg)
        write(6,'(a)') 'An error occurred in temp ccpp_timestep_final, Exiting...'
      end if
    end if
    if (errflg == 0) then
      call test_host_ccpp_physics_finalize('ddt_suite', errmsg, errflg)
      if (errflg /= 0) then
        write(6, *) trim(errmsg)
        write(6,'(a)') 'An error occurred in ddt ccpp_timestep_final, Exiting...'
      end if
    end if

    if (errflg == 0) then
       ! Run finished without error, check answers
       if (.not. check_model_times()) then
          write(6, *) 'Model times error!'
          errflg = -1
       else if (compare_temp()) then
          write(6, *) 'Answers are correct!'
          errflg = 0
       else
          write(6, *) 'Answers are not correct!'
          errflg = -1
       end if
    end if

    retval = errflg == 0

  end subroutine test_host

end module test_prog

program test
  use test_prog, only: test_host

  logical :: run_okay

  call test_host(run_okay)

  if (run_okay) then
    STOP 0
  else
    STOP -1
  end if

end program test
