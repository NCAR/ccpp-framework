module test_prog

  use ccpp_kinds, only: kind_phys

  implicit none
  private

  public test_host

  ! Public data and interfaces
  integer, public, parameter :: cs = 16
  integer, public, parameter :: cm = 64

  !> \section arg_table_suite_info  Argument Table
  !! \htmlinclude arg_table_suite_info.html
  !!
  type, public :: suite_info
    character(len=cs) :: suite_name = ''
    character(len=cs), pointer :: suite_parts(:) => null()
    character(len=cm), pointer :: suite_input_vars(:) => null()
    character(len=cm), pointer :: suite_output_vars(:) => null()
    character(len=cm), pointer :: suite_required_vars(:) => null()
  end type suite_info

contains

  logical function check_suite(test_suite)
    use test_host_ccpp_cap, only: ccpp_physics_suite_part_list
    use test_host_ccpp_cap, only: ccpp_physics_suite_variables
    use test_utils, only: check_list

    ! Dummy argument
    type(suite_info), intent(in) :: test_suite
    ! Local variables
    integer :: sind
    logical :: check
    integer :: errflg
    character(len=512) :: errmsg
    character(len=128), allocatable :: test_list(:)

    check_suite = .true.
    write(6, *) "Checking suite ", trim(test_suite%suite_name)
    ! First, check the suite parts
    call ccpp_physics_suite_part_list(test_suite%suite_name, test_list, &
        errmsg, errflg)
    if (errflg == 0) then
      check = check_list(test_list, test_suite%suite_parts, 'part names', &
          suite_name=test_suite%suite_name)
    else
      check = .false.
      write(6, '(a,i0,2a)') 'ERROR ', errflg, ': ', trim(errmsg)
    end if
    check_suite = check_suite .and. check
    if (allocated(test_list)) then
      deallocate(test_list)
    end if
    ! Check the input variables
    call ccpp_physics_suite_variables(test_suite%suite_name, test_list, &
        errmsg, errflg, input_vars=.true., output_vars=.false.)
    if (errflg == 0) then
      check = check_list(test_list, test_suite%suite_input_vars, &
          'input variable names', suite_name=test_suite%suite_name)
    else
      check = .false.
      write(6, '(a,i0,2a)') 'ERROR ', errflg, ': ', trim(errmsg)
    end if
    check_suite = check_suite .and. check
    if (allocated(test_list)) then
      deallocate(test_list)
    end if
    ! Check the output variables
    call ccpp_physics_suite_variables(test_suite%suite_name, test_list, &
        errmsg, errflg, input_vars=.false., output_vars=.true.)
    if (errflg == 0) then
      check = check_list(test_list, test_suite%suite_output_vars, &
          'output variable names', suite_name=test_suite%suite_name)
    else
      check = .false.
      write(6, '(a,i0,2a)') 'ERROR ', errflg, ': ', trim(errmsg)
    end if
    check_suite = check_suite .and. check
    if (allocated(test_list)) then
      deallocate(test_list)
    end if
    ! Check all required variables
    call ccpp_physics_suite_variables(test_suite%suite_name, test_list, &
        errmsg, errflg)
    if (errflg == 0) then
      check = check_list(test_list, test_suite%suite_required_vars, &
          'required variable names', suite_name=test_suite%suite_name)
    else
      check = .false.
      write(6, '(a,i0,2a)') 'ERROR ', errflg, ': ', trim(errmsg)
    end if
    check_suite = check_suite .and. check
    if (allocated(test_list)) then
      deallocate(test_list)
    end if
  end function check_suite

  !> \section arg_table_test_host  Argument Table
  !! \htmlinclude arg_table_test_host.html
  !!
  subroutine test_host(retval, test_suites)

#ifdef _OPENMP
    use omp_lib
#endif
    use test_host_mod, only: ncols, &
        num_time_steps
    use test_host_ccpp_cap, only: ccpp_register
    use test_host_ccpp_cap, only: ccpp_init
    use test_host_ccpp_cap, only: ccpp_physics_init
    use test_host_ccpp_cap, only: ccpp_physics_timestep_init
    use test_host_ccpp_cap, only: ccpp_physics_run
    use test_host_ccpp_cap, only: ccpp_physics_timestep_final
    use test_host_ccpp_cap, only: ccpp_physics_final
    use test_host_ccpp_cap, only: ccpp_final
    use test_host_ccpp_cap, only: ccpp_physics_suite_list
    use test_host_mod, only: init_data, &
        compare_data, &
        check_model_times
    use test_utils, only: check_list

    type(suite_info), intent(in) :: test_suites(:)
    logical, intent(out) :: retval

    logical :: check
    integer :: col_start, col_end
    integer :: thread_num, num_threads
    integer :: index, sind
    integer :: time_step
    integer :: num_suites
    character(len=128), allocatable :: suite_names(:)
    character(len=512) :: errmsg
    integer :: errflg

    ! Initialize our 'data'
    call init_data()

    ! Gather and test the inspection routines
    num_suites = size(test_suites)
    call ccpp_physics_suite_list(suite_names)
    retval = check_list(suite_names, test_suites(:)%suite_name, &
        'suite names')
    write(6, *) 'Available suites are:'
    do index = 1, size(suite_names)
      do sind = 1, num_suites
        if (trim(test_suites(sind)%suite_name) == &
            trim(suite_names(index))) then
          exit
        end if
      end do
      write(6, '(i0,3a,i0,a)') index, ') ', trim(suite_names(index)), &
          ' = test_suites(', sind, ')'
    end do
    if (retval) then
      do sind = 1, num_suites
        check = check_suite(test_suites(sind))
        retval = retval .and. check
      end do
    end if
    !!! Return here if any check failed
    if (.not. retval) then
      return
    end if

    ! Use the suite information to call the register phase
    do sind = 1, num_suites
      call ccpp_register(suite_name=test_suites(sind)%suite_name, &
          errmsg=errmsg, errflg=errflg)
      if (errflg /= 0) then
        write(6, '(4a)') 'ERROR in ccpp_register for ', &
            trim(test_suites(sind)%suite_name), ': ', trim(errmsg)
      end if
    end do
    ! Call the CCPP init phase for each suite
    do sind = 1, num_suites
      call ccpp_init(suite_name=test_suites(sind)%suite_name, &
          errmsg=errmsg, errflg=errflg)
      if (errflg /= 0) then
        write(6, '(4a)') 'ERROR in ccpp_init for ', &
            trim(test_suites(sind)%suite_name), ': ', trim(errmsg)
      end if
    end do          
    ! Use the suite information to setup the run
    do sind = 1, num_suites
      call ccpp_physics_init( &
          suite_name=test_suites(sind)%suite_name, &
          errmsg=errmsg, errflg=errflg, &
          group_name='all', col_start=1, col_end=ncols, &
          thread_num=1, nthreads=1, nphys_threads=1)
      if (errflg /= 0) then
        write(6, '(4a)') 'ERROR in ccpp_physics_init for ', &
            trim(test_suites(sind)%suite_name), ': ', trim(errmsg)
      end if
    end do
    ! Loop over time steps
    do time_step = 1, num_time_steps
      ! Initialize the timestep
      do sind = 1, num_suites
        if (errflg /= 0) then
          exit
        end if
        if (errflg == 0) then
          call ccpp_physics_timestep_init( &
              suite_name=test_suites(sind)%suite_name, &
              errmsg=errmsg, errflg=errflg, &
              group_name='all', col_start=1, col_end=ncols, &
              thread_num=1, nthreads=1, nphys_threads=1)
        end if
        if (errflg /= 0) then
          write(6, '(3a)') trim(test_suites(sind)%suite_name), ': ', &
              trim(errmsg)
          exit
        end if
        if (errflg /= 0) then
          exit
        end if
      end do

      run_phase_if_no_error: if (errflg == 0) then
#ifdef _OPENMP
        num_threads = omp_get_max_threads()
#else
        num_threads = 1
#endif
        !$OMP parallel num_threads (num_threads) &
        !$OMP default (none) &
        !$OMP shared (num_threads, num_suites, test_suites) &
        !$OMP private (thread_num, col_start, col_end, errmsg) &
        !$OMP reduction (+:errflg)
#ifdef _OPENMP
        thread_num = omp_get_thread_num()
#else
        thread_num = 0
#endif
        !$OMP do
        do col_start = 1, ncols, 5
          if (errflg /= 0) then
            continue
          end if
          col_end = min(col_start + 4, ncols)
          do sind = 1, num_suites
            if (errflg /= 0) then
              continue
            end if
            do index = 1, size(test_suites(sind)%suite_parts)
              if (errflg /= 0) then
                continue
              end if
              write(0, '(a,i0,a,i0,5a,i0,a,i0)') 'Thread ', thread_num, '/', num_threads, &
                  ': calling run phase for suite ', trim(test_suites(sind)%suite_name), &
                  ' part ', trim(test_suites(sind)%suite_parts(index)), &
                  ' columns ', col_start, ':', col_end
              call ccpp_physics_run( &
                  suite_name=test_suites(sind)%suite_name, &
                  group_name=test_suites(sind)%suite_parts(index), &
                  col_start=col_start, col_end=col_end, &
                  errmsg=errmsg, errflg=errflg, &
                  thread_num=thread_num, nthreads=num_threads, &
                  nphys_threads=1)
              if (errflg /= 0) then
                write(6, '(5a)') trim(test_suites(sind)%suite_name), &
                    '/', trim(test_suites(sind)%suite_parts(index)), &
                    ': ', trim(errmsg)
              end if
            end do
          end do
        end do
        !$OMP end do
        !$OMP end parallel
      end if run_phase_if_no_error

      do sind = 1, num_suites
        if (errflg /= 0) then
          exit
        end if
        if (errflg == 0) then
          call ccpp_physics_timestep_final( &
              suite_name=test_suites(sind)%suite_name, &
              errmsg=errmsg, errflg=errflg, &
              group_name='all', col_start=1, col_end=ncols, &
              thread_num=1, nthreads=1, nphys_threads=1)
        end if
        if (errflg /= 0) then
          write(6, '(3a)') trim(test_suites(sind)%suite_name), ': ', &
              trim(errmsg)
          exit
        end if
      end do
    end do ! End time step loop

    do sind = 1, num_suites
      if (errflg /= 0) then
        exit
      end if
      if (errflg == 0) then
        call ccpp_physics_final( &
            suite_name=test_suites(sind)%suite_name, &
            errmsg=errmsg, errflg=errflg, &
            group_name='all', col_start=1, col_end=ncols, &
            thread_num=1, nthreads=1, nphys_threads=1)
      end if
      if (errflg /= 0) then
        write(6, '(3a)') test_suites(sind)%suite_name, ': ', &
            trim(errmsg)
        write(6, '(2a)') 'An error occurred in ccpp_physics_final, ', &
            'Exiting...'
        exit
      end if
    end do

    do sind = 1, num_suites
      if (errflg /= 0) then
        exit
      end if
      if (errflg == 0) then
        call ccpp_final(suite_name=test_suites(sind)%suite_name, &
            errmsg=errmsg, errflg=errflg)
      end if
      if (errflg /= 0) then
        write(6, '(3a)') test_suites(sind)%suite_name, ': ', &
            trim(errmsg)
        write(6, '(2a)') 'An error occurred in ccpp_final, ', &
            'Exiting...'
        exit
      end if
    end do

    if (errflg == 0) then
      ! Run finished without error, check answers
      if (.not. check_model_times()) then
        write(6, *) 'Model times error!'
        errflg = -1
      else if (compare_data()) then
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
