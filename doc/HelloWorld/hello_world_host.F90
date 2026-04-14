module hello_world_host

  use ccpp_kinds, only: kind_phys

  implicit none
  private

  public hello_world_sub

contains

  !> \section arg_table_hello_world_sub  Argument Table
  !! \htmlinclude arg_table_hello_world_sub.html
  !!
  subroutine hello_world_sub()

    use hello_world_mod, only: ncols
    use helloworld_ccpp_cap, only: helloworld_ccpp_physics_initialize
    use helloworld_ccpp_cap, only: helloworld_ccpp_physics_timestep_initial
    use helloworld_ccpp_cap, only: helloworld_ccpp_physics_run
    use helloworld_ccpp_cap, only: helloworld_ccpp_physics_timestep_final
    use helloworld_ccpp_cap, only: helloworld_ccpp_physics_finalize
    use helloworld_ccpp_cap, only: ccpp_physics_suite_list
    use helloworld_ccpp_cap, only: ccpp_physics_suite_part_list
    use hello_world_mod, only: init_temp, &
                               compare_temp

    integer :: col_start, col_end
    integer :: index
    character(len=128), allocatable :: part_names(:)
    character(len=512) :: errmsg
    integer :: errflg

    ! Initialize our 'data'
    call init_temp()

    ! Use the suite information to setup the run
    call helloworld_ccpp_physics_initialize('hello_world_suite', errmsg, errflg)
    if (errflg /= 0) then
      write(6, *) trim(errmsg)
      stop
    end if

    ! Initialize the timestep
    call helloworld_ccpp_physics_timestep_initial('hello_world_suite', errmsg, errflg)
    if (errflg /= 0) then
      write(6, *) trim(errmsg)
      stop
    end if

    do col_start = 1, ncols, 5
      col_end = min(col_start + 4, ncols)

      call helloworld_ccpp_physics_run('hello_world_suite', 'physics', col_start, col_end, errmsg, errflg)
      if (errflg /= 0) then
        write(6, *) trim(errmsg)
        call ccpp_physics_suite_part_list('hello_world_suite', part_names, errmsg, errflg)
        write(6, *) 'Available suite parts are:'
        do index = 1, size(part_names)
          write(6, *) trim(part_names(index))
        end do
        stop
      end if
    end do

    call helloworld_ccpp_physics_timestep_final('hello_world_suite', errmsg, errflg)

    call helloworld_ccpp_physics_finalize('hello_world_suite', errmsg, errflg)
    if (errflg /= 0) then
      write(6, *) trim(errmsg)
      write(6, '(a)') 'An error occurred in ccpp_timestep_final, Exiting...'
      stop
    end if

    if (compare_temp()) then
      write(6, *) 'Answers are correct!'
    else
      write(6, *) 'Answers are not correct!'
    end if

  end subroutine hello_world_sub

end module hello_world_host

program hello_world
  use hello_world_host, only: hello_world_sub
  call hello_world_sub()
end program hello_world
