module test_prog

   use ccpp_kinds, only: kind_phys

   implicit none
   private

   public test_host

   ! Public data and interfaces
   integer, public, parameter :: cs = 16
   integer, public, parameter :: cm = 36

   type, public :: suite_info
      character(len=cs) :: suite_name = ''
      character(len=cs), pointer :: suite_parts(:) => NULL()
      character(len=cm), pointer :: suite_input_vars(:) => NULL()
      character(len=cm), pointer :: suite_output_vars(:) => NULL()
      character(len=cm), pointer :: suite_required_vars(:) => NULL()
   end type suite_info

   private :: check_list
   private :: check_suite
   private :: constituents_in     ! Data from suites to dycore array
   private :: constituents_out    ! Data from dycore array to suires
   private :: advect_constituents ! Move data around

CONTAINS

   logical function check_list(test_list, chk_list, list_desc, suite_name)
      ! Check a list (<test_list>) against its expected value (<chk_list>)

      ! Dummy arguments
      character(len=*),           intent(in) :: test_list(:)
      character(len=*),           intent(in) :: chk_list(:)
      character(len=*),           intent(in) :: list_desc
      character(len=*), optional, intent(in) :: suite_name

      ! Local variables
      logical                                :: found
      integer                                :: num_items
      integer                                :: lindex, tindex
      integer,          allocatable          :: check_unique(:)
      character(len=2)                       :: sep
      character(len=256)                     :: errmsg

      check_list = .true.
      errmsg = ''

      ! Check the list size
      num_items = size(chk_list)
      if (size(test_list) /= num_items) then
         write(errmsg, '(a,i0,2a)') 'ERROR: Found ', size(test_list),         &
              ' ', trim(list_desc)
         if (present(suite_name)) then
            write(errmsg(len_trim(errmsg)+1:), '(2a)') ' for suite, ',        &
                 trim(suite_name)
         end if
         write(errmsg(len_trim(errmsg)+1:), '(a,i0)') ', should be ', num_items
         write(6, *) trim(errmsg)
         errmsg = ''
         check_list = .false.
       end if

       ! Now, check the list contents for 1-1 correspondence
       if (check_list) then
          allocate(check_unique(num_items))
          check_unique = -1
          do lindex = 1, num_items
             found = .false.
             do tindex = 1, num_items
                if (trim(test_list(lindex)) == trim(chk_list(tindex))) then
                   check_unique(tindex) = lindex
                   found = .true.
                   exit
                end if
             end do
             if (.not. found) then
                check_list = .false.
                write(errmsg, '(5a)') 'ERROR: ', trim(list_desc), ' item, ',  &
                     trim(test_list(lindex)), ', was not found'
                if (present(suite_name)) then
                   write(errmsg(len_trim(errmsg)+1:), '(2a)') ' in suite, ',  &
                        trim(suite_name)
                end if
                write(6, *) trim(errmsg)
                errmsg = ''
             end if
          end do
          if (check_list .and. ANY(check_unique < 0)) then
             check_list = .false.
             write(errmsg, '(3a)') 'ERROR: The following ', trim(list_desc),  &
                  ' items were not found'
             if (present(suite_name)) then
                write(errmsg(len_trim(errmsg)+1:), '(2a)') ' in suite, ',     &
                     trim(suite_name)
             end if
             sep = '; '
             do lindex = 1, num_items
                if (check_unique(lindex) < 0) then
                   write(errmsg(len_trim(errmsg)+1:), '(2a)') sep,            &
                        trim(chk_list(lindex))
                   sep = ', '
                end if
             end do
             write(6, *) trim(errmsg)
             errmsg = ''
          end if
       end if

    end function check_list

    logical function check_suite(test_suite)
       use test_host_ccpp_cap, only: ccpp_physics_suite_part_list
       use test_host_ccpp_cap, only: ccpp_physics_suite_variables

       ! Dummy argument
       type(suite_info), intent(in)    :: test_suite
       ! Local variables
       integer                         :: sind
       logical                         :: check
       integer                         :: errflg
       character(len=512)              :: errmsg
       character(len=128), allocatable :: test_list(:)

       check_suite = .true.
       ! First, check the suite parts
       call ccpp_physics_suite_part_list(test_suite%suite_name, test_list,    &
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
       call ccpp_physics_suite_variables(test_suite%suite_name, test_list,    &
            errmsg, errflg, input_vars_in=.true., output_vars_in=.false.)
       if (errflg == 0) then
          check = check_list(test_list, test_suite%suite_input_vars,          &
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
       call ccpp_physics_suite_variables(test_suite%suite_name, test_list,    &
            errmsg, errflg, input_vars_in=.false., output_vars_in=.true.)
       if (errflg == 0) then
          check = check_list(test_list, test_suite%suite_output_vars,         &
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
       call ccpp_physics_suite_variables(test_suite%suite_name, test_list,    &
            errmsg, errflg)
       if (errflg == 0) then
          check = check_list(test_list, test_suite%suite_required_vars,       &
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

    logical function constituents_in(num_host_fields) result(okay)
       ! Copy advected species from physics to 'dynamics' array
       use test_host_constituents_mod, only: ccpp_constituents_layer
       use test_host_constituents_mod, only: ccpp_constituents_properties
       use test_host_mod,              only: phys_state, ncnst, index_qv

       ! Dummy argument
       integer, intent(in) :: num_host_fields ! Packed at beginning of Q
       ! Local variables
       integer :: index ! Index into CCPP constituents
       integer :: cind  ! Constituent index
       integer :: q_off ! Location in host advection array

       okay = .true.
       q_off = num_host_fields
       do index = 1, SIZE(ccpp_constituents_properties)
          if (ccpp_constituents_properties(index)%is_advected()) then
             cind = ccpp_constituent_properties(index)%field_index()
             ! Double check that we have the correct vertical dimension
             if (ccpp_constituent_properties(index)%vertical_dimension ==     &
                  'vertical_layer_dimension') then
                q_off = q_off + 1
                if (q_off > ncnst) then
                   okay = .false.
                   write(6, *) 'ERROR: Exceeded q bounds'
                   exit
                else
                   phys_state%q(:,:,q_off) = ccpp_constituents_layer(:,:,cind)
                end if
             else
                okay = .false.
                write(6, *) 'ERROR: ',                                        &
                     ccpp_constituents_properties(index)%standard_name(),     &
                     " has an unsupported vertical dimension, '",             &
                     ccpp_constituent_properties(index)%vertical_dimension,   &
                     "'"
                exit
             end if
          end if ! No else, just skip non-advected species
       end do
       if (q_off < ncnst) then
          okay = .false.
          write(6, '(2a,i0)') 'ERROR: Not all advected constituents found, ', &
               'missing, ' (ncnst - q_off)
       end if

    end function constituents_in

    logical function constituents_out(num_host_fields) result(okay)
       ! Copy advected constituents back to physics
       use test_host_constituents_mod, only: ccpp_constituents_layer
       use test_host_constituents_mod, only: ccpp_constituents_properties
       use test_host_mod,              only: phys_state, ncnst, index_qv

       ! Dummy argument
       integer, intent(in) :: num_host_fields ! Packed at beginning of Q
       ! Local variables
       integer :: index ! Index into CCPP constituents
       integer :: cind  ! Constituent index
       integer :: q_off ! Location in host advection array

       okay = .true.
       q_off = num_host_fields
       do index = 1, SIZE(ccpp_constituents_properties)
          if (ccpp_constituents_properties(index)%is_advected()) then
             cind = ccpp_constituent_properties(index)%field_index()
             ! Double check that we have the correct vertical dimension
             if (ccpp_constituent_properties(index)%vertical_dimension ==     &
                  'vertical_layer_dimension') then
                q_off = q_off + 1
                if (q_off > ncnst) then
                   okay = .false.
                   write(6, *) 'ERROR: Exceeded q bounds'
                   exit
                else
                   ccpp_constituents_layer(:,:,cind) = phys_state%q(:,:,q_off)
                end if
             else
                okay = .false.
                write(6, *) 'ERROR: ',                                        &
                     ccpp_constituents_properties(index)%standard_name(),     &
                     " has an unsupported vertical dimension, '",             &
                     ccpp_constituent_properties(index)%vertical_dimension,   &
                     "'"
                exit
             end if
          end if ! No else, just skip non-advected species
       end do
       if (q_off < ncnst) then
          okay = .false.
          write(6, '(2a,i0)') 'ERROR: Not all advected constituents found, ', &
               'missing, ' (ncnst - q_off)
       end if

    end function constituents_out

    subroutine advect_constituents()
       use test_host_mod, only: phys_state, ncnst, index_qv, ncols, pver

       ! Local variables
       integer         :: q_ind      ! Constituent index
       integer         :: icol, ilev ! Field coordinates
       integer         :: idir       ! 'w' sign
       integer         :: levb, leve ! Starting and ending level indices
       real(kind_phys) :: last_val, next_val

       do q_ind = 1, ncnst ! Skip checks, they were done in constituents_in
          idir = 1
          leve = (plev * mod(ncols, 2)) + mod(ncols-1, 2)
          last_val = phys_state%q(ncols, leve, q_ind)
          do icol = 1, ncols
             levb = ((plev * (1 - idir)) + (1 + idir)) / 2
             leve = ((plev * (1 + idir)) + (1 - idir)) / 2
             do ilev = levb, leve, idir
                next_val = phys_state%q(icol, ilev, q_ind)
                phys_state%q(icol, ilev, q_ind) = last_val
                last_val = next_val
             end do
             idir = idir * -1
          end do
       end do
    end subroutine advect_constituents

    !> \section arg_table_test_host  Argument Table
    !! \htmlinclude arg_table_test_host.html
    !!
    subroutine test_host(retval, test_suites)

       use test_host_mod,      only: ncols, num_time_steps, num_host_advected
       use test_host_mod,      only: init_data, compare_data, check_model_times
       use test_host_ccpp_cap, only: test_host_ccpp_physics_initialize
       use test_host_ccpp_cap, only: test_host_ccpp_physics_timestep_initial
       use test_host_ccpp_cap, only: test_host_ccpp_physics_run
       use test_host_ccpp_cap, only: test_host_ccpp_physics_timestep_final
       use test_host_ccpp_cap, only: test_host_ccpp_physics_finalize
       use test_host_ccpp_cap, only: ccpp_physics_suite_list
       use test_host_constituents_mod, only: ccpp_constituents_layer
       use test_host_constituents_mod, only: ccpp_constituents_properties

       type(suite_info), intent(in)  :: test_suites(:)
       logical,          intent(out) :: retval

       logical                         :: check
       integer                         :: col_start, col_end
       integer                         :: index, sind
       integer                         :: time_step
       integer                         :: num_suites
       integer                         :: num_consts   ! Total constituent count
       integer                         :: num_advected ! Num advected species
       character(len=128), allocatable :: suite_names(:)
       character(len=512)              :: errmsg
       integer                         :: errflg

       ! Register the constituents to find out what needs advecting
       call ccpp_register_constituents(test_suites, errmsg, errflg)
       num_consts = SIZE(ccpp_constituent_properties)
       num_advected = 0
       do index = 1, num_consts
          if (ccpp_constituents_properties(index)%is_advected()) then
             num_advected = num_advected + 1
          end if
       end do

       ! Initialize our 'data'
       call init_data(num_advected)

       ! Gather and test the inspection routines
       num_suites = size(test_suites)
       call ccpp_physics_suite_list(suite_names)
       retval = check_list(suite_names, test_suites(:)%suite_name,            &
            'suite names')
       write(6, *) 'Available suites are:'
       do index = 1, size(suite_names)
          do sind = 1, num_suites
             if (trim(test_suites(sind)%suite_name) ==                        &
                  trim(suite_names(index))) then
                exit
             end if
          end do
          write(6, '(i0,3a,i0,a)') index, ') ', trim(suite_names(index)),     &
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

       ! Use the suite information to setup the run
       do sind = 1, num_suites
          call test_host_ccpp_physics_initialize(test_suites(sind)%suite_name, &
               errmsg, errflg)
          if (errflg /= 0) then
             write(6, '(4a)') 'ERROR in initialize of ',                      &
                  trim(test_suites(sind)%suite_name), ': ', trim(errmsg)
          end if
       end do
       ! Loop over time steps
       do time_step = 1, num_time_steps
          ! Initialize the timestep
          do sind = 1, num_suites
             if (retval) then
                call test_host_ccpp_physics_timestep_initial(                 &
                     test_suites(sind)%suite_name, errmsg, errflg)
                if (errflg /= 0) then
                   write(6, '(3a)') trim(test_suites(sind)%suite_name), ': ', &
                        trim(errmsg)
                end if
             end if
          end do

          do col_start = 1, ncols, 5
             if (errflg /= 0) then
                continue
             end if
             col_end = MIN(col_start + 4, ncols)

             do sind = 1, num_suites
                do index = 1, size(test_suites(sind)%suite_parts)
                   call test_host_ccpp_physics_run(                           &
                        test_suites(sind)%suite_name,                         &
                        test_suites(sind)%suite_parts(index),                 &
                        col_start, col_end, errmsg, errflg)
                   if (errflg /= 0) then
                      write(6, '(5a)') trim(test_suites(sind)%suite_name),    &
                           '/', trim(test_suites(sind)%suite_parts(index)),   &
                           ': ', trim(errmsg)
                      exit
                   end if
                end do
             end do
          end do

          do sind = 1, num_suites
             if (errflg == 0) then
                call test_host_ccpp_physics_timestep_final(                   &
                     test_suites(sind)%suite_name, errmsg, errflg)
             end if
             if (errflg /= 0) then
                write(6, '(3a)') trim(test_suites(sind)%suite_name), ': ',    &
                     trim(errmsg)
             end if
          end do

          ! Run "dycore"
          if (errflg == 0) then
             errflg = constituents_in(num_host_advected)
          end if
          if (errflg == 0) then
             call advect_constituents()
          end if
          if (errflg == 0) then
             errflg = constituents_out(num_host_advected)
          end if
       end do ! End time step loop

       do sind = 1, num_suites
          if (errflg == 0) then
             call test_host_ccpp_physics_finalize(                            &
                  test_suites(sind)%suite_name, errmsg, errflg)
             if (errflg /= 0) then
                write(6, '(3a)') test_suites(sind)%suite_parts(index), ': ',  &
                     trim(errmsg)
                write(6,'(2a)') 'An error occurred in ccpp_timestep_final, ', &
                     'Exiting...'
             end if
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

 program test
    use test_prog, only: test_host, suite_info, cm, cs

    implicit none

   character(len=cs), target :: test_parts1(1) = (/ 'physics         ' /)
   character(len=cs), target :: test_parts2(1) = (/ 'data_prep       ' /)
   character(len=cm), target :: test_invars1(8) = (/                          &
        'horizontal_loop_extent              ',                               &
        'time_step_for_physics               ',                               &
        'minimum_temperature_for_cloud_liquid',                               &
        'temperature                         ',                               &
        'water_vapor_specific_humidity       ',                               &
        'surface_air_pressure                ',                               &
        'cloud_liquid_dry_mixing_ratio       ',                               &
        'cloud_ice_dry_mixing_ratio          ' /)
   character(len=cm), target :: test_outvars1(6) = (/                         &
        'ccpp_error_message                  ',                               &
        'ccpp_error_flag                     ',                               &
        'potential_temperature_at_interface  ',                               &
        'potential_temperature               ',                               &
        'cloud_liquid_dry_mixing_ratio       ',                               &
        'cloud_ice_dry_mixing_ratio          ' /)
   character(len=cm), target :: test_reqvars1(10) = (/                        &
        'horizontal_loop_extent              ',                               &
        'time_step_for_physics               ',                               &
        'minimum_temperature_for_cloud_liquid',                               &
        'temperature                         ',                               &
        'water_vapor_specific_humidity       ',                               &
        'surface_air_pressure                ',                               &
        'cloud_liquid_dry_mixing_ratio       ',                               &
        'cloud_ice_dry_mixing_ratio          ',                               &
        'ccpp_error_message                  ',                               &
        'ccpp_error_flag                     ' /)

    type(suite_info) :: test_suites(1)
    logical :: run_okay

    ! Setup expected test suite info
    test_suites(1)%suite_name = 'cld_suite'
	test_suites(1)%suite_parts => test_parts1
    test_suites(1)%suite_input_vars => test_invars1
    test_suites(1)%suite_output_vars => test_outvars1
    test_suites(1)%suite_required_vars => test_reqvars1

    call test_host(run_okay, test_suites)

    if (run_okay) then
       STOP 0
    else
       STOP -1
    end if

end program test
