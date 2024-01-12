module test_prog

   use ccpp_kinds,                only: kind_phys
   use ccpp_constituent_prop_mod, only: ccpp_constituent_properties_t

   implicit none
   private

   public test_host

   ! Public data and interfaces
   integer, public, parameter :: cs = 16
   integer, public, parameter :: cm = 36

   !> \section arg_table_suite_info  Argument Table
   !! \htmlinclude arg_table_suite_info.html
   !!
   type, public :: suite_info
      character(len=cs) :: suite_name = ''
      character(len=cs), pointer :: suite_parts(:) => NULL()
      character(len=cm), pointer :: suite_input_vars(:) => NULL()
      character(len=cm), pointer :: suite_output_vars(:) => NULL()
      character(len=cm), pointer :: suite_required_vars(:) => NULL()
   end type suite_info

   type(ccpp_constituent_properties_t), private, target :: host_constituents(1)


   private :: check_list
   private :: check_suite
   private :: advect_constituents ! Move data around
   private :: check_errflg

CONTAINS

   subroutine check_errflg(subname, errflg, errmsg, errflg_final)
      ! If errflg is not zero, print an error message
      character(len=*), intent(in)    :: subname
      integer,          intent(in)    :: errflg
      character(len=*), intent(in)    :: errmsg

      integer,          intent(out)   :: errflg_final

      if (errflg /= 0) then
         write(6, '(a,i0,4a)') "Error ", errflg, " from ", trim(subname),     &
              ':', trim(errmsg)
         !Notify test script that a failure occurred:
         errflg_final = -1 !Notify test script that a failure occured
      end if

   end subroutine check_errflg

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
            errmsg, errflg, input_vars=.true., output_vars=.false.)
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
            errmsg, errflg, input_vars=.false., output_vars=.true.)
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

    subroutine advect_constituents()
       use test_host_mod, only: phys_state, ncnst
       use test_host_mod, only: twist_array

       ! Local variables
       integer         :: q_ind      ! Constituent index

       do q_ind = 1, ncnst ! Skip checks, they were done in constituents_in
          call twist_array(phys_state%q(:,:,q_ind))
       end do
    end subroutine advect_constituents

    !> \section arg_table_test_host  Argument Table
    !! \htmlinclude arg_table_test_host.html
    !!
    subroutine test_host(retval, test_suites)

       use ccpp_constituent_prop_mod, only: ccpp_constituent_prop_ptr_t
       use test_host_mod,      only: num_time_steps
       use test_host_mod,      only: init_data, compare_data
       use test_host_mod,      only: ncols, pver
       use test_host_ccpp_cap, only: test_host_ccpp_register_constituents
       use test_host_ccpp_cap, only: test_host_ccpp_is_scheme_constituent
       use test_host_ccpp_cap, only: test_host_ccpp_initialize_constituents
       use test_host_ccpp_cap, only: test_host_ccpp_number_constituents
       use test_host_ccpp_cap, only: test_host_constituents_array
       use test_host_ccpp_cap, only: test_host_ccpp_physics_initialize
       use test_host_ccpp_cap, only: test_host_ccpp_physics_timestep_initial
       use test_host_ccpp_cap, only: test_host_ccpp_physics_run
       use test_host_ccpp_cap, only: test_host_ccpp_physics_timestep_final
       use test_host_ccpp_cap, only: test_host_ccpp_physics_finalize
       use test_host_ccpp_cap, only: ccpp_physics_suite_list
       use test_host_ccpp_cap, only: test_host_const_get_index
       use test_host_ccpp_cap, only: test_host_model_const_properties

       type(suite_info), intent(in)  :: test_suites(:)
       logical,          intent(out) :: retval

       logical                         :: check
       integer                         :: col_start, col_end
       integer                         :: index, sind
       integer                         :: index_liq, index_ice
       integer                         :: time_step
       integer                         :: num_suites
       integer                         :: num_advected ! Num advected species
       logical                         :: const_log
       logical                         :: is_constituent
       logical                         :: has_default
       character(len=128), allocatable :: suite_names(:)
       character(len=256)              :: const_str
       character(len=512)              :: errmsg
       integer                         :: errflg
       integer                         :: errflg_final ! Used to notify testing script of test failure
       real(kind_phys), pointer        :: const_ptr(:,:,:)
       real(kind_phys)                 :: default_value
       real(kind_phys)                 :: check_value
       type(ccpp_constituent_prop_ptr_t), pointer :: const_props(:)
       character(len=*), parameter     :: subname = 'test_host'

       ! Initialized "final" error flag used to report a failure to the larged
       ! testing script:
       errflg_final = 0

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

      errflg = 0
      errmsg = ''

      ! Check that is_scheme_constituent works as expected
      call test_host_ccpp_is_scheme_constituent('specific_humidity',          &
           is_constituent, errflg, errmsg)
      call check_errflg(subname//"_ccpp_is_scheme_constituent", errflg,       &
           errmsg, errflg_final)
      ! specific_humidity should not be an existing constituent
      if (is_constituent) then
         write(6, *) "ERROR: specific humidity is already a constituent"
         errflg_final = -1 !Notify test script that a failure occurred
      end if
      call test_host_ccpp_is_scheme_constituent('cloud_ice_dry_mixing_ratio', &
           is_constituent, errflg, errmsg)
      call check_errflg(subname//"_ccpp_is_scheme_constituent", errflg,       &
           errmsg, errflg_final)
      ! cloud_ice_dry_mixing_ratio should be an existing constituent
      if (.not. is_constituent) then
         write(6, *) "ERROR: cloud_ice_dry_mixing ratio not found in ",       &
                        "host cap constituent list"
         errflg_final = -1 !Notify test script that a failure occurred
      end if


      ! Register the constituents to find out what needs advecting
      call host_constituents(1)%instantiate(std_name="specific_humidity",     &
           long_name="Specific humidity", units="kg kg-1",                    &
           vertical_dim="vertical_layer_dimension", advected=.true.,          &
           min_value=1000._kind_phys, molar_mass=2000._kind_phys,           &
           errcode=errflg, errmsg=errmsg)
      call check_errflg(subname//'.initialize', errflg, errmsg, errflg_final)
      if (errflg == 0) then
         call test_host_ccpp_register_constituents(suite_names(:),            &
              host_constituents, errmsg=errmsg, errflg=errflg)
      end if
      if (errflg /= 0) then
         write(6, '(2a)') 'ERROR register_constituents: ', trim(errmsg)
         retval = .false.
         return
      end if
      ! Check number of advected constituents
      if (errflg == 0) then
         call test_host_ccpp_number_constituents(num_advected, errmsg=errmsg, &
              errflg=errflg)
         call check_errflg(subname//".num_advected", errflg, errmsg, errflg_final)
      end if
      if (num_advected /= 3) then
         write(6, '(a,i0)') "ERROR: num advected constituents = ", num_advected
         retval = .false.
         return
      end if
      ! Initialize constituent data
      call test_host_ccpp_initialize_constituents(ncols, pver, errflg, errmsg)

      !Stop tests here if initialization failed (as all other tests will likely
      !fail as well:
      if (errflg /= 0) then
         retval = .false.
         return
      end if

      ! Initialize our 'data'
      const_ptr => test_host_constituents_array()

      !Check if the specific humidity index can be found:
      call test_host_const_get_index('specific_humidity', index,              &
           errflg, errmsg)
      call check_errflg(subname//".index_specific_humidity", errflg, errmsg,  &
                        errflg_final)

      !Check if the cloud liquid index can be found:
      call test_host_const_get_index('cloud_liquid_dry_mixing_ratio',         &
           index_liq, errflg, errmsg)
      call check_errflg(subname//".index_cld_liq", errflg, errmsg,            &
           errflg_final)

      !Check if the cloud ice index can be found:
      call test_host_const_get_index('cloud_ice_dry_mixing_ratio',            &
           index_ice, errflg, errmsg)
      call check_errflg(subname//".index_cld_ice", errflg, errmsg,            &
           errflg_final)

      !Stop tests here if the index checks failed, as all other tests will
      !likely fail as well:
      if (errflg_final /= 0) then
         retval = .false.
         return
      end if

      call init_data(const_ptr, index, index_liq, index_ice)

      ! Check some constituent properties
      !++++++++++++++++++++++++++++++++++

      const_props => test_host_model_const_properties()

      !Standard name:
      call const_props(index)%standard_name(const_str, errflg, errmsg)
      if (errflg /= 0) then
         write(6, '(a,i0,a,a,i0,/,a)') "ERROR: Error, ", errflg, " trying ",  &
               "to get standard_name for specific_humidity, index = ",        &
               index, trim(errmsg)
         errflg_final = -1 !Notify test script that a failure occured
      end if
      if (errflg == 0) then
         if (trim(const_str) /= 'specific_humidity') then
            write(6, *) "ERROR: standard name, '", trim(const_str),           &
                 "' should be 'specific_humidity'"
            errflg_final = -1 !Notify test script that a failure occured
         end if
      else
         !Reset error flag to continue testing other properties:
         errflg = 0
      end if

      !Long name:
      call const_props(index_liq)%long_name(const_str, errflg, errmsg)
      if (errflg /= 0) then
         write(6, '(a,i0,a,i0,/,a)') "ERROR: Error, ", errflg, " trying ",    &
               "to get long_name for cld_liq index = ",                       &
               index_liq, trim(errmsg)
         errflg_final = -1 !Notify test script that a failure occured
      end if
      if (errflg == 0) then
         if (trim(const_str) /= 'Cloud liquid dry mixing ratio') then
            write(6, *) "ERROR: long name, '", trim(const_str),               &
                 "' should be 'Cloud liquid dry mixing ratio'"
            errflg_final = -1 !Notify test script that a failure occured
         end if
      else
         !Reset error flag to continue testing other properties:
         errflg = 0
      end if

      !Mass mixing ratio:
      call const_props(index_ice)%is_mass_mixing_ratio(const_log, errflg,     &
           errmsg)
      if (errflg /= 0) then
         write(6, '(a,i0,a,a,i0,/,a)') "ERROR: Error, ", errflg, " trying ",  &
              "to get mass mixing ratio prop for cld_ice index = ",           &
              index_ice, trim(errmsg)
         errflg_final = -1 !Notify test script that a failure occured
      end if
      if (errflg == 0) then
         if (.not. const_log) then
            write(6, *) "ERROR: cloud ice is not a mass mixing_ratio"
            errflg_final = -1 !Notify test script that a failure occured
         end if
      else
         !Reset error flag to continue testing other properties:
         errflg = 0
      end if

      !Dry mixing ratio:
      call const_props(index_ice)%is_dry(const_log, errflg, errmsg)
      if (errflg /= 0) then
         write(6, '(a,i0,a,a,i0,/,a)') "ERROR: Error, ", errflg, " trying ",  &
              "to get dry prop for cld_ice index = ", index_ice, trim(errmsg)
         errflg_final = -1 !Notify test script that a failure occurred
      end if
      if (errflg == 0) then
         if (.not. const_log) then
            write(6, *) "ERROR: cloud ice mass_mixing_ratio is not dry"
            errflg_final = -1
         end if
      else
         !Reset error flag to continue testing other properties:
         errflg = 0
      end if

      !-------------------
      !minimum value tests:
      !-------------------

      !Check that a constituent's minimum value defaults to zero:
      call const_props(index_ice)%minimum(check_value, errflg, errmsg)
      if (errflg /= 0) then
         write(6, '(a,i0,a,a,i0,/,a)') "ERROR: Error, ", errflg, " trying ",  &
              "to get minimum value for cld_ice index = ", index_ice,         &
              trim(errmsg)
         errflg_final = -1 !Notify test script that a failure occurred
      end if
      if (errflg == 0) then
         if (check_value /= 0._kind_phys) then !Should be zero
            write(6, *) "ERROR: 'minimum' should default to zero for all ",   &
                 "constituents unless set by host model or scheme metadata."
            errflg_final = -1 !Notify test script that a failure occured
         end if
      else
         !Reset error flag to continue testing other properties:
         errflg = 0
      end if

      !Check that a constituent instantiated with a specified minimum value
      !actually contains that minimum value property:
      call const_props(index)%minimum(check_value, errflg, errmsg)
      if (errflg /= 0) then
         write(6, '(a,i0,a,a,i0,/,a)') "ERROR: Error, ", errflg, " trying ",  &
              "to get minimum value for specific humidity index = ", index,   &
              trim(errmsg)
         errflg_final = -1 !Notify test script that a failure occurred
      end if
      if (errflg == 0) then
         if (check_value /= 1000._kind_phys) then !Should be 1000
            write(6, *) "ERROR: 'minimum' should give a value of 1000 ",      &
                 "for specific humidity, as was set during instantiation."
            errflg_final = -1 !Notify test script that a failure occured
         end if
      else
         !Reset error flag to continue testing other properties:
         errflg = 0
      end if

      !Check that setting a constituent's minimum value works
      !as expected:
      call const_props(index_ice)%set_minimum(1._kind_phys, errflg, errmsg)
      if (errflg /= 0) then
         write(6, '(a,i0,a,a,i0,/,a)') "ERROR: Error, ", errflg, " trying ",  &
              "to set minimum value for cld_ice index = ", index_ice,         &
              trim(errmsg)
         errflg_final = -1 !Notify test script that a failure occurred
      end if
      if (errflg == 0) then
         call const_props(index_ice)%minimum(check_value, errflg, errmsg)
         if (errflg /= 0) then
            write(6, '(a,i0,a,i0,/,a)') "ERROR: Error, ", errflg,             &
                 " trying to get minimum value for cld_ice index = ",         &
                 index_ice, trim(errmsg)
            errflg_final = -1 !Notify test script that a failure occurred
         end if
      end if
      if (errflg == 0) then
         if (check_value /= 1._kind_phys) then !Should now be one
            write(6, *) "ERROR: 'set_minimum' did not set constituent",       &
                 " minimum value correctly."
            errflg_final = -1 !Notify test script that a failure occurred
         end if
      else
         !Reset error flag to continue testing other properties:
         errflg = 0
      end if

      !----------------------
      !molecular weight tests:
      !----------------------

      !Check that a constituent instantiated with a specified molecular
      !weight actually contains that molecular weight property value:
      call const_props(index)%molar_mass(check_value, errflg, errmsg)
      if (errflg /= 0) then
         write(6, '(a,i0,a,a,i0,/,a)') "ERROR: Error, ", errflg, " trying ",  &
              "to get molecular weight for specific humidity index = ",       &
              index, trim(errmsg)
         errflg_final = -1 !Notify test script that a failure occurred
      end if
      if (errflg == 0) then
         if (check_value /= 2000._kind_phys) then !Should be 2000
            write(6, *) "ERROR: 'molar_mass' should give a value of 2000 ", &
                 "for specific humidity, as was set during instantiation."
            errflg_final = -1 !Notify test script that a failure occured
         end if
      else
         !Reset error flag to continue testing other properties:
         errflg = 0
      end if

      !Check that setting a constituent's molecular weight works
      !as expected:
      call const_props(index_ice)%set_molar_mass(1._kind_phys, errflg,      &
                       errmsg)
      if (errflg /= 0) then
         write(6, '(a,i0,a,a,i0,/,a)') "ERROR: Error, ", errflg, " trying ",  &
              "to set molecular weight for cld_ice index = ", index_ice,      &
              trim(errmsg)
         errflg_final = -1 !Notify test script that a failure occurred
      end if
      if (errflg == 0) then
         call const_props(index_ice)%molar_mass(check_value, errflg, errmsg)
         if (errflg /= 0) then
            write(6, '(a,i0,a,i0,/,a)') "ERROR: Error, ", errflg,             &
                 " trying to get molecular weight for cld_ice index = ",      &
                 index_ice, trim(errmsg)
            errflg_final = -1 !Notify test script that a failure occurred
         end if
      end if
      if (errflg == 0) then
         if (check_value /= 1._kind_phys) then !Should be equal to one
            write(6, *) "ERROR: 'set_molar_mass' did not set constituent",  &
                 " molecular weight value correctly."
            errflg_final = -1 !Notify test script that a failure occurred
         end if
      else
         !Reset error flag to continue testing other properties:
         errflg = 0
      end if

      !-------------------
      !thermo-active tests:
      !-------------------

      !Check that being thermodynamically active defaults to False:
      call const_props(index_ice)%is_thermo_active(check, errflg, errmsg)
      if (errflg /= 0) then
         write(6, '(a,i0,a,a,i0,/,a)') "ERROR: Error, ", errflg, " trying ",  &
              "to get thermo_active prop for cld_ice index = ", index_ice,    &
              trim(errmsg)
         errflg_final = -1 !Notify test script that a failure occurred
      end if
      if (errflg == 0) then
         if (check) then !Should be False
            write(6, *) "ERROR: 'is_thermo_active' should default to False ", &
                 "for all constituents unless set by host model."
            errflg_final = -1 !Notify test script that a failure occured
         end if
      else
         !Reset error flag to continue testing other properties:
         errflg = 0
      end if

      !Check that setting a constituent to be thermodynamically active works
      !as expected:
      call const_props(index_ice)%set_thermo_active(.true., errflg, errmsg)
      if (errflg /= 0) then
         write(6, '(a,i0,a,a,i0,/,a)') "ERROR: Error, ", errflg, " trying ",  &
              "to set thermo_active prop for cld_ice index = ", index_ice,    &
              trim(errmsg)
         errflg_final = -1 !Notify test script that a failure occurred
      end if
      if (errflg == 0) then
         call const_props(index_ice)%is_thermo_active(check, errflg, errmsg)
         if (errflg /= 0) then
            write(6, '(a,i0,a,i0,/,a)') "ERROR: Error, ", errflg,             &
                 " trying to get thermo_active prop for cld_ice index = ",    &
                 index_ice, trim(errmsg)
            errflg_final = -1 !Notify test script that a failure occurred
         end if
      end if
      if (errflg == 0) then
         if (.not. check) then !Should now be True
            write(6, *) "ERROR: 'set_thermo_active' did not set",             &
                 " thermo_active constituent property correctly."
            errflg_final = -1 !Notify test script that a failure occurred
         end if
      else
         !Reset error flag to continue testing other properties:
         errflg = 0
      end if
      !-------------------

      !-------------------
      !water-species tests:
      !-------------------

      !Check that being a water species defaults to False:
      call const_props(index_liq)%is_water_species(check, errflg, errmsg)
      if (errflg /= 0) then
         write(6, '(a,i0,a,a,i0,/,a)') "ERROR: Error, ", errflg, " trying ",  &
              "to get water_species prop for cld_liq index = ", index_liq,    &
              trim(errmsg)
         errflg_final = -1 !Notify test script that a failure occurred
      end if
      if (errflg == 0) then
         if (check) then !Should be False
            write(6, *) "ERROR: 'is_water_species' should default to False ", &
                 "for all constituents unless set by host model."
            errflg_final = -1 !Notify test script that a failure occured
         end if
      else
         !Reset error flag to continue testing other properties:
         errflg = 0
      end if

      !Check that setting a constituent to be a water species works
      !as expected:
      call const_props(index_liq)%set_water_species(.true., errflg, errmsg)
      if (errflg /= 0) then
         write(6, '(a,i0,a,a,i0,/,a)') "ERROR: Error, ", errflg, " trying ",  &
              "to set water_species prop for cld_liq index = ", index_liq,    &
              trim(errmsg)
         errflg_final = -1 !Notify test script that a failure occurred
      end if
      if (errflg == 0) then
         call const_props(index_liq)%is_water_species(check, errflg, errmsg)
         if (errflg /= 0) then
            write(6, '(a,i0,a,i0,/,a)') "ERROR: Error, ", errflg,             &
                 " trying to get water_species prop for cld_liq index = ",    &
                 index_liq, trim(errmsg)
            errflg_final = -1 !Notify test script that a failure occurred
         end if
      end if
      if (errflg == 0) then
         if (.not. check) then !Should now be True
            write(6, *) "ERROR: 'set_water_species' did not set",             &
                 " water_species constituent property correctly."
            errflg_final = -1 !Notify test script that a failure occurred
         end if
      else
         !Reset error flag to continue testing other properties:
         errflg = 0
      end if
      !-------------------

      !Check that setting a constituent's default value works as expected
      call const_props(index_liq)%has_default(has_default, errflg, errmsg)
      if (errflg /= 0) then
         write(6, '(a,i0,2a,i0,/,a)') "ERROR: Error, ", errflg, " trying ", &
              "to check for default for cld_liq index = ", index_liq, trim(errmsg)
         errflg_final = -1 !Notify test script that a failure occurred
      end if
      if (errflg == 0) then
         if (has_default) then
            write(6, *) "ERROR: cloud liquid mass_mixing_ratio should not have default but does"
            errflg_final = -1 !Notify test script that a failure occurred
         end if
      else
         !Reset error flag to continue testing other properties:
         errflg = 0
      end if
      call const_props(index_ice)%has_default(has_default, errflg, errmsg)
      if (errflg /= 0) then
         write(6, '(a,i0,2a,i0,/,a)') "ERROR: Error, ", errflg, " trying ", &
              "to check for default for cld_ice index = ", index_ice, trim(errmsg)
         errflg_final = -1 !Notify test script that a failure occurred
      end if
      if (errflg == 0) then
         if (.not. has_default) then
            write(6, *) "ERROR: cloud ice mass_mixing_ratio should have default but doesn't"
            errflg_final = -1 !Notify test script that a failure occurred
         end if
      else
         !Reset error flag to continue testing other properties:
         errflg = 0
      end if
      call const_props(index_ice)%default_value(default_value, errflg, errmsg)
      if (errflg /= 0) then
         write(6, '(a,i0,2a,i0,/,a)') "ERROR: Error, ", errflg, " trying ", &
              "to grab default for cld_ice index = ", index_ice, trim(errmsg)
         errflg_final = -1 !Notify test script that a failure occurred
      end if
      if (errflg == 0) then
         if (default_value /= 0.0_kind_phys) then
            write(6, *) "ERROR: cloud ice mass_mixing_ratio default is ", default_value, &
                        " but should be 0.0"
            errflg_final = -1 !Notify test script that a failure occurred
         end if
      else
         !Reset error flag to continue testing other properties:
         errflg = 0
      end if
      !++++++++++++++++++++++++++++++++++

      !Set error flag to the "final" value, because any error
      !above will likely result in a large number of failures
      !below:
      errflg = errflg_final

      ! Use the suite information to setup the run
      do sind = 1, num_suites
         if (errflg == 0) then
            call test_host_ccpp_physics_initialize(                           &
                 test_suites(sind)%suite_name, errmsg, errflg)
            if (errflg /= 0) then
               write(6, '(4a)') 'ERROR in initialize of ',                   &
                    trim(test_suites(sind)%suite_name), ': ', trim(errmsg)
               exit
            end if
         end if
       end do

       ! Loop over time steps
       do time_step = 1, num_time_steps
          ! Initialize the timestep
          do sind = 1, num_suites
             if (errflg == 0) then
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
                   if (errflg == 0) then
                      call test_host_ccpp_physics_run(                        &
                           test_suites(sind)%suite_name,                      &
                           test_suites(sind)%suite_parts(index),              &
                           col_start, col_end, errmsg, errflg)
                      if (errflg /= 0) then
                         write(6, '(5a)') trim(test_suites(sind)%suite_name), &
                              '/', trim(test_suites(sind)%suite_parts(index)),&
                              ': ', trim(errmsg)
                         exit
                      end if
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
                exit
             end if
          end do

          ! Run "dycore"
          if (errflg == 0) then
             call advect_constituents()
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
                exit
             end if
          end if
       end do

       if (errflg == 0) then
          ! Run finished without error, check answers
          if (compare_data(num_advected)) then
             write(6, *) 'Answers are correct!'
             errflg = 0
          else
             write(6, *) 'Answers are not correct!'
             errflg = -1
          end if
       end if

       !Make sure "final" flag is non-zero if "errflg" is:
       if (errflg /= 0) then
          errflg_final = -1 !Notify test script that a failure occured
       end if

       !Set return value to False if any errors were found:
       retval = errflg_final == 0

    end subroutine test_host

 end module test_prog

 program test
    use test_prog, only: test_host, suite_info, cm, cs

    implicit none

   character(len=cs), target :: test_parts1(1)
   character(len=cm), target :: test_invars1(7)
   character(len=cm), target :: test_outvars1(6)
   character(len=cm), target :: test_reqvars1(9)

    type(suite_info) :: test_suites(1)
    logical :: run_okay

    test_parts1 = (/ 'physics         '/)
    test_invars1 = (/                          &
        'cloud_ice_dry_mixing_ratio          ',                               &
        'cloud_liquid_dry_mixing_ratio       ',                               &
        'surface_air_pressure                ',                               &
        'temperature                         ',                               &
        'time_step_for_physics               ',                               &
        'water_temperature_at_freezing       ',                               &
        'water_vapor_specific_humidity       ' /)
    test_outvars1 = (/                         &
        'ccpp_error_message                  ',                               &
        'ccpp_error_code                     ',                               &
        'temperature                         ',                               &
        'water_vapor_specific_humidity       ',                               &
        'cloud_liquid_dry_mixing_ratio       ',                               &
        'cloud_ice_dry_mixing_ratio          ' /)
    test_reqvars1 = (/                         &
        'surface_air_pressure                ',                               &
        'temperature                         ',                               &
        'time_step_for_physics               ',                               &
        'cloud_liquid_dry_mixing_ratio       ',                               &
        'cloud_ice_dry_mixing_ratio          ',                               &
        'water_temperature_at_freezing       ',                               &
        'water_vapor_specific_humidity       ',                               &
        'ccpp_error_message                  ',                               &
        'ccpp_error_code                     ' /)


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
