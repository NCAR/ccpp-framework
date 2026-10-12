module test_prog

  use ccpp_kinds, only: kind_phys
  use ccpp_constituent_prop_mod, only: ccpp_constituent_properties_t

  implicit none
  private

  public test_host

  ! Public data and interfaces
  integer, public, parameter :: cs = 16
  integer, public, parameter :: cm = 41

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

  type(ccpp_constituent_properties_t), private, target, allocatable :: host_constituents(:)

  private :: check_suite
  private :: advect_constituents ! Move data around
  private :: check_errcode

contains

  subroutine check_errcode(subname, errcode, errmsg, errcode_final)
    ! If errcode is not zero, print an error message
    character(len=*), intent(in) :: subname
    integer, intent(in) :: errcode
    character(len=*), intent(in) :: errmsg

    integer, intent(out) :: errcode_final

    if (errcode /= 0) then
      write(6, '(a,i0,4a)') "Error ", errcode, " from ", trim(subname), &
          ':', trim(errmsg)
      !Notify test script that a failure occurred:
      errcode_final = -1 !Notify test script that a failure occured
    end if

  end subroutine check_errcode

  logical function check_suite(test_suite)
    use test_host_ccpp_cap, only: ccpp_physics_suite_part_list
    use test_host_ccpp_cap, only: ccpp_physics_suite_variables
    use test_utils, only: check_list

    ! Dummy argument
    type(suite_info), intent(in) :: test_suite
    ! Local variables
    logical :: check
    integer :: errcode
    character(len=512) :: errmsg
    character(len=128), allocatable :: test_list(:)

    check_suite = .true.
    ! First, check the suite parts
    call ccpp_physics_suite_part_list(test_suite%suite_name, test_list, &
        errmsg, errcode)
    if (errcode == 0) then
      check = check_list(test_list, test_suite%suite_parts, 'part names', &
          suite_name=test_suite%suite_name)
    else
      check = .false.
      write(6, '(a,i0,2a)') 'ERROR ', errcode, ': ', trim(errmsg)
    end if
    check_suite = check_suite .and. check
    if (allocated(test_list)) then
      deallocate(test_list)
    end if
    ! Check the input variables
    call ccpp_physics_suite_variables(test_suite%suite_name, test_list, &
        errmsg, errcode, input_vars=.true., output_vars=.false.)
    if (errcode == 0) then
      check = check_list(test_list, test_suite%suite_input_vars, &
          'input variable names', suite_name=test_suite%suite_name)
    else
      check = .false.
      write(6, '(a,i0,2a)') 'ERROR ', errcode, ': ', trim(errmsg)
    end if
    check_suite = check_suite .and. check
    if (allocated(test_list)) then
      deallocate(test_list)
    end if
    ! Check the output variables
    call ccpp_physics_suite_variables(test_suite%suite_name, test_list, &
        errmsg, errcode, input_vars=.false., output_vars=.true.)
    if (errcode == 0) then
      check = check_list(test_list, test_suite%suite_output_vars, &
          'output variable names', suite_name=test_suite%suite_name)
    else
      check = .false.
      write(6, '(a,i0,2a)') 'ERROR ', errcode, ': ', trim(errmsg)
    end if
    check_suite = check_suite .and. check
    if (allocated(test_list)) then
      deallocate(test_list)
    end if
    ! Check all required variables
    call ccpp_physics_suite_variables(test_suite%suite_name, test_list, &
        errmsg, errcode)
    if (errcode == 0) then
      check = check_list(test_list, test_suite%suite_required_vars, &
          'required variable names', suite_name=test_suite%suite_name)
    else
      check = .false.
      write(6, '(a,i0,2a)') 'ERROR ', errcode, ': ', trim(errmsg)
    end if
    check_suite = check_suite .and. check
    if (allocated(test_list)) then
      deallocate(test_list)
    end if
  end function check_suite

  subroutine advect_constituents()
    use test_host_mod, only: phys_state, &
        ncnst
    use test_host_mod, only: twist_array

    ! Local variables
    integer :: q_ind ! Constituent index

    do q_ind = 1, ncnst ! Skip checks, they were done in constituents_in
      call twist_array(phys_state%q(:, :, q_ind))
    end do
  end subroutine advect_constituents

  !> \section arg_table_test_host  Argument Table
  !! \htmlinclude arg_table_test_host.html
  !!
  subroutine test_host(retval, test_suites)

    use ccpp_constituent_prop_mod, only: ccpp_constituent_prop_ptr_t
    use test_host_mod, only: num_time_steps
    use test_host_mod, only: init_data, &
        compare_data
    use test_host_mod, only: ncols, &
        pver
    use test_host_data, only: num_consts, &
        std_name_array, &
        const_std_name
    use test_host_data, only: check_constituent_indices
    use test_host_ccpp_cap, only: ccpp_deallocate_dynamic_constituents
    use test_host_ccpp_cap, only: ccpp_register_constituents
    use test_host_ccpp_cap, only: ccpp_is_scheme_constituent
    use test_host_ccpp_cap, only: ccpp_initialize_constituents
    use test_host_ccpp_cap, only: ccpp_number_constituents
    use test_host_ccpp_cap, only: ccpp_constituents_array
    use test_host_ccpp_cap, only: ccpp_register
    use test_host_ccpp_cap, only: ccpp_init
    use test_host_ccpp_cap, only: ccpp_physics_init
    use test_host_ccpp_cap, only: ccpp_physics_timestep_init
    use test_host_ccpp_cap, only: ccpp_physics_run
    use test_host_ccpp_cap, only: ccpp_physics_timestep_final
    use test_host_ccpp_cap, only: ccpp_physics_final
    use test_host_ccpp_cap, only: ccpp_final
    use test_host_ccpp_cap, only: ccpp_physics_suite_list
    use test_host_ccpp_cap, only: ccpp_const_get_index
    use test_host_ccpp_cap, only: ccpp_model_const_properties
    use test_utils, only: check_list

    type(suite_info), intent(in) :: test_suites(:)
    logical, intent(out) :: retval

    logical :: check
    integer :: col_start, col_end
    integer :: index, sind
    integer :: index_liq, index_ice
    integer :: index_dyn1, index_dyn2, index_dyn3
    integer :: time_step
    integer :: num_suites
    integer :: num_advected ! Num advected species
    logical :: const_log
    logical :: is_constituent
    logical :: has_default
    integer :: test_scalar_const_index
    integer :: test_const_indices(num_consts)
    character(len=128), allocatable :: suite_names(:)
    character(len=256) :: const_str
    character(len=512) :: errmsg
    character(len=512) :: expected_error
    integer :: errcode
    integer :: errcode_final ! Used to notify testing script of test failure
    real(kind=kind_phys), pointer :: const_ptr(:, :, :)
    real(kind=kind_phys) :: default_value
    real(kind=kind_phys) :: check_value
    type(ccpp_constituent_prop_ptr_t), pointer :: const_props(:)
    character(len=*), parameter :: subname = 'test_host'

    ! Initialized "final" error flag used to report a failure to the larged
    ! testing script:
    errcode_final = 0

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

    errcode = 0
    errmsg = ''

    ! Check that is_scheme_constituent works as expected
    call ccpp_is_scheme_constituent('specific_humidity', &
        is_constituent, errcode, errmsg)
    call check_errcode(subname // "_ccpp_is_scheme_constituent", errcode, &
        errmsg, errcode_final)
    ! specific_humidity should not be an existing constituent
    if (is_constituent) then
      write(6, *) "ERROR: specific humidity is already a constituent"
      errcode_final = -1 ! Notify test script that a failure occurred
    end if
    call ccpp_is_scheme_constituent('cloud_ice_dry_mixing_ratio', &
        is_constituent, errcode, errmsg)
    call check_errcode(subname // "_ccpp_is_scheme_constituent", errcode, &
        errmsg, errcode_final)
    ! cloud_ice_dry_mixing_ratio should be an existing constituent
    if (.not. is_constituent) then
      write(6, *) "ERROR: cloud_ice_dry_mixing ratio not found in ", &
          "host cap constituent list"
      errcode_final = -1 ! Notify test script that a failure occurred
    end if

    ! Use the suite information to call the register phase
    do sind = 1, num_suites
      if (errcode == 0) then
        call ccpp_register(suite_name=test_suites(sind)%suite_name, &
            errmsg=errmsg, errcode=errcode)
        if (errcode /= 0) then
          write(6, '(4a)') 'ERROR in register of ', &
              trim(test_suites(sind)%suite_name), ': ', trim(errmsg)
          exit
        end if
      end if
    end do

    ! Register the constituents to find out what needs advecting
    ! DO A COUPLE OF TESTS FIRST

    ! First confirm the correct error occurs if you try to add an
    ! incompatible constituent with the same standard name
    expected_error = 'ccp_model_const_add_metadata ERROR: Trying to add ' //&
        'constituent specific_humidity but an incompatible ' // &
        'constituent with this name already exists'
    allocate(host_constituents(2))
    call host_constituents(1)%instantiate(std_name="specific_humidity", &
        long_name="Specific humidity", diag_name='H2O', units="kg kg-1", &
        vertical_dim="vertical_layer_dimension", advected=.true., &
        min_value=1000._kind_phys, molar_mass=2000._kind_phys, &
        errcode=errcode, errmsg=errmsg)
    call host_constituents(2)%instantiate(std_name="specific_humidity", &
        long_name="Specific humidity", diag_name='H2O', units="kg kg", &
        vertical_dim="vertical_layer_dimension", advected=.true., &
        min_value=1000._kind_phys, molar_mass=2000._kind_phys, &
        errcode=errcode, errmsg=errmsg)
    call check_errcode(subname // '.initialize', errcode, errmsg, errcode_final)
    if (errcode == 0) then
      call ccpp_register_constituents(host_constituents, &
          errmsg=errmsg, errcode=errcode)
    end if
    ! Check the error
    if (errcode == 0) then
      write(6, '(2a)') 'ERROR register_constituents: expected this error: ', &
          trim(expected_error)
    else
      if (trim(errmsg) /= trim(expected_error)) then
        write(6, '(4a)') 'ERROR register_constituents: expected this error: ', &
            trim(expected_error), ' Got: ', trim(errmsg)
      end if
    end if

    ! Now try again but with a compatible constituent - should be ignored when
    ! the constituents object is created
    ! Use the suite information to call the register phase
    errcode = 0
    call ccpp_deallocate_dynamic_constituents()
    deallocate(host_constituents)
    do sind = 1, num_suites
      if (errcode == 0) then
        call ccpp_register(suite_name=test_suites(sind)%suite_name, &
            errmsg=errmsg, errcode=errcode)
        if (errcode /= 0) then
          write(6, '(4a)') 'ERROR in register of ', &
              trim(test_suites(sind)%suite_name), ': ', trim(errmsg)
          exit
        end if
      end if
    end do
    allocate(host_constituents(3))
    call host_constituents(1)%instantiate(std_name="specific_humidity", &
        long_name="Specific humidity", diag_name='H2O', units="kg kg-1", &
        vertical_dim="vertical_layer_dimension", advected=.true., &
        min_value=1000._kind_phys, molar_mass=2000._kind_phys, &
        errcode=errcode, errmsg=errmsg)
    call host_constituents(2)%instantiate(std_name="specific_humidity", &
        long_name="Specific humidity", diag_name='H2O', units="kg kg-1", &
        vertical_dim="vertical_layer_dimension", advected=.true., &
        min_value=1000._kind_phys, molar_mass=2000._kind_phys, &
        errcode=errcode, errmsg=errmsg)
    call host_constituents(3)%instantiate( &
        std_name='cloud_ice_dry_mixing_ratio',               &
        long_name='Cloud ice dry mixing ratio',              &
        diag_name='CLDICE',                                  &
        units='kg kg-1',                                     &
        vertical_dim='vertical_layer_dimension',             &
        advected=.true.,                                     &
        default_value=0._kind_phys,                          &
        !water_species=.true.,                                &
        mixing_ratio_type='dry',                             &
        errcode=errcode, errmsg=errmsg)

    call check_errcode(subname // '.initialize', errcode, errmsg, errcode_final)
    if (errcode == 0) then
      call ccpp_register_constituents(host_constituents, &
          errmsg=errmsg, errcode=errcode)
    end if
    if (errcode /= 0) then
      write(6, '(2a)') 'ERROR register_constituents: ', trim(errmsg)
      retval = .false.
      return
    end if
    ! Check number of advected constituents
    if (errcode == 0) then
      call ccpp_number_constituents(num_advected, errmsg=errmsg, &
          errcode=errcode)
      call check_errcode(subname // ".num_advected", errcode, errmsg, errcode_final)
    end if
    if (num_advected /= 6) then
      write(6, '(a,i0)') "ERROR: num advected constituents = ", num_advected
      retval = .false.
      return
    end if
    ! Initialize constituent data
    call ccpp_initialize_constituents(ncols=ncols, num_layers=pver, errcode=errcode, errmsg=errmsg)

    ! Stop tests here if initialization failed (as all other tests will likely
    ! fail as well:
    if (errcode /= 0) then
      retval = .false.
      return
    end if

    ! Initialize our 'data'
    const_ptr => ccpp_constituents_array()

    ! Check if the specific humidity index can be found:
    call ccpp_const_get_index('specific_humidity', const_index=index, errcode=errcode, errmsg=errmsg)
    call check_errcode(subname // ".index_specific_humidity", errcode, errmsg, &
        errcode_final)

    ! Check if the cloud liquid index can be found:
    call ccpp_const_get_index(stdname='cloud_liquid_dry_mixing_ratio', &
        const_index=index_liq, errcode=errcode, errmsg=errmsg)
    call check_errcode(subname // ".index_cld_liq", errcode, errmsg, &
        errcode_final)

    ! Check if the cloud ice index can be found:
    call ccpp_const_get_index(stdname='cloud_ice_dry_mixing_ratio', &
        const_index=index_ice, errcode=errcode, errmsg=errmsg)
    call check_errcode(subname // ".index_cld_ice", errcode, errmsg, &
        errcode_final)

    ! Check if the dynamic constituents indices can be found
    call ccpp_const_get_index(stdname='dyn_const1', const_index=index_dyn1, errcode=errcode, errmsg=errmsg)
    call check_errcode(subname // ".index_dyn_const1", errcode, errmsg, &
        errcode_final)
    call ccpp_const_get_index(stdname='dyn_const2_wrt_moist_air', const_index=index_dyn2, errcode=errcode, errmsg=errmsg)
    call check_errcode(subname // ".index_dyn_const2", errcode, errmsg, &
        errcode_final)
    call ccpp_const_get_index(stdname='dyn_const3_wrt_moist_air_and_condensed_water', const_index=index_dyn3, errcode=errcode, errmsg=errmsg)
    call check_errcode(subname // ".index_dyn_const3", errcode, errmsg, &
        errcode_final)

    ! Load up the test array indices
    call ccpp_const_get_index(stdname=const_std_name, const_index=test_scalar_const_index, errcode=errcode, errmsg=errmsg)
    call check_errcode(subname // "." // const_std_name, errcode, errmsg, &
        errcode_final)
    do sind = 1, num_consts
      call ccpp_const_get_index(stdname=std_name_array(sind), &
          const_index=test_const_indices(sind), errcode=errcode, errmsg=errmsg)
      call check_errcode(subname // "." // std_name_array(sind), errcode, errmsg, &
          errcode_final)
    end do

    ! Stop tests here if the index checks failed, as all other tests will
    ! likely fail as well:
    if (errcode_final /= 0) then
      retval = .false.
      return
    end if

    call init_data(const_ptr, index, index_liq, index_ice, index_dyn3)

    ! Check some constituent properties
    ! ++++++++++++++++++++++++++++++++++

    const_props => ccpp_model_const_properties()

    ! Standard name:
    call const_props(index)%standard_name(const_str, errcode, errmsg)
    if (errcode /= 0) then
      write(6, '(a,i0,a,a,i0,/,a)') "ERROR: Error, ", errcode, " trying ", &
          "to get standard_name for specific_humidity, index = ", &
          index, trim(errmsg)
      errcode_final = -1 ! Notify test script that a failure occured
    end if
    if (errcode == 0) then
      if (trim(const_str) /= 'specific_humidity') then
        write(6, *) "ERROR: standard name, '", trim(const_str), &
            "' should be 'specific_humidity'"
        errcode_final = -1 ! Notify test script that a failure occured
      end if
    else
      ! Reset error flag to continue testing other properties:
      errcode = 0
    end if
    ! Check standard name for a dynamic constituent
    call const_props(index_dyn2)%standard_name(const_str, errcode, errmsg)
    if (errcode /= 0) then
      write(6, '(a,i0,a,a,i0,/,a)') "ERROR: Error, ", errcode, " trying ", &
          "to get standard_name for dyn_const2, index = ", &
          index_dyn2, trim(errmsg)
      errcode_final = -1 ! Notify test script that a failure occured
    end if
    if (errcode == 0) then
      if (trim(const_str) /= 'dyn_const2_wrt_moist_air') then
        write(6, *) "ERROR: standard name, '", trim(const_str), &
            "' should be 'dyn_const2_wrt_moist_air'"
        errcode_final = -1 ! Notify test script that a failure occured
      end if
    else
      ! Reset error flag to continue testing other properties:
      errcode = 0
    end if

    ! Long name:
    call const_props(index_liq)%long_name(const_str, errcode, errmsg)
    if (errcode /= 0) then
      write(6, '(a,i0,a,i0,/,a)') "ERROR: Error, ", errcode, " trying ", &
          "to get long_name for cld_liq index = ", &
          index_liq, trim(errmsg)
      errcode_final = -1 ! Notify test script that a failure occured
    end if
    if (errcode == 0) then
      if (trim(const_str) /= 'Cloud liquid dry mixing ratio') then
        write(6, *) "ERROR: long name, '", trim(const_str), &
            "' should be 'Cloud liquid dry mixing ratio'"
        errcode_final = -1 ! Notify test script that a failure occured
      end if
    else
      ! Reset error flag to continue testing other properties:
      errcode = 0
    end if
    ! Check long name for a dynamic constituent
    call const_props(index_dyn1)%long_name(const_str, errcode, errmsg)
    if (errcode /= 0) then
      write(6, '(a,i0,a,i0,/,a)') "ERROR: Error, ", errcode, " trying ", &
          "to get long_name for dyn_const1 index = ", &
          index_dyn1, trim(errmsg)
      errcode_final = -1 ! Notify test script that a failure occured
    end if
    if (errcode == 0) then
      if (trim(const_str) /= 'dyn const1') then
        write(6, *) "ERROR: long name, '", trim(const_str), &
            "' should be 'dyn const1'"
        errcode_final = -1 ! Notify test script that a failure occured
      end if
    else
      ! Reset error flag to continue testing other properties:
      errcode = 0
    end if

    ! Diagnostic name:
    call const_props(index_liq)%diagnostic_name(const_str, errcode, errmsg)
    if (errcode /= 0) then
      write(6, '(a,i0,a,i0,/,a)') "ERROR: Error, ", errcode, " trying ", &
          "to get diagnostic name for cld_liq index = ", &
          index_liq, trim(errmsg)
      errcode_final = -1 ! Notify test script that a failure occured
    end if
    if (errcode == 0) then
      if (trim(const_str) /= 'CLDLIQ') then
        write(6, *) "ERROR: diagnostic name, '", trim(const_str), &
            "' should be 'CLDLIQ'"
        errcode_final = -1 ! Notify test script that a failure occured
      end if
    else
      ! Reset error flag to continue testing other properties:
      errcode = 0
    end if
    ! Check default diagnostic name is set correctly
    call const_props(index_ice)%diagnostic_name(const_str, errcode, errmsg)
    if (errcode /= 0) then
      write(6, '(a,i0,a,i0,/,a)') "ERROR: Error, ", errcode, " trying ", &
          "to get diagnostic name for cld_ice index = ", &
          index_ice, trim(errmsg)
      errcode_final = -1 ! Notify test script that a failure occured
    end if
    if (errcode == 0) then
      if (trim(const_str) /= 'CLDICE') then
        write(6, *) "ERROR: diagnostic name, '", trim(const_str), &
            "' should be 'CLDICE'"
        errcode_final = -1 ! Notify test script that a failure occured
      end if
    else
      ! Reset error flag to continue testing other properties:
      errcode = 0
    end if
    ! Check diagnostic name of a dynamic constituent
    call const_props(index_dyn2)%diagnostic_name(const_str, errcode, &
        errmsg)
    if (errcode /= 0) then
      write(6, '(a,i0,a,a,i0,/,a)') "ERROR: Error, ", errcode, " trying ", &
          "to get diagnostic name for dyn_const2 index = ", &
          index_dyn2, trim(errmsg)
      errcode_final = -1 ! Notify test script that a failure occured
    end if
    if (errcode == 0) then
      if (trim(const_str) /= 'DYNCONST2') then
        write(6, *) "ERROR: diagnostic name, '", trim(const_str), &
            "' should be 'DYNCONST2'"
        errcode_final = -1 ! Notify test script that a failure occured
      end if
    else
      ! Reset error flag to continue testing other properties:
      errcode = 0
    end if

    ! Mass mixing ratio:
    call const_props(index_ice)%is_mass_mixing_ratio(const_log, errcode, &
        errmsg)
    if (errcode /= 0) then
      write(6, '(a,i0,a,a,i0,/,a)') "ERROR: Error, ", errcode, " trying ", &
          "to get mass mixing ratio prop for cld_ice index = ", &
          index_ice, trim(errmsg)
      errcode_final = -1 ! Notify test script that a failure occured
    end if
    if (errcode == 0) then
      if (.not. const_log) then
        write(6, *) "ERROR: cloud ice is not a mass mixing_ratio"
        errcode_final = -1 ! Notify test script that a failure occured
      end if
    else
      ! Reset error flag to continue testing other properties:
      errcode = 0
    end if
    ! Check mass mixing ratio for a dynamic constituent
    call const_props(index_dyn2)%is_mass_mixing_ratio(const_log, errcode, &
        errmsg)
    if (errcode /= 0) then
      write(6, '(a,i0,a,a,i0,/,a)') "ERROR: Error, ", errcode, " trying ", &
          "to get mass mixing ratio prop for dyn_const2 index = ", &
          index_dyn2, trim(errmsg)
      errcode_final = -1 ! Notify test script that a failure occured
    end if
    if (errcode == 0) then
      if (.not. const_log) then
        write(6, *) "ERROR: dyn_const2 is not a mass mixing_ratio"
        errcode_final = -1 ! Notify test script that a failure occured
      end if
    else
      ! Reset error flag to continue testing other properties:
      errcode = 0
    end if

    ! Dry mixing ratio:
    call const_props(index_ice)%is_dry(const_log, errcode, errmsg)
    if (errcode /= 0) then
      write(6, '(a,i0,a,a,i0,/,a)') "ERROR: Error, ", errcode, " trying ", &
          "to get dry prop for cld_ice index = ", index_ice, trim(errmsg)
      errcode_final = -1 ! Notify test script that a failure occurred
    end if
    if (errcode == 0) then
      if (.not. const_log) then
        write(6, *) "ERROR: cloud ice mass_mixing_ratio is not dry"
        errcode_final = -1
      end if
    else
      ! Reset error flag to continue testing other properties:
      errcode = 0
    end if
    ! Check wet mixing ratio for dynamic constituent 1
    call const_props(index_dyn1)%is_dry(const_log, errcode, errmsg)
    if (errcode /= 0) then
      write(6, '(a,i0,a,a,i0,/,a)') "ERROR: Error, ", errcode, " trying ", &
          "to get dry prop for dyn_const1 index = ", index_dyn1, trim(errmsg)
      errcode_final = -1 ! Notify test script that a failure occurred
    end if
    if (errcode == 0) then
      if (const_log) then
        write(6, *) "ERROR: dyn_const1 is dry and should be wet"
        errcode_final = -1
      end if
    else
      ! Reset error flag to continue testing other properties:
      errcode = 0
    end if
    call const_props(index_dyn1)%is_wet(const_log, errcode, errmsg)
    if (errcode /= 0) then
      write(6, '(a,i0,a,a,i0,/,a)') "ERROR: Error, ", errcode, " trying ", &
          "to get wet prop for dyn_const1 index = ", index_dyn1, trim(errmsg)
      errcode_final = -1 ! Notify test script that a failure occurred
    end if
    if (errcode == 0) then
      if (.not. const_log) then
        write(6, *) "ERROR: dyn_const1 is not wet but should be"
        errcode_final = -1
      end if
    else
      ! Reset error flag to continue testing other properties:
      errcode = 0
    end if
    ! Check moist mixing ratio for dynamic constituent 2
    call const_props(index_dyn2)%is_dry(const_log, errcode, errmsg)
    if (errcode /= 0) then
      write(6, '(a,i0,a,a,i0,/,a)') "ERROR: Error, ", errcode, " trying ", &
          "to get dry prop for dyn_const2 index = ", index_dyn2, trim(errmsg)
      errcode_final = -1 ! Notify test script that a failure occurred
    end if
    if (errcode == 0) then
      if (const_log) then
        write(6, *) "ERROR: dyn_const2 is dry and should be moist"
        errcode_final = -1
      end if
    else
      ! Reset error flag to continue testing other properties:
      errcode = 0
    end if
    call const_props(index_dyn2)%is_moist(const_log, errcode, errmsg)
    if (errcode /= 0) then
      write(6, '(a,i0,a,a,i0,/,a)') "ERROR: Error, ", errcode, " trying ", &
          "to get moist prop for dyn_const2 index = ", index_dyn2, trim(errmsg)
      errcode_final = -1 ! Notify test script that a failure occurred
    end if
    if (errcode == 0) then
      if (.not. const_log) then
        write(6, *) "ERROR: dyn_const2 is not moist but should be"
        errcode_final = -1
      end if
    else
      ! Reset error flag to continue testing other properties:
      errcode = 0
    end if
    ! Check dry mixing ratio for dynamic constituent 3
    call const_props(index_dyn3)%is_dry(const_log, errcode, errmsg)
    if (errcode /= 0) then
      write(6, '(a,i0,a,a,i0,/,a)') "ERROR: Error, ", errcode, " trying ", &
          "to get dry prop for dyn_const3 index = ", index_dyn3, trim(errmsg)
      errcode_final = -1 ! Notify test script that a failure occurred
    end if
    if (errcode == 0) then
      if (.not. const_log) then
        write(6, *) "ERROR: dyn_const3 is not dry and should be"
        errcode_final = -1
      end if
    else
      ! Reset error flag to continue testing other properties:
      errcode = 0
    end if

    ! -------------------

    ! -------------------
    ! minimum value tests:
    ! -------------------

    ! Check that a constituent's minimum value defaults to zero:
    call const_props(index_dyn2)%minimum(check_value, errcode, errmsg)
    if (errcode /= 0) then
      write(6, '(a,i0,a,a,i0,/,a)') "ERROR: Error, ", errcode, " trying ", &
          "to get minimum value for dyn_const2 index = ", index_dyn2, &
          trim(errmsg)
      errcode_final = -1 ! Notify test script that a failure occurred
    end if
    if (errcode == 0) then
      if (check_value /= 0._kind_phys) then ! Should be zero
        write(6, *) "ERROR: 'minimum' should default to zero for all ", &
            "constituents unless set by host model or scheme metadata."
        errcode_final = -1 ! Notify test script that a failure occured
      end if
    else
      ! Reset error flag to continue testing other properties:
      errcode = 0
    end if

    ! Check that a constituent instantiated with a specified minimum value
    ! actually contains that minimum value property:
    call const_props(index_dyn1)%minimum(check_value, errcode, errmsg)
    if (errcode /= 0) then
      write(6, '(a,i0,a,a,i0,/,a)') "ERROR: Error, ", errcode, " trying ", &
          "to get minimum value for dyn_const1 index = ", index_dyn1, &
          trim(errmsg)
      errcode_final = -1 ! Notify test script that a failure occurred
    end if
    if (errcode == 0) then
      if (check_value /= 1000._kind_phys) then !Should be 1000
        write(6, *) "ERROR: 'minimum' should give a value of 1000 ", &
            "for dyn_const1, as was set during instantiation."
        errcode_final = -1 ! Notify test script that a failure occured
      end if
    else
      ! Reset error flag to continue testing other properties:
      errcode = 0
    end if

    ! Check that setting a constituent's minimum value works
    ! as expected:
    call const_props(index_dyn1)%set_minimum(1._kind_phys, errcode, errmsg)
    if (errcode /= 0) then
      write(6, '(a,i0,a,a,i0,/,a)') "ERROR: Error, ", errcode, " trying ", &
          "to set minimum value for dyn_const1 index = ", index_dyn1, &
          trim(errmsg)
      errcode_final = -1 ! Notify test script that a failure occurred
    end if
    if (errcode == 0) then
      call const_props(index_dyn1)%minimum(check_value, errcode, errmsg)
      if (errcode /= 0) then
        write(6, '(a,i0,a,i0,/,a)') "ERROR: Error, ", errcode, &
            " trying to get minimum value for dyn_const1 index = ", &
            index_dyn1, trim(errmsg)
        errcode_final = -1 ! Notify test script that a failure occurred
      end if
    end if
    if (errcode == 0) then
      if (check_value /= 1._kind_phys) then ! Should now be one
        write(6, *) "ERROR: 'set_minimum' did not set constituent", &
            " minimum value correctly."
        errcode_final = -1 ! Notify test script that a failure occurred
      end if
    else
      ! Reset error flag to continue testing other properties:
      errcode = 0
    end if

    ! ----------------------
    ! molecular weight tests:
    ! ----------------------

    ! Check that a constituent instantiated with a specified molecular
    ! weight actually contains that molecular weight property value:
    call const_props(index)%molar_mass(check_value, errcode, errmsg)
    if (errcode /= 0) then
      write(6, '(a,i0,a,a,i0,/,a)') "ERROR: Error, ", errcode, " trying ", &
          "to get molecular weight for specific humidity index = ", &
          index, trim(errmsg)
      errcode_final = -1 ! Notify test script that a failure occurred
    end if
    if (errcode == 0) then
      if (check_value /= 2000._kind_phys) then ! Should be 2000
        write(6, *) "ERROR: 'molar_mass' should give a value of 2000 ", &
            "for specific humidity, as was set during instantiation."
        errcode_final = -1 ! Notify test script that a failure occured
      end if
    else
      ! Reset error flag to continue testing other properties:
      errcode = 0
    end if

    ! Check that setting a constituent's molecular weight works
    ! as expected:
    call const_props(index_ice)%set_molar_mass(1._kind_phys, errcode, &
        errmsg)
    if (errcode /= 0) then
      write(6, '(a,i0,a,a,i0,/,a)') "ERROR: Error, ", errcode, " trying ", &
          "to set molecular weight for cld_ice index = ", index_ice, &
          trim(errmsg)
      errcode_final = -1 ! Notify test script that a failure occurred
    end if
    if (errcode == 0) then
      call const_props(index_ice)%molar_mass(check_value, errcode, errmsg)
      if (errcode /= 0) then
        write(6, '(a,i0,a,i0,/,a)') "ERROR: Error, ", errcode, &
            " trying to get molecular weight for cld_ice index = ", &
            index_ice, trim(errmsg)
        errcode_final = -1 ! Notify test script that a failure occurred
      end if
    end if
    if (errcode == 0) then
      if (check_value /= 1._kind_phys) then ! Should be equal to one
        write(6, *) "ERROR: 'set_molar_mass' did not set constituent", &
            " molecular weight value correctly."
        errcode_final = -1 ! Notify test script that a failure occurred
      end if
    else
      ! Reset error flag to continue testing other properties:
      errcode = 0
    end if

    ! -------------------
    ! thermo-active tests:
    ! -------------------

    ! Check that being thermodynamically active defaults to False:
    call const_props(index_ice)%is_thermo_active(check, errcode, errmsg)
    if (errcode /= 0) then
      write(6, '(a,i0,a,a,i0,/,a)') "ERROR: Error, ", errcode, " trying ", &
          "to get thermo_active prop for cld_ice index = ", index_ice, &
          trim(errmsg)
      errcode_final = -1 ! Notify test script that a failure occurred
    end if
    if (errcode == 0) then
      if (check) then ! Should be False
        write(6, *) "ERROR: 'is_thermo_active' should default to False ", &
            "for all constituents unless set by host model."
        errcode_final = -1 ! Notify test script that a failure occured
      end if
    else
      ! Reset error flag to continue testing other properties:
      errcode = 0
    end if

    ! Check that setting a constituent to be thermodynamically active works
    ! as expected:
    call const_props(index_ice)%set_thermo_active(.true., errcode, errmsg)
    if (errcode /= 0) then
      write(6, '(a,i0,a,a,i0,/,a)') "ERROR: Error, ", errcode, " trying ", &
          "to set thermo_active prop for cld_ice index = ", index_ice, &
          trim(errmsg)
      errcode_final = -1 ! Notify test script that a failure occurred
    end if
    if (errcode == 0) then
      call const_props(index_ice)%is_thermo_active(check, errcode, errmsg)
      if (errcode /= 0) then
        write(6, '(a,i0,a,i0,/,a)') "ERROR: Error, ", errcode, &
            " trying to get thermo_active prop for cld_ice index = ", &
            index_ice, trim(errmsg)
        errcode_final = -1 ! Notify test script that a failure occurred
      end if
    end if
    if (errcode == 0) then
      if (.not. check) then ! Should now be True
        write(6, *) "ERROR: 'set_thermo_active' did not set", &
            " thermo_active constituent property correctly."
        errcode_final = -1 ! Notify test script that a failure occurred
      end if
    else
      ! Reset error flag to continue testing other properties:
      errcode = 0
    end if
    ! -------------------

    ! -------------------
    ! water-species tests:
    ! -------------------

    ! Check that being a water species defaults to False:
    call const_props(index_liq)%is_water_species(check, errcode, errmsg)
    if (errcode /= 0) then
      write(6, '(a,i0,a,a,i0,/,a)') "ERROR: Error, ", errcode, " trying ", &
          "to get water_species prop for cld_liq index = ", index_liq, &
          trim(errmsg)
      errcode_final = -1 ! Notify test script that a failure occurred
    end if
    if (errcode == 0) then
      if (check) then ! Should be False
        write(6, *) "ERROR: 'is_water_species' should default to False ", &
            "for all constituents unless set by host model."
        errcode_final = -1 ! Notify test script that a failure occured
      end if
    else
      ! Reset error flag to continue testing other properties:
      errcode = 0
    end if

    ! Check that setting a constituent to be a water species works
    ! as expected:
    call const_props(index_liq)%set_water_species(.true., errcode, errmsg)
    if (errcode /= 0) then
      write(6, '(a,i0,a,a,i0,/,a)') "ERROR: Error, ", errcode, " trying ", &
          "to set water_species prop for cld_liq index = ", index_liq, &
          trim(errmsg)
      errcode_final = -1 ! Notify test script that a failure occurred
    end if
    if (errcode == 0) then
      call const_props(index_liq)%is_water_species(check, errcode, errmsg)
      if (errcode /= 0) then
        write(6, '(a,i0,a,i0,/,a)') "ERROR: Error, ", errcode, &
            " trying to get water_species prop for cld_liq index = ", &
            index_liq, trim(errmsg)
        errcode_final = -1 ! Notify test script that a failure occurred
      end if
    end if
    if (errcode == 0) then
      if (.not. check) then ! Should now be True
        write(6, *) "ERROR: 'set_water_species' did not set", &
            " water_species constituent property correctly."
        errcode_final = -1 ! Notify test script that a failure occurred
      end if
    else
      ! Reset error flag to continue testing other properties:
      errcode = 0
    end if

    ! Check that setting a constituent to be a water species via the
    ! instantiate call works as expected
    call const_props(index_dyn1)%is_water_species(check, errcode, errmsg)
    if (errcode /= 0) then
      write(6, '(a,i0,a,i0,/,a)') "ERROR: Error, ", errcode, &
          "trying to get water_species prop for dyn_const1 index = ", &
          index_dyn1, trim(errmsg)
    end if
    if (errcode == 0) then
      if (.not. check) then ! Should now be True
        write(6, *) "ERROR: 'water_species=.true. did not set", &
            " water_species constituent property correctly"
        errcode_final = -1 ! Notify test script that a failure occurred
      end if
    else
      ! Reset error flag to continue testing other properties:
      errcode = 0
    end if
    call const_props(index_dyn2)%is_water_species(check, errcode, errmsg)
    if (errcode /= 0) then
      write(6, '(a,i0,a,i0,/,a)') "ERROR: Error, ", errcode, &
          "trying to get water_species prop for dyn_const2 index = ", &
          index_dyn2, trim(errmsg)
    end if
    if (errcode == 0) then
      if (check) then ! Should now be False
        write(6, *) "ERROR: 'water_species=.false. did not set", &
            " water_species constituent property correctly"
        errcode_final = -1 ! Notify test script that a failure occurred
      end if
    else
      ! Reset error flag to continue testing other properties:
      errcode = 0
    end if
    ! -------------------

    ! Check that setting a constituent's default value works as expected
    call const_props(index_liq)%has_default(has_default, errcode, errmsg)
    if (errcode /= 0) then
      write(6, '(a,i0,2a,i0,/,a)') "ERROR: Error, ", errcode, " trying ", &
          "to check for default for cld_liq index = ", index_liq, trim(errmsg)
      errcode_final = -1 ! Notify test script that a failure occurred
    end if
    if (errcode == 0) then
      if (.not. has_default) then
        write(6, *) "ERROR: cloud_liquid_dry_mixing_ratio should have default but doesn't"
        errcode_final = -1 ! Notify test script that a failure occurred
      end if
    else
      ! Reset error flag to continue testing other properties:
      errcode = 0
    end if
    call const_props(index_ice)%has_default(has_default, errcode, errmsg)
    if (errcode /= 0) then
      write(6, '(a,i0,2a,i0,/,a)') "ERROR: Error, ", errcode, " trying ", &
          "to check for default for cld_ice index = ", index_ice, trim(errmsg)
      errcode_final = -1 ! Notify test script that a failure occurred
    end if
    if (errcode == 0) then
      if (.not. has_default) then
        write(6, *) "ERROR: cloud ice_dry_mixing_ratio should have default but doesn't"
        errcode_final = -1 ! Notify test script that a failure occurred
      end if
    else
      ! Reset error flag to continue testing other properties:
      errcode = 0
    end if
    call const_props(index_ice)%default_value(default_value, errcode, errmsg)
    if (errcode /= 0) then
      write(6, '(a,i0,2a,i0,/,a)') "ERROR: Error, ", errcode, " trying ", &
          "to grab default for cld_ice index = ", index_ice, trim(errmsg)
      errcode_final = -1 ! Notify test script that a failure occurred
    end if
    if (errcode == 0) then
      if (default_value /= 0.0_kind_phys) then
        write(6, *) "ERROR: cloud ice mass_mixing_ratio default is ", default_value, &
            " but should be 0.0"
        errcode_final = -1 ! Notify test script that a failure occurred
      end if
    else
      ! Reset error flag to continue testing other properties:
      errcode = 0
    end if
    ! ++++++++++++++++++++++++++++++++++

    ! Set error flag to the "final" value, because any error
    ! above will likely result in a large number of failures
    ! below:
    errcode = errcode_final

    ! Call ccpp_init
    do sind = 1, num_suites
      if (errcode == 0) then
        call ccpp_init(suite_name=test_suites(sind)%suite_name, &
            errmsg=errmsg, errcode=errcode)
        if (errcode /= 0) then
          write(6, '(4a)') 'ERROR in initialize of ', &
              trim(test_suites(sind)%suite_name), ': ', trim(errmsg)
          exit
        end if
      end if
    end do

    ! Call ccpp_physics_init
    do sind = 1, num_suites
      if (errcode == 0) then
        call ccpp_physics_init( &
            suite_name=test_suites(sind)%suite_name, &
            group_name='all', col_start=1, col_end=ncols, &
            thread_num=1, nthreads=1, nphys_threads=1, &
            errmsg=errmsg, errcode=errcode)
        if (errcode /= 0) then
          write(6, '(4a)') 'ERROR in initialize of ', &
              trim(test_suites(sind)%suite_name), ': ', trim(errmsg)
          exit
        end if
      end if
    end do

    ! Check indices
    call check_constituent_indices(test_scalar_const_index, test_const_indices, &
        errmsg, errcode)
    call check_errcode(subname // " check suite indices", errcode, errmsg, &
        errcode_final)

    ! Loop over time steps
    do time_step = 1, num_time_steps
      ! Initialize the timestep
      do sind = 1, num_suites
        if (errcode == 0) then
          call ccpp_physics_timestep_init( &
            suite_name=test_suites(sind)%suite_name, &
            group_name='all', col_start=1, col_end=ncols, &
            thread_num=1, nthreads=1, nphys_threads=1, &
            errmsg=errmsg, errcode=errcode)
          if (errcode /= 0) then
            write(6, '(3a)') trim(test_suites(sind)%suite_name), ': ', &
                trim(errmsg)
          end if
        end if
      end do

      do col_start = 1, ncols, 5
        if (errcode /= 0) then
          continue
        end if
        col_end = min(col_start + 4, ncols)

        do sind = 1, num_suites
          do index = 1, size(test_suites(sind)%suite_parts)
            if (errcode == 0) then
              call ccpp_physics_run( &
                  suite_name=test_suites(sind)%suite_name, &
                  group_name=test_suites(sind)%suite_parts(index), &
                  col_start=col_start, col_end=col_end, &
                  thread_num=1, nthreads=1, nphys_threads=1, &
                  errmsg=errmsg, errcode=errcode)
              if (errcode /= 0) then
                write(6, '(5a)') trim(test_suites(sind)%suite_name), &
                    '/', trim(test_suites(sind)%suite_parts(index)),&
                    ': ', trim(errmsg)
                exit
              end if
            end if
          end do
        end do
      end do
      ! Check indices
      call check_constituent_indices(test_scalar_const_index, test_const_indices, &
          errmsg, errcode)
      call check_errcode(subname // " check suite indices", errcode, errmsg, &
          errcode_final)

      do sind = 1, num_suites
        if (errcode == 0) then
          call ccpp_physics_timestep_final( &
              suite_name=test_suites(sind)%suite_name, &
              group_name='all', col_start=1, col_end=ncols, &
              thread_num=1, nthreads=1, nphys_threads=1, &
              errmsg=errmsg, errcode=errcode)
        end if
        if (errcode /= 0) then
          write(6, '(3a)') trim(test_suites(sind)%suite_name), ': ', &
              trim(errmsg)
          exit
        end if
      end do

      ! Run "dycore"
      if (errcode == 0) then
        call advect_constituents()
      end if
    end do ! End time step loop

    do sind = 1, num_suites
      if (errcode == 0) then
        call ccpp_physics_final( &
            suite_name=test_suites(sind)%suite_name, &
            group_name='all', col_start=1, col_end=ncols, &
            thread_num=1, nthreads=1, nphys_threads=1, &
            errmsg=errmsg, errcode=errcode)
        if (errcode /= 0) then
          write(6, '(3a)') test_suites(sind)%suite_parts(index), ': ', &
              trim(errmsg)
          write(6, '(2a)') 'An error occurred in ccpp_physics_final, ', &
              'Exiting...'
          exit
        end if
      end if
    end do

    do sind = 1, num_suites
      if (errcode == 0) then
        call ccpp_final(suite_name=test_suites(sind)%suite_name, &
            errmsg=errmsg, errcode=errcode)
        if (errcode /= 0) then
          write(6, '(3a)') test_suites(sind)%suite_parts(index), ': ', &
              trim(errmsg)
          write(6, '(2a)') 'An error occurred in ccpp_final, ', &
              'Exiting...'
          exit
        end if
      end if
    end do

    call ccpp_deallocate_dynamic_constituents()
    deallocate(host_constituents)

    if (errcode == 0) then
      ! Run finished without error, check answers
      if (compare_data(num_advected)) then
        write(6, *) 'Answers are correct!'
        errcode = 0
      else
        write(6, *) 'Answers are not correct!'
        errcode = -1
      end if
    end if

    ! Make sure "final" flag is non-zero if "errcode" is:
    if (errcode /= 0) then
      errcode_final = -1 ! Notify test script that a failure occured
    end if

    ! Set return value to False if any errors were found:
    retval = errcode_final == 0

  end subroutine test_host

end module test_prog
