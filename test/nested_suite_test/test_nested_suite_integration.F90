program test_nested_suite_integration
    use test_prog, only: test_host, suite_info, cm, cs

    implicit none

    character(len=cs), target :: test_parts1(3) = (/                          &
        'radiation1      ',                                                   &
        'rad_lw_group    ',                                                   &
        'rad_sw_group    '/)

    character(len=cm), target :: test_invars1(5) = (/                         &
        'effective_radius_of_stratiform_cloud_snow_particle',                 &
        'physics_state_derived_type                        ',                 &
        'flag_indicating_cloud_microphysics_has_graupel    ',                 &
        'flag_indicating_cloud_microphysics_has_ice        ',                 &
        'num_subcycles_for_effr                            '/)

    character(len=cm), target :: test_outvars1(4) = (/                        &
        'effective_radius_of_stratiform_cloud_snow_particle',                 &
        'physics_state_derived_type                        ',                 &
        'ccpp_error_code                                   ',                 &
        'ccpp_error_message                                '/)

    character(len=cm), target :: test_reqvars1(7) = (/                        &
        'effective_radius_of_stratiform_cloud_snow_particle',                 &
        'physics_state_derived_type                        ',                 &
        'flag_indicating_cloud_microphysics_has_graupel    ',                 &
        'flag_indicating_cloud_microphysics_has_ice        ',                 &
        'num_subcycles_for_effr                            ',                 &
        'ccpp_error_code                                   ',                 &
        'ccpp_error_message                                '/)

    type(suite_info) :: test_suites(1)
    logical :: run_okay

    ! Setup expected test suite info
    test_suites(1)%suite_name = 'main_suite'
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
end program test_nested_suite_integration
