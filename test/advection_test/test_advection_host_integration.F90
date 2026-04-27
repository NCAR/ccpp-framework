program test
    use test_prog, only: test_host, suite_info, cm, cs

    implicit none

   character(len=cs), target :: test_parts1(1)
   character(len=cm), target :: test_invars1(8)
   character(len=cm), target :: test_outvars1(11)
   character(len=cm), target :: test_reqvars1(14)

    type(suite_info) :: test_suites(1)
    logical :: run_okay

    test_parts1 = (/ 'physics         '/)

    test_invars1 = (/                                                         &
        'ccpp_model_constituents_object           ',                          &
        'cloud_ice_dry_mixing_ratio               ',                          &
        'cloud_liquid_dry_mixing_ratio            ',                          &
        'physics_state_derived_type               ',                          &
        'tendency_of_cloud_liquid_dry_mixing_ratio',                          &
        'banana_array_dim                         ',                          &
        'time_step_for_physics                    ',                          &
        'water_temperature_at_freezing            ' /)
    test_outvars1 = (/                                                        &
        'ccpp_model_constituents_object           ',                          &
        'cloud_ice_dry_mixing_ratio               ',                          &
        'cloud_liquid_dry_mixing_ratio            ',                          &
        'physics_state_derived_type               ',                          &
        'tendency_of_cloud_liquid_dry_mixing_ratio',                          &
        'ccpp_error_code                          ',                          &
        'ccpp_error_message                       ',                          &
        'dynamic_constituents_for_cld_ice         ',                          &
        'dynamic_constituents_for_cld_liq         ',                          &
        'test_banana_constituent_index            ',                          &
        'test_banana_constituent_indices          ' /)
    test_reqvars1 = (/                                                        &
        'ccpp_model_constituents_object           ',                          &
        'cloud_ice_dry_mixing_ratio               ',                          &
        'cloud_liquid_dry_mixing_ratio            ',                          &
        'physics_state_derived_type               ',                          &
        'tendency_of_cloud_liquid_dry_mixing_ratio',                          &
        'banana_array_dim                         ',                          &
        'time_step_for_physics                    ',                          &
        'water_temperature_at_freezing            ',                          &
        'ccpp_error_code                          ',                          &
        'ccpp_error_message                       ',                          &
        'dynamic_constituents_for_cld_ice         ',                          &
        'dynamic_constituents_for_cld_liq         ',                          &
        'test_banana_constituent_index            ',                          &
        'test_banana_constituent_indices          ' /)

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
