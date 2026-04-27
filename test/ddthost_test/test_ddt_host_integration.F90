program test
    use test_prog, only: test_host, suite_info, cm, cs

    implicit none

   character(len=cs), target :: test_parts1(2) = (/ 'physics1        ',       &
                                                    'physics2        ' /)
   character(len=cs), target :: test_parts2(1) = (/ 'data_prep       ' /)
   character(len=cm), target :: test_invars1(8) = (/                          &
        'potential_temperature                 ',                             &
        'potential_temperature_at_interface    ',                             &
        'coefficients_for_interpolation        ',                             &
        'index_of_water_vapor_specific_humidity',                             &
        'host_standard_ccpp_type               ',                             &
        'potential_temperature_increment       ',                             &
        'physics_state_derived_type            ',                             &
        'time_step_for_physics                 ' /)
   character(len=cm), target :: test_outvars1(8) = (/                         &
        'potential_temperature                 ',                             &
        'potential_temperature_at_interface    ',                             &
        'coefficients_for_interpolation        ',                             &
        'index_of_water_vapor_specific_humidity',                             &
        'host_standard_ccpp_type               ',                             &
        'physics_state_derived_type            ',	                      &
        'ccpp_error_code                       ',                             &
        'ccpp_error_message                    ' /)
   character(len=cm), target :: test_reqvars1(10) = (/                        &
        'potential_temperature                 ',                             &
        'potential_temperature_at_interface    ',                             &
        'coefficients_for_interpolation        ',                             &
        'index_of_water_vapor_specific_humidity',                             &
        'host_standard_ccpp_type               ',                             &        
        'potential_temperature_increment       ',                             &
        'physics_state_derived_type            ',                             &
        'time_step_for_physics                 ',                             &
        'ccpp_error_code                       ',                             &
        'ccpp_error_message                    ' /)

   character(len=cm), target :: test_invars2(4) = (/                          &
        'model_times                           ',                             &
        'number_of_model_times                 ',                             &
        'physics_state_derived_type            ',                             &
        'host_standard_ccpp_type               ' /)

   character(len=cm), target :: test_outvars2(6) = (/                         &
        'ccpp_error_code                       ',                             &
        'ccpp_error_message                    ',                             &
        'model_times                           ',                             &
        'physics_state_derived_type            ',                             &
	'host_standard_ccpp_type               ',                             &
        'number_of_model_times                 ' /)

   character(len=cm), target :: test_reqvars2(6) = (/                         &
        'model_times                           ',                             &
        'number_of_model_times                 ',                             &
        'ccpp_error_code                       ',                             &
        'ccpp_error_message                    ',                             &
        'physics_state_derived_type            ',     	      	      	      &
        'host_standard_ccpp_type               ' /)
    type(suite_info) :: test_suites(2)
    logical :: run_okay

    ! Setup expected test suite info
    test_suites(1)%suite_name = 'temp_suite'
	test_suites(1)%suite_parts => test_parts1
    test_suites(1)%suite_input_vars => test_invars1
    test_suites(1)%suite_output_vars => test_outvars1
    test_suites(1)%suite_required_vars => test_reqvars1
    test_suites(2)%suite_name = 'ddt_suite'
	test_suites(2)%suite_parts => test_parts2
    test_suites(2)%suite_input_vars => test_invars2
    test_suites(2)%suite_output_vars => test_outvars2
    test_suites(2)%suite_required_vars => test_reqvars2

    call test_host(run_okay, test_suites)

    if (run_okay) then
       STOP 0
    else
       STOP -1
    end if

end program test
