program test
    use test_prog, only: test_host, suite_info, cm, cs

    implicit none

   character(len=cs), target :: test_parts1(2) = (/ 'physics1        ',       &
                                                    'physics2        ' /)
   character(len=cs), target :: test_parts2(1) = (/ 'data_prep       ' /)
   character(len=cm), target :: test_invars1(10) = (/                         &
        'potential_temperature               ',                               &
        'potential_temperature_at_interface  ',                               &
        'coefficients_for_interpolation      ',                               &
        'surface_air_pressure                ',                               &
        'water_vapor_specific_humidity       ',                               &
        'potential_temperature_increment     ',                               &
        'soil_levels                         ',                               &
        'temperature_at_diagnostic_levels    ',                               &
        'time_step_for_physics               ',                               &
        'array_variable_for_testing          ' /)
   character(len=cm), target :: test_outvars1(10) = (/                        &
        'potential_temperature               ',                               &
        'potential_temperature_at_interface  ',                               &
        'coefficients_for_interpolation      ',                               &
        'surface_air_pressure                ',                               &
        'water_vapor_specific_humidity       ',                               &
        'soil_levels                         ',                               &
        'temperature_at_diagnostic_levels    ',                               &
        'ccpp_error_code                     ',                               &
        'ccpp_error_message                  ',                               &
        'array_variable_for_testing          ' /)
   character(len=cm), target :: test_reqvars1(12) = (/                        &
        'potential_temperature               ',                               &
        'potential_temperature_at_interface  ',                               &
        'coefficients_for_interpolation      ',                               &
        'surface_air_pressure                ',                               &
        'water_vapor_specific_humidity       ',                               &
        'potential_temperature_increment     ',                               &
        'time_step_for_physics               ',                               &
        'soil_levels                         ',                               &
        'temperature_at_diagnostic_levels    ',	      	      	              &
        'ccpp_error_code                     ',                               &
        'ccpp_error_message                  ',                               &
        'array_variable_for_testing          ' /)

   character(len=cm), target :: test_invars2(3) = (/                          &
        'model_times                         ',                               &
        'number_of_model_times               ',                               &
        'surface_air_pressure                ' /)

   character(len=cm), target :: test_outvars2(5) = (/                         &
        'ccpp_error_code                     ',                               &
        'ccpp_error_message                  ',                               &
        'model_times                         ',                               &
        'surface_air_pressure                ',                               &
        'number_of_model_times               ' /)

   character(len=cm), target :: test_reqvars2(5) = (/                         &
        'model_times                         ',                               &
        'number_of_model_times               ',                               &
        'surface_air_pressure                ',                               &
        'ccpp_error_code                     ',                               &
        'ccpp_error_message                  ' /)
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
