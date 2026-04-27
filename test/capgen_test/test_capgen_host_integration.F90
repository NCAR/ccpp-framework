program test
  use test_prog, only: test_host, &
      suite_info, &
      cm, &
      cs

  implicit none
  character(len=cs), target :: test_parts1(2) = (/ 'physics1        ', &
      'physics2        ' /)
  character(len=cs), target :: test_parts2(1) = (/ 'data_prep       ' /)
  character(len=cm), target :: test_invars1(10) = (/ &
      'array_variable_for_testing             ', & 
      'coefficients_for_interpolation         ', & 
      'physics_state_derived_type             ', & 
      'potential_temperature                  ', & 
      'potential_temperature_at_interface     ', & 
      'temperature_at_diagnostic_levels       ', & 
      'index_of_water_vapor_specific_humidity ', & 
      'do_cloud_fraction_adjustment           ', & 
      'potential_temperature_increment        ', & 
      'time_step_for_physics                  '/)

  character(len=cm), target :: test_outvars1(9) = (/ &
      'array_variable_for_testing             ', &
      'coefficients_for_interpolation         ', &
      'physics_state_derived_type             ', &
      'potential_temperature                  ', &
      'potential_temperature_at_interface     ', &
      'temperature_at_diagnostic_levels       ', &
      'index_of_water_vapor_specific_humidity ', &
      'ccpp_error_code                        ', &
      'ccpp_error_message                     '/)

  character(len=cm), target :: test_reqvars1(12) = (/ &
      'array_variable_for_testing             ', &
      'coefficients_for_interpolation         ', &
      'physics_state_derived_type             ', &
      'potential_temperature                  ', &
      'potential_temperature_at_interface     ', &
      'temperature_at_diagnostic_levels       ', &
      'index_of_water_vapor_specific_humidity ', &
      'do_cloud_fraction_adjustment           ', &
      'potential_temperature_increment        ', &
      'time_step_for_physics                  ', &
      'ccpp_error_code                        ', &
      'ccpp_error_message                     '/)

  character(len=cm), target :: test_invars2(3) = (/ &
      'model_times                            ', &
      'number_of_model_times                  ', &
      'physics_state_derived_type             ' /)

  character(len=cm), target :: test_outvars2(5) = (/ &
      'number_of_model_times                  ', &
      'physics_state_derived_type             ', &
      'ccpp_error_code                        ', &
      'ccpp_error_message                     ', &
      'model_times                            ' /)

  character(len=cm), target :: test_reqvars2(5) = (/ &
      'number_of_model_times                  ', &
      'physics_state_derived_type             ', &
      'model_times                            ', &
      'ccpp_error_code                        ', &
      'ccpp_error_message                     ' /)

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
    stop 0
  else
    stop -1
  end if

end program test
