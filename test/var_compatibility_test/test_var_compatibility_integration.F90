program test_var_compatibility_integration
    use test_prog, only: test_host, suite_info, cm, cs

    implicit none

    character(len=cs), target :: test_parts1(1) = (/ 'radiation       ' /)

    character(len=cm), target :: test_invars1(18) = (/                         &
        'effective_radius_of_stratiform_cloud_rain_particle        ',         &
        'effective_radius_of_stratiform_cloud_liquid_water_particle',         &
        'effective_radius_of_stratiform_cloud_snow_particle        ',         &
        'effective_radius_of_stratiform_cloud_graupel              ',         &
        'cloud_graupel_number_concentration                        ',         &
        'scalar_variable_for_testing                               ',         &
        'turbulent_kinetic_energy                                  ',         &
        'turbulent_kinetic_energy2                                 ',         &
        'scalar_variable_for_testing_a                             ',         &
        'scalar_variable_for_testing_b                             ',         &
        'scalar_variable_for_testing_c                             ',         &
        'scheme_order_in_suite                                     ',         &
        'num_subcycles_for_effr                                    ',         &
        'flag_indicating_cloud_microphysics_has_graupel            ',         &
        'flag_indicating_cloud_microphysics_has_ice                ',         &
        'surface_downwelling_shortwave_radiation_flux              ',         &
        'surface_upwelling_shortwave_radiation_flux                ',         &
        'longwave_radiation_fluxes                                 '/)

    character(len=cm), target :: test_outvars1(14) = (/                        &
        'ccpp_error_code                                           ',         &
        'ccpp_error_message                                        ',         &
        'effective_radius_of_stratiform_cloud_ice_particle         ',         &
        'effective_radius_of_stratiform_cloud_liquid_water_particle',         &
        'effective_radius_of_stratiform_cloud_rain_particle        ',         &
        'effective_radius_of_stratiform_cloud_snow_particle        ',         &
        'cloud_ice_number_concentration                            ',         &
        'scalar_variable_for_testing                               ',         &
        'scheme_order_in_suite                                     ',         &
        'surface_downwelling_shortwave_radiation_flux              ', 	      &
        'surface_upwelling_shortwave_radiation_flux                ', 	      &
        'turbulent_kinetic_energy                                  ',         &
        'turbulent_kinetic_energy2                                 ',         &
        'longwave_radiation_fluxes                                 '/)

    character(len=cm), target :: test_reqvars1(22) = (/                        &
        'ccpp_error_code                                           ',         &
        'ccpp_error_message                                        ',         &
        'effective_radius_of_stratiform_cloud_rain_particle        ',         &
        'effective_radius_of_stratiform_cloud_ice_particle         ',         &
        'effective_radius_of_stratiform_cloud_liquid_water_particle',         &
        'effective_radius_of_stratiform_cloud_snow_particle        ',         &
        'effective_radius_of_stratiform_cloud_graupel              ',         &
        'cloud_graupel_number_concentration                        ',         &
        'cloud_ice_number_concentration                            ',         &
        'scalar_variable_for_testing                               ',         &
        'turbulent_kinetic_energy                                  ',         &
        'turbulent_kinetic_energy2                                 ',         &
        'scalar_variable_for_testing_a                             ',         &
        'scalar_variable_for_testing_b                             ',         &
        'scalar_variable_for_testing_c                             ',         &
        'scheme_order_in_suite                                     ', 	      &
        'num_subcycles_for_effr                                    ',         &
        'flag_indicating_cloud_microphysics_has_graupel            ',         &
        'flag_indicating_cloud_microphysics_has_ice                ',         &
        'surface_downwelling_shortwave_radiation_flux              ', 	      &
        'surface_upwelling_shortwave_radiation_flux                ', 	      &
        'longwave_radiation_fluxes                                 '/)

    type(suite_info) :: test_suites(1)
    logical :: run_okay

    ! Setup expected test suite info
    test_suites(1)%suite_name = 'var_compatibility_suite'
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
end program test_var_compatibility_integration
