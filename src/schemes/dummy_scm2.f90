module dummy_scm2
    use, intrinsic :: iso_c_binding

    use            :: kinds,                                            &
                      only: i_sp, r_dp
    use            :: types,                                            &
                      only: aip_t
    use            :: phy_fields,                                       &
                      only: phy_field_data
    implicit none

    private
    public :: dummy_scm2_cap

    contains

    subroutine dummy_scm2_cap(ptr) bind(c)
        implicit none
        type(c_ptr), intent(inout) :: ptr

        type(aip_t),      pointer  :: ap_data
        real(kind=r_dp),  pointer  :: t(:), u(:), v(:), q_v(:)

        call c_f_pointer(ptr, ap_data)

        call phy_field_data(ap_data, 'temperature', t)
        call phy_field_data(ap_data, 'eastward_wind', u)
        call phy_field_data(ap_data, 'northward_wind', v)
        call phy_field_data(ap_data, 'water_vapor_specific_humidity', q_v)

        call dummy_scm2_run(t, u, v, q_v)
    end subroutine dummy_scm2_cap

    subroutine dummy_scm2_run(t, u, v, q_v)
        implicit none
        real(kind=r_dp), pointer, intent(inout) :: t(:), u(:), v(:), q_v(:)

        print *, 'In DUMMY_SCM2'


    end subroutine dummy_scm2_run

end module dummy_scm2
