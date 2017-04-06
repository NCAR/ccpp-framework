module dummy
    use, intrinsic :: iso_c_binding

    use            :: kinds,                                            &
                      only: i_sp, r_dp
    use            :: ccpp_types,                                       &
                      only: aip_t
    use            :: ccpp_fields,                                      &
                      only: ccpp_field_data
    implicit none

    private
    public :: dummy_cap

    contains

    subroutine dummy_cap(ptr) bind(c)
        implicit none
        type(c_ptr), intent(inout) :: ptr

        type(aip_t),      pointer  :: ap_data
        real(kind=r_dp),  pointer  :: gravity
        real(kind=r_dp),  pointer  :: surf_t(:)
        real(kind=r_dp),  pointer  :: u(:,:,:)
        real(kind=r_dp),  pointer  :: v(:,:,:)
        integer :: i
        integer :: ierr

        call c_f_pointer(ptr, ap_data)

        call ccpp_field_data(ap_data, 'gravity', gravity, ierr)
        call ccpp_field_data(ap_data, 'surface_temperature', surf_t, ierr)
        call ccpp_field_data(ap_data, 'eastward_wind', u, ierr)
        call ccpp_field_data(ap_data, 'northward_wind', v, ierr)

        call dummy_run(gravity, u, v, surf_t)
    end subroutine dummy_cap

    subroutine dummy_run(gravity, u, v, surf_t)
        implicit none
        real(kind=r_dp), pointer, intent(inout) :: gravity
        real(kind=r_dp), pointer, intent(inout) :: surf_t(:)
        real(kind=r_dp), pointer, intent(inout) :: u(:,:,:)
        real(kind=r_dp), pointer, intent(inout) :: v(:,:,:)

        print *, 'In DUMMY'
        print *, 'gravity: ', gravity
        print *, 'surf_t:  ', surf_t
        print *, 'updating u to be 10m/s'
        u = 10.0_r_dp
        print *, 'updating v to be -10m/s'
        v = -10.0_r_dp

    end subroutine dummy_run

end module dummy
