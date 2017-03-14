module dummy
    use, intrinsic :: iso_c_binding

    use            :: kinds,                                            &
                      only: i_sp, r_dp
    use            :: types,                                            &
                      only: aip_t
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

        call c_f_pointer(ptr, ap_data)

        call c_f_pointer(ap_data%fields(1), gravity)
        call c_f_pointer(ap_data%fields(2), surf_t, [5])
        call c_f_pointer(ap_data%fields(3), u, [5,5,5])
        call c_f_pointer(ap_data%fields(4), v, [5,5,5])

        call dummy_run(u, v, gravity, surf_t)
    end subroutine dummy_cap

    subroutine dummy_run(u, v, gravity, surf_t)
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
