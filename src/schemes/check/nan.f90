!>
!! @brief A physics module to check for NaNs.
!!
!
module check_nans

    use, intrinsic :: iso_c_binding,                                   &
                      only: c_f_pointer, c_ptr
    use            :: kinds,                                           &
                      only: i_sp, r_dp
    use            :: ccpp_types,                                      &
                      only: ccpp_t
    use            :: ccpp_fields,                                     &
                      only: ccpp_fields_get
    implicit none

    private
    public :: nans_cap

    contains

    subroutine nans_cap(ptr) bind(c)
        implicit none
        type(c_ptr), intent(inout) :: ptr

        type(ccpp_t),     pointer  :: cdata
        real(kind=r_dp),  pointer  :: v(:,:,:)
        integer                    :: i
        integer                    :: ierr

        call c_f_pointer(ptr, cdata)

        call ccpp_fields_get(cdata, 'gravity', gravity, ierr)
        call ccpp_fields_get(cdata, 'surface_temperature', surf_t, ierr)
        call ccpp_fields_get(cdata, 'eastward_wind', u, ierr)
        call ccpp_fields_get(cdata, 'northward_wind', v, ierr)

        call test_run(gravity, u, v, surf_t)

    end subroutine nans_cap

    subroutine run(gravity, u, v, surf_t)
        implicit none
        real(kind=r_dp), pointer, intent(inout) :: gravity
        real(kind=r_dp), pointer, intent(inout) :: surf_t(:)
        real(kind=r_dp), pointer, intent(inout) :: u(:,:,:)
        real(kind=r_dp), pointer, intent(inout) :: v(:,:,:)

        print *, 'In physics test_run'
        print *, 'gravity: ', gravity
        print *, 'surf_t:  ', surf_t
        print *, 'updating u to be 10m/s'
        u = 10.0_r_dp
        print *, 'updating v to be -10m/s'
        v = -10.0_r_dp

    end subroutine test_run

end module check_test
