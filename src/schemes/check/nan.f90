!>
!! @brief A physics module to check for NaNs.
!!
!
module check_nans

    use, intrinsic :: iso_c_binding,                                   &
                      only: c_f_pointer, c_ptr
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
        real,             pointer  :: v(:,:,:)
        integer                    :: i
        integer                    :: ierr

        call c_f_pointer(ptr, cdata)

        call ccpp_fields_get(cdata, 'northward_wind', v, ierr)

        call test_run(gravity, u, v, surf_t)

    end subroutine nans_cap

    subroutine run(gravity, u, v, surf_t)
        implicit none
        real, pointer, intent(inout) :: gravity
        real, pointer, intent(inout) :: surf_t(:)
        real, pointer, intent(inout) :: u(:,:,:)
        real, pointer, intent(inout) :: v(:,:,:)

        print *, 'In physics check nans run'

    end subroutine test_run

end module check_test
