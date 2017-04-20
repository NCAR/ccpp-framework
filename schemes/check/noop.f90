!>
!! @brief A NO-OP physics modules.
!!
!
module check_noop

    use, intrinsic :: iso_c_binding,                                   &
                      only: c_f_pointer, c_ptr
    use            :: ccpp_types,                                      &
                      only: ccpp_t, STR_LEN
    use            :: ccpp_fields,                                     &
                      only: ccpp_fields_get
    implicit none

    private
    public :: noop_cap

    contains

    subroutine noop_cap(ptr) bind(c)
        implicit none
        type(c_ptr), intent(inout) :: ptr

        type(ccpp_t), pointer      :: cdata

        call c_f_pointer(ptr, cdata)

        print *, 'In noop_cap'
        print *, cdata%suite%ipds(1)%subcycles(1)%schemes(1)%name

    end subroutine noop_cap

end module check_noop
