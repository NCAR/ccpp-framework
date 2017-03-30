module pbl

    use, intrinsic :: iso_c_binding

    use            :: types,                                            &
                       only: aip_t
    implicit none

    private
    public :: pbl_cap

    contains

    subroutine pbl_cap(ptr) bind(c)
        implicit none
        type(c_ptr), intent(inout) :: ptr

        type(aip_t), pointer :: sink

        call c_f_pointer(ptr, sink)

        !print *, 'PBL Cap with aip_t name: ', sink%name

    end subroutine pbl_cap

end module pbl
