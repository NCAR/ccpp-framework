module sfc

    use, intrinsic :: iso_c_binding

    use            :: types,                                            &
                       only: aip_t
    implicit none

    private
    public :: sfc_cap

    contains

    subroutine sfc_cap(ptr) bind(c)
        implicit none
        type(c_ptr), intent(inout) :: ptr

        type(aip_t), pointer :: ap_data

        call c_f_pointer(ptr, ap_data)

        !print *, 'SFC Cap with aip_t name: ', ap_data%name

    end subroutine sfc_cap

end module sfc
