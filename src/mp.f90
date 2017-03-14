module mp

    use, intrinsic :: iso_c_binding

    use            :: types,                                            &
                       only: aip_t
    implicit none

    private
    public :: mp_cap

    contains

    subroutine mp_cap(ptr) bind(c)
        implicit none
        type(c_ptr), intent(inout) :: ptr

        type(aip_t), pointer :: ap_data

        call c_f_pointer(ptr, ap_data)

        !print *, 'MP  Cap with aip_t name: ', ap_data%name

    end subroutine mp_cap

end module mp
