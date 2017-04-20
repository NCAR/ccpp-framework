!>
!! @brief The IPD module.
!!
!! @details The IPD routines for calling the specified
!!          physics schemes.
!
module ccpp_ipd

    use, intrinsic :: iso_c_binding,                                   &
                      only: c_int32_t, c_char, c_ptr, c_loc, c_funptr
    use            :: ccpp_types,                                      &
                      only: ccpp_t, ccpp_scheme_t
    use            :: ccpp_errors,                                     &
                      only: ccpp_error
    use            :: ccpp_strings,                                    &
                      only: ccpp_cstr

    implicit none

    private
    public :: ccpp_ipd_init,                                           &
              ccpp_ipd_fini,                                           &
              ccpp_ipd_run

    interface
        integer(c_int32_t)                                             &
        function ccpp_ipd_open                                         &
                   (name, library, version, shdl, lhdl)                &
                   bind(c, name='ccpp_ipd_open')
            import :: c_char, c_int32_t, c_ptr, c_funptr
            character(kind=c_char), dimension(*)  :: name
            character(kind=c_char), dimension(*)  :: library
            character(kind=c_char), dimension(*)  :: version
            type(c_ptr)                           :: shdl
            type(c_ptr)                           :: lhdl
        end function ccpp_ipd_open

        integer(c_int32_t)                                             &
        function ccpp_ipd_close                                        &
                   (lhdl)                                              &
                   bind(c, name='ccpp_ipd_close')
            import :: c_int32_t, c_ptr, c_funptr
            type(c_ptr)                           :: lhdl
        end function ccpp_ipd_close

        integer(c_int32_t)                                             &
        function ccpp_ipd_cap                                          &
                   (shdl, cdata)                                       &
                   bind(c, name='ccpp_ipd_cap')
            import :: c_int32_t, c_ptr, c_funptr
            type(c_ptr)                           :: shdl
            type(c_ptr)                           :: cdata
        end function ccpp_ipd_cap
    end interface

    contains

    !>
    !! IPD initialization subroutine.
    !!
    !! This loops over all defined schemes to obtain
    !! a function pointer to them.
    !!
    !! @param[in,out] cdata    The CCPP data of type ccpp_t
    !! @param[  out]  ierr     Integer error flag
    !
    subroutine ccpp_ipd_init(cdata, ierr)

        type(ccpp_t), target, intent(inout)  :: cdata

        integer                              :: ierr
        integer                              :: i
        integer                              :: j
        integer                              :: k

        ierr = 0

        do i=1, cdata%suite%ipds_max
            do j=1, cdata%suite%ipds(i)%subcycles_max
                do k=1, cdata%suite%ipds(i)%subcycles(j)%schemes_max

                    associate (s => cdata%suite%ipds(i)%subcycles(j)%schemes(k))
                    ierr = ccpp_ipd_open(ccpp_cstr(s%name),    &
                                         ccpp_cstr(s%library), &
                                         ccpp_cstr(s%version), &
                                         s%scheme_hdl,         &
                                         s%library_hdl)
                    if (ierr /= 0) then
                        call ccpp_error('A problem occured loading ' &
                                        // trim(s%name) // ' from '  &
                                        // trim(s%library))
                    end if
                    end associate

                end do
            end do
        end do

    end subroutine ccpp_ipd_init

    !>
    !! IPD finalization subroutine.
    !!
    !! This loops over all defined schemes to close
    !! the handle to the scheme library
    !!
    !! @param[in,out] cdata    The CCPP data of type ccpp_t
    !! @param[   out] ierr     Integer error flag
    !
    subroutine ccpp_ipd_fini(cdata, ierr)

        type(ccpp_t), target, intent(inout)  :: cdata

        integer                              :: ierr
        integer                              :: i
        integer                              :: j
        integer                              :: k

        ierr = 0

        do i=1, cdata%suite%ipds_max
            do j=1, cdata%suite%ipds(i)%subcycles_max
                do k=1, cdata%suite%ipds(i)%subcycles(j)%schemes_max

                    associate (s => cdata%suite%ipds(i)%subcycles(j)%schemes(k))
                    ierr = ccpp_ipd_close(s%library_hdl)
                    if (ierr /= 0) then
                        call ccpp_error('A problem occured close ' &
                                        // trim(s%library))
                    end if
                    end associate
                end do
            end do
        end do

    end subroutine ccpp_ipd_fini

    !>
    !! The run subroutine for the IPD. This will call
    !! the various physics schemes as specified in the
    !! suite data.
    !!
    !! @param[in    ] scheme  The scheme to run
    !! @param[in,out] cdata   The CCPP data of type ccpp_t
    !! @param[   out] ierr     Integer error flag
    !
    subroutine ccpp_ipd_run(scheme, cdata, ierr)

        type(ccpp_scheme_t),  intent(in   )  :: scheme
        type(ccpp_t), target, intent(inout)  :: cdata
        integer,              intent(  out)  :: ierr

        ierr = 0

        ierr = ccpp_ipd_cap(scheme%scheme_hdl, c_loc(cdata))
        if (ierr /= 0) then
            call ccpp_error('A problem occured calling '// &
                            trim(scheme%name) //' scheme')
        end if

    end subroutine ccpp_ipd_run

end module ccpp_ipd
