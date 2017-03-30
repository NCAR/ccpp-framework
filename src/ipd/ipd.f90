!>
!! @brief The IPD module.
!!
!! @details The IPD routines for calling the specified
!!          physics schemes.
!
module ipd
    use, intrinsic :: iso_c_binding
    use            :: types
    use            :: phy_infra, only: phy_init

    implicit none
    private
    public :: ipd_init, &
              ipd_run,  &
              ipd_fini

    interface
        integer(c_int32_t)                                              &
        function ipd_phy_c                                              &
                   (scheme, ap_data)                                    &
                   bind(c, name='ipd_phy')
            import :: c_char, c_int32_t, c_ptr
            character(kind=c_char), dimension(*)  :: scheme
            type(c_ptr)                           :: ap_data
        end function ipd_phy_c
    end interface

    contains

    !>
    !! The initialization subroutine for the IPD.
    !!
    !! @param[inout] ap_data   The aip_t data.
    !
    subroutine ipd_init(filename, suite)

        use            :: types

        character(len=*),       intent(in)    :: filename
        type(suite_t),          intent(inout) :: suite

        call phy_init(filename, suite)


    end subroutine ipd_init

    !>
    !! The run subroutine for the IPD. This will call
    !! the various physics schemes as specified in the
    !! suite data.
    !!
    !! @param[in]    suite     The suite data.
    !! @param[inout] ap_data   The aip_t data.
    !
    subroutine ipd_run(ap_data)

        use, intrinsic :: iso_c_binding
        use            :: types
        use            :: strings

        type(aip_t), target, intent(inout)  :: ap_data
        integer                             :: ierr
        integer                             :: i
        integer                             :: j
        integer                             :: k
        character(len=STR_LEN)              :: scheme
        type(c_ptr)                         :: ptr

        ierr = 0

        i = ap_data%suite%ipd_n

        do j=1, ap_data%suite%ipds(i)%subcycles_max
            ap_data%suite%ipds(i)%subcycles_n = j
            do k=1, ap_data%suite%ipds(i)%subcycles(j)%schemes_max
                ap_data%suite%ipds(i)%subcycles(j)%schemes_n = k
                scheme = ap_data%suite%ipds(i)%subcycles(j)%schemes(k)
                ptr = c_loc(ap_data)
                ierr = ipd_phy_c(cstr(scheme), ptr)
                if (ierr /= 0) then
                    print *, 'Bork, bork, bork'
                end if
            end do
        end do

    end subroutine ipd_run

    !>
    !! The finalization subroutine for the IPD.
    !! This will destroy the ap_data containing all
    !! the physics scheme call order.
    !!
    !! @param[inout] ap_data   The aip_t data.
    !
    subroutine ipd_fini(ap_data)

        use            :: types

        type(aip_t), intent(inout) :: ap_data


    end subroutine ipd_fini

end module ipd
