!>
!! @brief The IPD module.
!!
!! @details The IPD routines for calling the specified
!!          physics schemes.
!
module ccpp_ipd
    use, intrinsic :: iso_c_binding
    use            :: ccpp_types
    use            :: ccpp_errors
    use            :: ccpp_strings
    use            :: ccpp_phy_infra, only: ccpp_phy_init

    implicit none
    private
    public :: ccpp_ipd_init, &
              ccpp_ipd_run,  &
              ccpp_ipd_fini

    interface
        integer(c_int32_t)                                              &
        function ccpp_ipd_phy_cap                                       &
                   (scheme, ap_data)                                    &
                   bind(c, name='ccpp_ipd_phy_cap')
            import :: c_char, c_int32_t, c_ptr
            character(kind=c_char), dimension(*)  :: scheme
            type(c_ptr)                           :: ap_data
        end function ccpp_ipd_phy_cap
    end interface

    contains

    !>
    !! The initialization subroutine for the IPD.
    !!
    !! @param[in]  filename  The IPD scheme filename
    !! @param[out] suite     The IPD suite
    !! @param[out] ierr      Returned error status
    !
    subroutine ccpp_ipd_init(filename, suite, ierr)

       character(len=*),       intent(in)    :: filename
       type(suite_t),          intent(  out) :: suite
       integer,                intent(  out) :: ierr

       ierr = 0
       call ccpp_phy_init(filename, suite, ierr)

    end subroutine ccpp_ipd_init

    !>
    !! The run subroutine for the IPD. This will call
    !! the various physics schemes as specified in the
    !! suite data.
    !!
    !! @param[inout] ap_data   The aip_t data.
    !
    subroutine ccpp_ipd_run(ap_data)

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
               ierr = ccpp_ipd_phy_cap(ccpp_cstr(scheme), ptr)
               if (ierr /= 0) then
                   call ccpp_error('A problem occured calling ' &
                                   // trim(scheme) // ' physics scheme')
               end if
           end do
       end do

    end subroutine ccpp_ipd_run

    !>
    !! The finalization subroutine for the IPD.
    !! This will destroy the ap_data containing all
    !! the physics scheme call order.
    !!
    !! @param[inout] ap_data   The aip_t data.
    !
    subroutine ccpp_ipd_fini(ap_data)

       type(aip_t), intent(inout) :: ap_data

    end subroutine ccpp_ipd_fini

end module ccpp_ipd
