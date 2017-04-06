!>
!! @brief The IPD module.
!!
!! @details The IPD routines for calling the specified
!!          physics schemes.
!
module ccpp_ipd

    use, intrinsic :: iso_c_binding,                                   &
                      only: c_int32_t, c_char, c_ptr, c_loc
    use            :: ccpp_types,                                      &
                      only: ccpp_t
    use            :: ccpp_errors,                                     &
                      only: ccpp_error
    use            :: ccpp_strings,                                    &
                      only: ccpp_cstr

    implicit none

    private
    public :: ccpp_ipd_run

    interface
        integer(c_int32_t)                                              &
        function ccpp_ipd_phy_cap                                       &
                   (scheme, cdata)                                      &
                   bind(c, name='ccpp_ipd_phy_cap')
            import :: c_char, c_int32_t, c_ptr
            character(kind=c_char), dimension(*)  :: scheme
            type(c_ptr)                           :: cdata
        end function ccpp_ipd_phy_cap
    end interface

    contains

    !>
    !! The run subroutine for the IPD. This will call
    !! the various physics schemes as specified in the
    !! suite data.
    !!
    !! @param[inout] cdata   The CCPP data of type ccpp_t
    !
    subroutine ccpp_ipd_run(cdata)

       type(ccpp_t), target, intent(inout)  :: cdata

       integer                              :: ierr
       integer                              :: i
       integer                              :: j
       integer                              :: k
       character(len=:), pointer            :: scheme
       type(c_ptr)                          :: ptr

       ierr = 0

       i = cdata%suite%ipd_n

       do j=1, cdata%suite%ipds(i)%subcycles_max
           cdata%suite%ipds(i)%subcycles_n = j
           do k=1, cdata%suite%ipds(i)%subcycles(j)%schemes_max
               cdata%suite%ipds(i)%subcycles(j)%schemes_n = k
               scheme => cdata%suite%ipds(i)%subcycles(j)%schemes(k)
               ptr = c_loc(cdata)
               ierr = ccpp_ipd_phy_cap(ccpp_cstr(scheme), ptr)
               if (ierr /= 0) then
                   call ccpp_error('A problem occured calling ' &
                                   // trim(scheme) // ' physics scheme')
               end if
           end do
       end do

    end subroutine ccpp_ipd_run

end module ccpp_ipd
