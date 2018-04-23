!>
!! @brief The CCPP function call module.
!!
!! @details The CCPP routines for calling the specified
!!          physics group/subcyce/scheme.
!
module ccpp_fcall

    use, intrinsic :: iso_c_binding,                                   &
                      only: c_int32_t, c_char, c_ptr, c_loc, c_funptr
    use            :: ccpp_types,                                      &
                      only: ccpp_t, ccpp_suite_t, ccpp_group_t,        &
                            ccpp_subcycle_t, ccpp_scheme_t
    use            :: ccpp_errors,                                     &
                      only: ccpp_error, ccpp_debug
    use            :: ccpp_strings,                                    &
                      only: ccpp_cstr
    use            :: ccpp_dl,                                         &
                      only: ccpp_dl_call

    implicit none

    private
    public :: ccpp_run

    interface ccpp_run
        module procedure ccpp_run_suite,    &
                         ccpp_run_group,    &
                         ccpp_run_subcycle, &
                         ccpp_run_scheme,   &
                         ccpp_run_fptr
    end interface ccpp_run

    contains

    !>
    !! The run subroutine for a suite. This will call
    !! the all groups within a suite.
    !!
    !! @param[in    ] suite    The suite to run
    !! @param[in,out] cdata    The CCPP data of type ccpp_t
    !! @param[   out] ierr     Integer error flag
    !
    subroutine ccpp_run_suite(suite, cdata, ierr)

        type(ccpp_suite_t),    intent(inout)  :: suite
        type(ccpp_t), target,  intent(inout)  :: cdata
        integer,               intent(  out)  :: ierr

        integer                               :: i

        ierr = 0

        call ccpp_debug('Called ccpp_run_suite')

        do i=1,suite%groups_max
            suite%group_n = i 
            call ccpp_run_group(suite%groups(i), cdata, ierr)
            if (ierr /= 0) then
                return
            end if
        end do

    end subroutine ccpp_run_suite

    !>
    !! The run subroutine for a group. This will call
    !! the all subcycles within a group.
    !!
    !! @param[in    ] group    The group to run
    !! @param[in,out] cdata    The CCPP data of type ccpp_t
    !! @param[   out] ierr     Integer error flag
    !
    subroutine ccpp_run_group(group, cdata, ierr)

        type(ccpp_group_t),    intent(inout)  :: group
        type(ccpp_t), target,  intent(inout)  :: cdata
        integer,               intent(  out)  :: ierr

        integer                               :: i

        ierr = 0

        call ccpp_debug('Called ccpp_run_group')

        do i=1,group%subcycles_max
            group%subcycle_n = i
            call ccpp_run_subcycle(group%subcycles(i), cdata, ierr)
            if (ierr /= 0) then
                return
            end if
        end do

    end subroutine ccpp_run_group

    !>
    !! The run subroutine for a subcycle. This will call
    !! the all schemes within a subcycle. It will also
    !! loop if the loop attribut is greater than 1.
    !!
    !! @param[in    ] subcycle The subcycle to run
    !! @param[in,out] cdata    The CCPP data of type ccpp_t
    !! @param[   out] ierr     Integer error flag
    !
    subroutine ccpp_run_subcycle(subcycle, cdata, ierr)

        type(ccpp_subcycle_t), intent(inout)  :: subcycle
        type(ccpp_t), target,  intent(inout)  :: cdata
        integer,               intent(  out)  :: ierr

        integer                               :: i
        integer                               :: j

        ierr = 0

        call ccpp_debug('Called ccpp_run_subcycle')

        do i=1,subcycle%loop
            do j=1,subcycle%schemes_max
                subcycle%scheme_n = j
                call ccpp_run_scheme(subcycle%schemes(j), cdata, ierr)
                if (ierr /= 0) then
                    return
                end if
            end do
        end do

    end subroutine ccpp_run_subcycle

    !>
    !! The run subroutine for a scheme. This will call
    !! the single scheme specified.
    !!
    !! @param[in    ] scheme  The scheme to run
    !! @param[in,out] cdata   The CCPP data of type ccpp_t
    !! @param[   out] ierr    Integer error flag
    !
    subroutine ccpp_run_scheme(scheme, cdata, ierr)

        type(ccpp_scheme_t),  intent(in   )  :: scheme
        type(ccpp_t), target, intent(inout)  :: cdata
        integer,              intent(  out)  :: ierr

        ierr = 0

        call ccpp_debug('Called ccpp_run_scheme for "' // trim(scheme%name) // '"')

        ierr = ccpp_dl_call(scheme%scheme_hdl, c_loc(cdata))
        if (ierr /= 0) then
            call ccpp_error('A problem occured calling '// &
                            trim(scheme%name) //' scheme')
        end if

    end subroutine ccpp_run_scheme

    !>
    !! The run subroutine for a function pointer. This
    !! will call the single function specified.
    !!
    !! @param[in    ] scheme  The scheme to run
    !! @param[in,out] cdata   The CCPP data of type ccpp_t
    !! @param[   out] ierr    Integer error flag
    !
    subroutine ccpp_run_fptr(fptr, cdata, ierr)

        type(c_ptr),          intent(in   )  :: fptr
        type(ccpp_t), target, intent(inout)  :: cdata
        integer,              intent(  out)  :: ierr

        ierr = 0

        call ccpp_debug('Called ccpp_run_fptr')

        ierr = ccpp_dl_call(fptr, c_loc(cdata))
        if (ierr /= 0) then
            call ccpp_error('A problem occured calling function pointer')
        end if

    end subroutine ccpp_run_fptr

end module ccpp_fcall
