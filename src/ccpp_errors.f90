!>
!! @brief Error/Warning reporting module.
!!
!! @details Subroutines for reporting warnings.
!
module ccpp_errors
    use, intrinsic :: iso_fortran_env,                                 &
                      only: error_unit

    implicit none

    private
    public :: ccpp_error,                                              &
              ccpp_warn,                                               &
              ccpp_if_error,                                           &
              ccpp_if_warn

    contains

    !>
    !! Fatal error reporting.
    !!
    !! Write an error message to error_unit/stderr.
    !!
    !! @param[in] message   The error message to write.
    !
    subroutine ccpp_error(message)
        character(len=*),        intent(in) :: message

        write(error_unit, *) 'ERROR: ', trim(message)
    end subroutine ccpp_error

    !>
    !! Non-fatal warning reporting.
    !!
    !! Write an warning message to error_unit/stderr.
    !!
    !! @param[in] message   The warning message to write.
    !
    subroutine ccpp_warn(message)
        character(len=*),        intent(in) :: message

        write(error_unit, *) 'WARN: ', trim(message)
    end subroutine ccpp_warn

    !>
    !! Fatal error checking and reporting.
    !!
    !! Check to see if ierr is non-zero. If it is
    !! write an error message to error_unit/stderr.
    !!
    !! @param[in] ierr      The exit code.
    !! @param[in] message   The error message to write.
    !
    subroutine ccpp_if_error(ierr, message)
        integer,                 intent(in) :: ierr
        character(len=*),        intent(in) :: message

        if (ierr /= 0) then
            write(error_unit, *) 'ERROR: ', trim(message)
        end if

    end subroutine ccpp_if_error

    !>
    !! Non-fatal warning checking and reporting.
    !!
    !! Check to see if ierr is non-zero. If it is
    !! write an warning message to error_unit/stderr.
    !!
    !! @param[in] ierr      The exit code.
    !! @param[in] message   The warning message to write.
    !
    subroutine ccpp_if_warn(ierr, message)
        integer,                 intent(in) :: ierr
        character(len=*),        intent(in) :: message

        if (ierr /= 0) then
            write(error_unit, *) 'WARN: ', trim(message)
        end if

    end subroutine ccpp_if_warn

end module ccpp_errors
