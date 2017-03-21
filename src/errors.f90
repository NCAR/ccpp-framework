!>
!! @brief Error reporting module.
!!
!! @details Subroutines for reporting fatal errors and warnings.
!!          All compilers tested have the subroutine exit()
!!          implemented. If they did not, we would have to use
!!          iso_c_binding to bind to exit() in C.
!
module errors
    use, intrinsic :: iso_fortran_env,                                 &
                      only: error_unit

    implicit none

    private
    public :: err,                                                     &
              warn,                                                    &
              if_err,                                                  &
              if_warn

    contains

    !>
    !! Fatal error reporting and exit.
    !!
    !! Write an error message to error_unit/stderr
    !! then exit with the specified error code.
    !!
    !! @param[in] ierr      The exit code.
    !! @param[in] message   The error message to write.
    !
    subroutine err(ierr, message)
        integer,                 intent(in) :: ierr
        character(len=*),        intent(in) :: message

        write(error_unit, *) 'ERROR: ', trim(message)
        call exit(ierr)

    end subroutine err

    !>
    !! Non-fatal warning reporting.
    !!
    !! Write an error message to error_unit/stderr.
    !!
    !! @param[in] message   The error message to write.
    !
    subroutine warn(message)
        character(len=*),        intent(in) :: message

        write(error_unit, *) 'WARN: ', trim(message)
    end subroutine warn

    !>
    !! Fatal error checking, reporting and exit.
    !!
    !! Check to see if ierr is non-zero. If it is
    !! write an error message to error_unit/stderr
    !! then exit with the specified error code.
    !!
    !! @param[in] ierr      The exit code.
    !! @param[in] message   The error message to write.
    !
    subroutine if_err(ierr, message)
        integer,                 intent(in) :: ierr
        character(len=*),        intent(in) :: message

        if (ierr /= 0) then
            write(error_unit, *) 'ERROR: ', trim(message)
            call exit(ierr)
        end if

    end subroutine if_err

    !>
    !! Non-fatal error checking and reporting.
    !!
    !! Check to see if ierr is non-zero. If it is
    !! write an error message to error_unit/stderr.
    !!
    !! @param[in] ierr      The exit code.
    !! @param[in] message   The error message to write.
    !
    subroutine if_warn(ierr, message)
        integer,                 intent(in) :: ierr
        character(len=*),        intent(in) :: message

        if (ierr /= 0) then
            write(error_unit, *) 'WARN: ', trim(message)
        end if

    end subroutine if_warn

end module errors
