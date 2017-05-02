!>
!! @brief String routines module.
!!
!! @details A module continaing subroutines and fuctions to
!!          manipulate strings.
!
module ccpp_strings

    use, intrinsic :: iso_c_binding,                                   &
                      only: c_char, c_null_char, c_size_t,             &
                            c_f_pointer, c_ptr
    use            :: ccpp_errors,                                     &
                      only: ccpp_error

    implicit none

    private
    public :: ccpp_fstr,                                               &
              ccpp_cstr

    interface
        integer(c_size_t)                                              &
        function strlen(s)                                             &
                 bind(c, name='strlen')
            import :: c_size_t, c_ptr
            type(c_ptr), value :: s
        end function strlen
    end interface

    contains

    !>
    !! ccpp_fstr converts an array of characters into a string.
    !!
    !! This function is needed to pass C char arrays to Fortran.
    !!
    !! @param[in]  str1 The character array.
    !! @returns    str2 The fortran string.
    !
    function ccpp_fstr(str1) result(str2)
        type(c_ptr),            intent(in) :: str1
        character(len=:), allocatable      :: str2

        integer                            :: ierr
        integer                            :: i ! Temporary loop indexer
        integer                            :: n ! Length of the str1
        character(kind=c_char), pointer    :: cstr(:)

        n = strlen(str1)

        call c_f_pointer(str1, cstr, [strlen(str1)])

        allocate(character(n) :: str2, stat=ierr)
        if (ierr /= 0) then
            call ccpp_error('Unable to allocate a string')
            return
        end if

        i = 1
        do i=1,n
            str2(i:i) = cstr(i)
        enddo

    end function ccpp_fstr

    !>
    !! ccpp_cstr converts a string to a trimmed null terminated string.
    !!
    !! This function is needed to pass Fortran strings to C.
    !!
    !! @param[in]  str1 The fortran string.
    !! @returns    str2 The trimmed, null terminated string.
    !
    function ccpp_cstr(str1) result(str2)
        character(len=*)                            :: str1
        character(len=:), allocatable               :: str2

        str2 = trim(str1)//c_null_char
    end function ccpp_cstr

end module ccpp_strings
