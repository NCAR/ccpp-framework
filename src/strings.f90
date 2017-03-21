!>
!! @breif String routines module.
!!
!! @details A module continaing subroutines and fuctions to
!!          manipulate strings.
!
module strings

    use, intrinsic :: iso_c_binding,                                   &
                      only: c_char, c_null_char

    implicit none

    private
    public :: fstr,                                                    &
              cstr

    contains

    !>
    !! fstr converts an array of characters into a string.
    !!
    !! This function is needed to pass C char arrys to Fortran.
    !!
    !! \param[in]  str1 The character array.
    !! \returns    str2 The fortran string.
    !
    function fstr(str1) result(str2)
        character(kind=c_char),               intent(in)  :: str1(:)
        character(len=:), allocatable                     :: str2

        integer                            :: ierr
        integer                            :: i ! Temporary loop indexer
        integer                            :: n ! Length of the str1

        n = size(str1)

        allocate(character(n) :: str2, stat=ierr)
        if (ierr /= 0) then
            print *, 'Error unable to allocate'
        end if

        i = 1
        do i=1,n
            str2(i:i) = str1(i)
        enddo

    end function fstr

    !>
    !! cstr converts a string to a trimmed null terminated string.
    !!
    !! This function is needed to pass Fortran strings to C.
    !!
    !! \param[in]  str1 The fortran string.
    !! \returns    str2 The trimmed, null terminated string.
    !
    function cstr(str1) result(str2)
        character(len=*)                            :: str1
        character(len=:), allocatable               :: str2

        str2 = trim(str1)//c_null_char
    end function cstr

end module strings
