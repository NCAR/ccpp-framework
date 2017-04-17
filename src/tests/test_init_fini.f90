!>
!! @brief A test program to test the CCPP.
!!
!! @details This will test the
!!          - initialization and finalization subroutines of
!!            -- CCPP
!!            -- Suite
!!            -- Fields
!!          It can be used multipile times to test the parsing
!!          of various suite XML files.
!
program test_init_fini

    use            :: ccpp_types,                          &
                      only: STR_LEN, ccpp_t
    use            :: ccpp,                                &
                      only: ccpp_init, ccpp_fini

    implicit none

    integer                :: ierr
    integer                :: len
    character(len=STR_LEN) :: filename
    type(ccpp_t), target   :: cdata


    ierr = 0

    call get_command_argument(1, filename, len, ierr)
    if (ierr /= 0) then
        print *, 'Error: no suite XML file specified.'
        call exit(ierr)
    end if

    call ccpp_init(filename, cdata, ierr)
    if (ierr /= 0) then
        call exit(ierr)
    end if

    call ccpp_fini(cdata, ierr)
    if (ierr /= 0) then
        call exit(ierr)
    end if

end program test_init_fini
