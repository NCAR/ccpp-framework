program test_xml

    use            :: ccpp_types,                          &
                      only: STR_LEN, aip_t
    use            :: ccpp_ipd,                            &
                      only: ccpp_ipd_init
    implicit none

    integer                :: ierr
    integer                :: len
    character(len=STR_LEN) :: filename
    type(aip_t), target    :: ap_data


    ierr = 0

    call get_command_argument(1, filename, len, ierr)
    if (ierr /= 0) then
        print *, 'Error: no suite XML file specified.'
        call exit(ierr)
    end if

    call ccpp_ipd_init(filename, ap_data%suite, ierr)
    if (ierr /= 0) then
        call exit(ierr)
    end if

end program test_xml
