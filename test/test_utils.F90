module test_utils

    public :: check_list

contains
    logical function check_list(test_list, chk_list, list_desc, suite_name)
    ! Check a list (<test_list>) against its expected value (<chk_list>)

        ! Dummy arguments
        character(len=*),           intent(in) :: test_list(:)
        character(len=*),           intent(in) :: chk_list(:)
        character(len=*),           intent(in) :: list_desc
        character(len=*), optional, intent(in) :: suite_name

        ! Local variables
        logical                                :: found
        integer                                :: num_items
        integer                                :: lindex, tindex
        integer,          allocatable          :: check_unique(:)
        character(len=2)                       :: sep
        character(len=256)                     :: errmsg

        check_list = .true.
        errmsg = ''

        ! Check the list size
        num_items = size(chk_list)
        if (size(test_list) /= num_items) then
            write(errmsg, '(a,i0,2a)') 'ERROR: Found ', size(test_list),         &
                    ' ', trim(list_desc)
            if (present(suite_name)) then
                write(errmsg(len_trim(errmsg)+1:), '(2a)') ' for suite, ',        &
                    trim(suite_name)
            end if
            write(errmsg(len_trim(errmsg)+1:), '(a,i0)') ', should be ', num_items
            write(6, *) trim(errmsg)
            errmsg = ''
            check_list = .false.
        end if

        ! Now, check the list contents for 1-1 correspondence
        if (check_list) then
            allocate(check_unique(num_items))
            check_unique = -1
            do lindex = 1, num_items
            found = .false.
            do tindex = 1, num_items
                if (trim(test_list(lindex)) == trim(chk_list(tindex))) then
                    check_unique(tindex) = lindex
                    found = .true.
                    exit
                end if
            end do
            if (.not. found) then
                check_list = .false.
                write(errmsg, '(5a)') 'ERROR: ', trim(list_desc), ' item, ',  &
                    trim(test_list(lindex)), ', was not found'
                if (present(suite_name)) then
                    write(errmsg(len_trim(errmsg)+1:), '(2a)') ' in suite, ',  &
                        trim(suite_name)
                end if
                write(6, *) trim(errmsg)
                errmsg = ''
            end if
            end do
            if (check_list .and. any(check_unique < 0)) then
            check_list = .false.
            write(errmsg, '(3a)') 'ERROR: The following ', trim(list_desc),  &
                    ' items were not found'
            if (present(suite_name)) then
                write(errmsg(len_trim(errmsg)+1:), '(2a)') ' in suite, ',     &
                    trim(suite_name)
            end if
            sep = '; '
            do lindex = 1, num_items
                if (check_unique(lindex) < 0) then
                    write(errmsg(len_trim(errmsg)+1:), '(2a)') sep,            &
                        trim(chk_list(lindex))
                    sep = ', '
                end if
            end do
            write(6, *) trim(errmsg)
            errmsg = ''
            end if
        end if

    end function check_list
end module test_utils