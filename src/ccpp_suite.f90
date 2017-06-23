!
! This work (Common Community Physics Package), identified by NOAA, NCAR,
! CU/CIRES, is free of known copyright restrictions and is placed in the
! public domain.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
! THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
! IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
! CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
!

!>
!! @brief Physics suite infrastructure module.
!
module ccpp_suite

    use, intrinsic :: iso_c_binding,                                   &
                      only: c_ptr, c_char, c_null_ptr
    use            :: ccpp_types,                                      &
                      only: ccpp_suite_t
    use            :: ccpp_errors,                                     &
                      only: ccpp_error, ccpp_warn
    use            :: ccpp_strings,                                    &
                      only: ccpp_fstr, ccpp_cstr
    use            :: ccpp_dl,                                         &
                      only: ccpp_dl_open, ccpp_dl_close
    use            :: ccpp_xml

    implicit none

    private
    public :: ccpp_suite_init,                                         &
              ccpp_suite_fini,                                         &
              ccpp_suite_load,                                         &
              ccpp_suite_unload

    contains

    !>
    !! Suite initialization subroutine.
    !!
    !! @param[in]     filename The file name of the XML scheme file to load.
    !! @param[in,out] suite    The ccpp_suite_t type to initalize from
    !!                         the scheme XML file.
    !! @param[  out]  ierr     Integer error flag.
    !
    subroutine ccpp_suite_init(filename, suite, ierr)

        character(len=*),       intent(in)    :: filename
        type(ccpp_suite_t),     intent(inout) :: suite
        integer,                intent(  out) :: ierr

        integer                               :: i
        integer                               :: j
        integer                               :: k
        type(c_ptr)                           :: xml
        type(c_ptr)                           :: root
        type(c_ptr)                           :: ipd
        type(c_ptr)                           :: subcycle
        type(c_ptr)                           :: scheme
        type(c_ptr), target                   :: tmp
        character(len=*), parameter           :: err_msg =             &
            'Please validate the suite xml file: '

        ierr = 0
        tmp = c_null_ptr

        ! Load the xml document.
        ierr = ccpp_xml_load(ccpp_cstr(filename), xml, root)
        if (ierr /= 0) then
            call ccpp_error('Unable to load suite from: ' // trim(filename))
            return
        end if

        ! Parse the suite element
        call ccpp_xml_parse(root, suite, ierr)
        if (ierr /= 0) then
            call ccpp_error(err_msg // trim(filename))
            return
        end if

        ! Find the init subroutine
        call ccpp_xml_ele_find(root, ccpp_cstr(CCPP_XML_ELE_INIT), tmp, ierr)
        if (ierr == 0) then
           ! Get the init subroutine name
           call ccpp_xml_parse(tmp, suite%library, suite%version, &
                               suite%init, ierr)
            if (ierr /= 0) then
               call ccpp_error('Unable to load initialization subroutine')
               call ccpp_error(err_msg // trim(filename))
               return
            end if
        end if

        ! Find the fini subroutine
        call ccpp_xml_ele_find(root, ccpp_cstr(CCPP_XML_ELE_FINI), &
                               tmp, ierr)
        if (ierr == 0) then
        ! Get the fini subroutine name
            call ccpp_xml_parse(tmp, suite%library, suite%version, &
                                suite%fini, ierr)
            if (ierr /= 0) then
               call ccpp_error('Unable to load finalization subroutine')
               call ccpp_error(err_msg // trim(filename))
               return
            end if
        end if

        ! Find the first IPD
        call ccpp_xml_ele_find(root, CCPP_XML_ELE_IPD, ipd, ierr)
        if (ierr /= 0) then
            call ccpp_error('Unable to find first ipd')
            call ccpp_error(err_msg // trim(filename))
            return
        end if

        ! Loop over all IPDs
        do i=1, suite%ipds_max

            ! Parse the IPD
            call ccpp_xml_parse(ipd, suite%ipds_max, suite%ipds(i), ierr)
            if (ierr /= 0) then
                call ccpp_error(err_msg // trim(filename))
                return
            end if

            ! Find the first subcycle
            call ccpp_xml_ele_find(ipd, CCPP_XML_ELE_SUBCYCLE, subcycle, ierr)
            if (ierr /= 0) then
                call ccpp_error('Unable to locate element: ' &
                                // CCPP_XML_ELE_SUBCYCLE)
                call ccpp_error(err_msg // trim(filename))
                return
            end if

            ! Loop over all subcycles
            do j=1, suite%ipds(i)%subcycles_max

                ! Parse the subcycle
                call ccpp_xml_parse(subcycle,                    &
                                    suite%ipds(i)%subcycles_max, &
                                    suite%ipds(i)%subcycles(j),  &
                                    ierr)
                if (ierr /= 0) then
                    call ccpp_error(err_msg // trim(filename))
                    return
                end if

                ! Find the first scheme
                call ccpp_xml_ele_find(subcycle, CCPP_XML_ELE_SCHEME, &
                                       scheme, ierr)

                ! Loop over all scheme
                do k=1, suite%ipds(i)%subcycles(j)%schemes_max
                    ! Parse the scheme
                    call ccpp_xml_parse(scheme, suite%library, suite%version,  &
                                        suite%ipds(i)%subcycles(j)%schemes(k), &
                                        ierr)
                    ! Find the next scheme
                    call ccpp_xml_ele_next(scheme, CCPP_XML_ELE_SCHEME, &
                                           scheme, ierr)
                end do
                ! Find the next subcycle
                call ccpp_xml_ele_next(subcycle, CCPP_XML_ELE_SUBCYCLE, &
                                       subcycle, ierr)
            end do
            ! Find the next IPD
            call ccpp_xml_ele_next(ipd, CCPP_XML_ELE_IPD, ipd, ierr)
        end do

!        write(6, '(A, A, A, A, A, A, A)') &
!                 '<suite name="', trim(suite%name), &
!                 '" lib="', trim(suite%library),    &
!                 '" ver="', trim(suite%version),    &
!                 '">'
!
!        do i=1, suite%ipds_max
!            write(6, '(A, I0, A)') '  <ipd part="', suite%ipds(i)%part, '">'
!            do j=1, suite%ipds(i)%subcycles_max
!                write(6, '(A, I0, A)') '    <subcycle loop="', suite%ipds(i)%subcycles(j)%loop, '">'
!                do k=1, suite%ipds(i)%subcycles(j)%schemes_max
!                    write(6, '(A, A, A, A, A, A, A)') &
!                          '     <scheme lib="', &
!                          trim(suite%ipds(i)%subcycles(j)%schemes(k)%library), &
!                          '" ver="', &
!                          trim(suite%ipds(i)%subcycles(j)%schemes(k)%version), '">', &
!                          trim(suite%ipds(i)%subcycles(j)%schemes(k)%name), &
!                          '</scheme>'
!                end do
!                write(6, '(A)') '    </subcycle>'
!            end do
!            write(6, '(A)') '  </ipd>'
!        end do
!        write(6, '(A)') '</suite>'

        ierr = ccpp_xml_unload(xml)
        call ccpp_suite_load(suite, ierr)

    end subroutine ccpp_suite_init

    !>
    !! Suite finalization subroutine.
    !!
    !! @param[in,out] suite    The suite_t type to finialize.
    !! @param[   out] ierr     Integer error flag.
    !
    subroutine ccpp_suite_fini(suite, ierr)
        type(ccpp_suite_t),     intent(inout) :: suite
        integer,                intent(  out) :: ierr

        integer                               :: i
        integer                               :: j
        integer                               :: k

        ierr = 0

        call ccpp_suite_unload(suite, ierr)

        do i=1, suite%ipds_max
            do j=1, suite%ipds(i)%subcycles_max
                do k=1, suite%ipds(i)%subcycles(j)%schemes_max
                    if (allocated(suite%ipds(i)%subcycles(j)%schemes(k)%name)) then
                        deallocate(suite%ipds(i)%subcycles(j)%schemes(k)%name)
                    end if
                    if (allocated(suite%ipds(i)%subcycles(j)%schemes(k)%library)) &
                            then
                        deallocate(suite%ipds(i)%subcycles(j)%schemes(k)%library)
                    end if
                    if (allocated(suite%ipds(i)%subcycles(j)%schemes(k)%version)) &
                            then
                        deallocate(suite%ipds(i)%subcycles(j)%schemes(k)%version)
                    end if
                end do
                if (allocated(suite%ipds(i)%subcycles(j)%schemes)) then
                    deallocate(suite%ipds(i)%subcycles(j)%schemes)
                end if
            end do
            if (allocated(suite%ipds(i)%subcycles)) then
                deallocate(suite%ipds(i)%subcycles)
            end if
        end do

        if (allocated(suite%ipds)) then
            deallocate(suite%ipds)
        end if

        ! Clean up the init
        if (allocated(suite%init%name)) then
            deallocate(suite%init%name)
        end if

        if (allocated(suite%init%library)) then
            deallocate(suite%init%library)
        end if

        if (allocated(suite%init%version)) then
            deallocate(suite%init%version)
        end if

        ! Clean up the fini
        if (allocated(suite%fini%name)) then
            deallocate(suite%fini%name)
        end if

        if (allocated(suite%fini%library)) then
            deallocate(suite%fini%library)
        end if

        if (allocated(suite%fini%version)) then
            deallocate(suite%fini%version)
        end if

        ! Clean up ourself
        if (allocated(suite%name)) then
            deallocate(suite%name)
        end if

        if (allocated(suite%library)) then
            deallocate(suite%library)
        end if

        if (allocated(suite%version)) then
            deallocate(suite%version)
        end if

        suite%ipd_n    = 0
        suite%ipds_max = 0

    end subroutine ccpp_suite_fini

    !>
    !! Suite sub-components loading.
    !!
    !! @param[in,out] suite    The suite_t type to load all sub-components.
    !! @param[   out] ierr     Integer error flag.
    !
    subroutine ccpp_suite_load(suite, ierr)
        type(ccpp_suite_t),     intent(inout) :: suite
        integer,                intent(  out) :: ierr

        integer                               :: i
        integer                               :: j
        integer                               :: k

        ierr = 0

        if (allocated(suite%init%name)) then
            ierr = ccpp_dl_open(ccpp_cstr(suite%init%name),    &
                                ccpp_cstr(suite%init%library), &
                                ccpp_cstr(suite%init%version), &
                                suite%init%scheme_hdl,         &
                                suite%init%library_hdl)
            if (ierr /= 0) then
                call ccpp_error('A problem occured loading '         &
                                // trim(suite%init%name) // ' from ' &
                                // trim(suite%init%library))
            end if
        end if

        if (allocated(suite%fini%name)) then
            ierr = ccpp_dl_open(ccpp_cstr(suite%fini%name),    &
                                ccpp_cstr(suite%fini%library), &
                                ccpp_cstr(suite%fini%version), &
                                suite%fini%scheme_hdl,         &
                                suite%fini%library_hdl)
            if (ierr /= 0) then
                call ccpp_error('A problem occured loading '         &
                                // trim(suite%fini%name) // ' from ' &
                                // trim(suite%fini%library))
            end if
        end if

        do i=1, suite%ipds_max
            do j=1, suite%ipds(i)%subcycles_max
                do k=1, suite%ipds(i)%subcycles(j)%schemes_max
                    associate (s => suite%ipds(i)%subcycles(j)%schemes(k))
                    ierr = ccpp_dl_open(ccpp_cstr(s%name),    &
                                        ccpp_cstr(s%library), &
                                        ccpp_cstr(s%version), &
                                        s%scheme_hdl,         &
                                        s%library_hdl)
                    if (ierr /= 0) then
                        call ccpp_error('A problem occured loading ' &
                                        // trim(s%name) // ' from '  &
                                        // trim(s%library))
                    end if
                    end associate
                end do
            end do
        end do

    end subroutine ccpp_suite_load

    !>
    !! Suite unload subroutine.
    !!
    !! This loops over all defined schemes to close
    !! the handle to the scheme library
    !!
    !! @param[in,out] cdata    The CCPP data of type ccpp_t
    !! @param[   out] ierr     Integer error flag
    !
    subroutine ccpp_suite_unload(suite, ierr)

        type(ccpp_suite_t), intent(inout)  :: suite

        integer                            :: ierr
        integer                            :: i
        integer                            :: j
        integer                            :: k

        ierr = 0

        if (allocated(suite%init%name)) then
            ierr = ccpp_dl_close(suite%init%library_hdl)
            if (ierr /= 0) then
                call ccpp_error('A problem occured closing '         &
                                // trim(suite%init%library))
            end if
        end if

        if (allocated(suite%fini%name)) then
            ierr = ccpp_dl_close(suite%fini%library_hdl)
            if (ierr /= 0) then
                call ccpp_error('A problem occured closing '         &
                                // trim(suite%fini%library))
            end if
        end if

        do i=1, suite%ipds_max
            do j=1, suite%ipds(i)%subcycles_max
                do k=1, suite%ipds(i)%subcycles(j)%schemes_max
                    associate (s => suite%ipds(i)%subcycles(j)%schemes(k))
                    ierr = ccpp_dl_close(s%library_hdl)
                    if (ierr /= 0) then
                        call ccpp_error('A problem occured closing ' &
                                        // trim(s%library))
                    end if
                    end associate
                end do
            end do
        end do

    end subroutine ccpp_suite_unload

end module ccpp_suite
