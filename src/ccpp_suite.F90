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
                      only: c_ptr, c_null_ptr
    use            :: ccpp_types,                                      &
                      only: ccpp_suite_t
    use            :: ccpp_errors,                                     &
                      only: ccpp_error, ccpp_debug
    use            :: ccpp_strings,                                    &
                      only: ccpp_cstr
    use            :: ccpp_xml
    use            :: ccpp_scheme,                                     &
                      only: ccpp_scheme_init, ccpp_scheme_finalize,    &
                            ccpp_scheme_load, ccpp_scheme_unload

    implicit none

    private
    public :: ccpp_suite_init,                                         &
              ccpp_suite_finalize,                                     &
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

        implicit none

        character(len=*),       intent(in)    :: filename
        type(ccpp_suite_t),     intent(inout) :: suite
        integer,                intent(  out) :: ierr

        integer                               :: i
        integer                               :: j
        integer                               :: k
        integer                               :: l
        type(c_ptr)                           :: xml
        type(c_ptr)                           :: root
        type(c_ptr)                           :: group
        type(c_ptr)                           :: subcycle
        type(c_ptr)                           :: scheme
        type(c_ptr), target                   :: tmp
        character(len=*), parameter           :: err_msg =             &
            'Please validate the suite xml file: '

        ierr = 0
        tmp = c_null_ptr

        call ccpp_debug('Called ccpp_suite_init')

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
            ! Do not allow empty init constructs <<init></init>
            if (trim(suite%init%name) == '') then
                call ccpp_error('CCPP does not allow empty <init></init> ' &
                                // ' XML elements; remove if not used')
                ierr = 1
                return
            end if
            ! Initialize the scheme
            call ccpp_scheme_init(suite%init, ierr)
        end if

        ! Find the finalize subroutine
        call ccpp_xml_ele_find(root, ccpp_cstr(CCPP_XML_ELE_FINALIZE), &
                               tmp, ierr)
        if (ierr == 0) then
        ! Get the finalize subroutine name
            call ccpp_xml_parse(tmp, suite%library, suite%version, &
                                suite%finalize, ierr)
            if (ierr /= 0) then
               call ccpp_error('Unable to load finalization subroutine')
               call ccpp_error(err_msg // trim(filename))
               return
            end if
            ! Do not allow empty init constructs <<init></init>
            if (trim(suite%finalize%name) == '') then
                call ccpp_error('CCPP does not allow empty <finalize></finalize> ' &
                                // 'XML elements; remove if not used')
                ierr = 1
                return
            end if
            ! Initialize the scheme
            call ccpp_scheme_init(suite%finalize, ierr)
            if (ierr /= 0) return
        end if

        ! Find the first group
        call ccpp_xml_ele_find(root, CCPP_XML_ELE_GROUP, group, ierr)
        if (ierr /= 0) then
            call ccpp_error('Unable to find first group')
            call ccpp_error(err_msg // trim(filename))
            return
        end if

        ! Loop over all groups
        do i=1, suite%groups_max

            ! Parse the group
            call ccpp_xml_parse(group, suite%groups_max, suite%groups(i), ierr)
            if (ierr /= 0) then
                call ccpp_error(err_msg // trim(filename))
                return
            end if

            ! Find the first subcycle
            call ccpp_xml_ele_find(group, CCPP_XML_ELE_SUBCYCLE, subcycle, ierr)
            if (ierr /= 0) then
                call ccpp_error('Unable to locate element: ' &
                                // CCPP_XML_ELE_SUBCYCLE)
                call ccpp_error(err_msg // trim(filename))
                return
            end if

            ! Loop over all subcycles
            do j=1, suite%groups(i)%subcycles_max

                ! Parse the subcycle
                call ccpp_xml_parse(subcycle,                      &
                                    suite%groups(i)%subcycles_max, &
                                    suite%groups(i)%subcycles(j),  &
                                    ierr)
                if (ierr /= 0) then
                    call ccpp_error(err_msg // trim(filename))
                    return
                end if

                ! Find the first scheme
                call ccpp_xml_ele_find(subcycle, CCPP_XML_ELE_SCHEME, &
                                       scheme, ierr)

                ! Loop over all scheme
                do k=1, suite%groups(i)%subcycles(j)%schemes_max
                    ! Parse the scheme
                    call ccpp_xml_parse(scheme, suite%library, suite%version,    &
                                        suite%groups(i)%subcycles(j)%schemes(k), &
                                        ierr)

                    ! Initialize the scheme
                    call ccpp_scheme_init(suite%groups(i)%subcycles(j)%schemes(k), ierr)
                    if (ierr /= 0) return

                    ! Find the next scheme
                    call ccpp_xml_ele_next(scheme, CCPP_XML_ELE_SCHEME, &
                                           scheme, ierr)
                end do
                ! Find the next subcycle
                call ccpp_xml_ele_next(subcycle, CCPP_XML_ELE_SUBCYCLE, &
                                       subcycle, ierr)
            end do
            ! Find the next group
            call ccpp_xml_ele_next(group, CCPP_XML_ELE_GROUP, group, ierr)
        end do

#ifdef DEBUG
        write(6, '(A)') '--------------------------------------------------------------------------------'
        write(6, '(A)') 'CCPP suite configuration parsed from SDF ' // trim(filename)
        write(6, '(A)') '--------------------------------------------------------------------------------'
        write(6, '(*(A))') &
                 '<suite name="', trim(suite%name), &
                 '" lib="', trim(suite%library),    &
                 '" ver="', trim(suite%version),    &
                 '">'

        write(6, '(A, I0)') '[suite%groups_max] = ', suite%groups_max
        do i=1, suite%groups_max
            write(6, '(A, A, A)') '  <group name="', trim(suite%groups(i)%name), '">'
            write(6, '(A, I0)') '  [suite%groups(i)%subcycles_max] = ', suite%groups(i)%subcycles_max
            do j=1, suite%groups(i)%subcycles_max
                write(6, '(A, I0, A)') '    <subcycle loops_max="', suite%groups(i)%subcycles(j)%loops_max, '">'
                write(6, '(A, I0)') '    [suite%groups(i)%subcycles(j)%schemes_max] = ', &
                                                  suite%groups(i)%subcycles(j)%schemes_max
                do k=1, suite%groups(i)%subcycles(j)%schemes_max
                    write(6, '(*(A))') &
                          '      <scheme name="', &
                          trim(suite%groups(i)%subcycles(j)%schemes(k)%name), &
                          '" lib="', &
                          trim(suite%groups(i)%subcycles(j)%schemes(k)%library), &
                          '" ver="', &
                          trim(suite%groups(i)%subcycles(j)%schemes(k)%version), '">'
                    write(6, '(A, I0)') '    [suite%groups(i)%subcycles(j)%schemes(k)%functions_max] = ', &
                                                      suite%groups(i)%subcycles(j)%schemes(k)%functions_max
                    do l=1, suite%groups(i)%subcycles(j)%schemes(k)%functions_max
                        write(6, '(*(A))') &
                              '        <function>', &
                              trim(suite%groups(i)%subcycles(j)%schemes(k)%functions(l)%name), &
                              '</function>'
                    end do
                    write(6, '(A)') '      </scheme>'
                end do
                write(6, '(A)') '    </subcycle>'
            end do
            write(6, '(A)') '  </group>'
        end do
        write(6, '(A)') '</suite>'
        write(6, '(A)') '--------------------------------------------------------------------------------'
#endif

        ierr = ccpp_xml_unload(xml)
        call ccpp_suite_load(suite, ierr)

    end subroutine ccpp_suite_init

    !>
    !! Suite finalization subroutine.
    !!
    !! @param[in,out] suite    The suite_t type to finalize.
    !! @param[   out] ierr     Integer error flag.
    !
    subroutine ccpp_suite_finalize(suite, ierr)
        type(ccpp_suite_t),     intent(inout) :: suite
        integer,                intent(  out) :: ierr

        integer                               :: i
        integer                               :: j
        integer                               :: k

        ierr = 0

        call ccpp_debug('Called ccpp_suite_finalize')

        do i=1, suite%groups_max
            do j=1, suite%groups(i)%subcycles_max
                do k=1, suite%groups(i)%subcycles(j)%schemes_max
                    call ccpp_scheme_finalize(suite%groups(i)%subcycles(j)%schemes(k), ierr)
                    if (ierr /= 0) return
                    if (allocated(suite%groups(i)%subcycles(j)%schemes(k)%name)) then
                        deallocate(suite%groups(i)%subcycles(j)%schemes(k)%name)
                    end if
                    if (allocated(suite%groups(i)%subcycles(j)%schemes(k)%library)) &
                            then
                        deallocate(suite%groups(i)%subcycles(j)%schemes(k)%library)
                    end if
                    if (allocated(suite%groups(i)%subcycles(j)%schemes(k)%version)) &
                            then
                        deallocate(suite%groups(i)%subcycles(j)%schemes(k)%version)
                    end if
                end do
                if (allocated(suite%groups(i)%subcycles(j)%schemes)) then
                    deallocate(suite%groups(i)%subcycles(j)%schemes)
                end if
            end do
            if (allocated(suite%groups(i)%subcycles)) then
                deallocate(suite%groups(i)%subcycles)
            end if
        end do

        if (allocated(suite%groups)) then
            deallocate(suite%groups)
        end if

        ! Clean up the init scheme
        call ccpp_scheme_finalize(suite%init, ierr)
        if (ierr /=0) return

        if (allocated(suite%init%name)) then
            deallocate(suite%init%name)
        end if
        
        if (allocated(suite%init%library)) then
            deallocate(suite%init%library)
        end if
        
        if (allocated(suite%init%version)) then
            deallocate(suite%init%version)
        end if
        
        ! Clean up the finalize scheme
        call ccpp_scheme_finalize(suite%finalize, ierr)
        if (ierr /=0) return

        if (allocated(suite%finalize%name)) then
            deallocate(suite%finalize%name)
        end if
        
        if (allocated(suite%finalize%library)) then
            deallocate(suite%finalize%library)
        end if
        
        if (allocated(suite%finalize%version)) then
            deallocate(suite%finalize%version)
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

        suite%groups_max = 0

    end subroutine ccpp_suite_finalize

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

        call ccpp_debug('Called ccpp_suite_load')

        if (allocated(suite%init%name)) then
            call ccpp_scheme_load(suite%init, ierr)
            if (ierr /= 0) return
        end if
        
        if (allocated(suite%finalize%name)) then
            call ccpp_scheme_load(suite%finalize, ierr)
            if (ierr /= 0) return
        end if

        do i=1, suite%groups_max
            do j=1, suite%groups(i)%subcycles_max
                do k=1, suite%groups(i)%subcycles(j)%schemes_max
                    call ccpp_scheme_load(suite%groups(i)%subcycles(j)%schemes(k), ierr)
                    if (ierr /= 0) return
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
        integer           , intent(  out)  :: ierr

        integer                            :: i
        integer                            :: j
        integer                            :: k

        ierr = 0

        call ccpp_debug('Called ccpp_suite_unload')

        if (allocated(suite%init%name)) then
            call ccpp_scheme_unload(suite%init, ierr)
            if (ierr /= 0) return
        end if

        if (allocated(suite%finalize%name)) then
            call ccpp_scheme_unload(suite%finalize, ierr)
            if (ierr /= 0) return
        end if

        do i=1, suite%groups_max
            do j=1, suite%groups(i)%subcycles_max
                do k=1, suite%groups(i)%subcycles(j)%schemes_max
                    call ccpp_scheme_unload(suite%groups(i)%subcycles(j)%schemes(k), ierr)
                    if (ierr /= 0) return
                end do
            end do
        end do

    end subroutine ccpp_suite_unload

end module ccpp_suite
