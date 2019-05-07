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
!! @brief The CCPP library main entry and exit points.
!!
!
module ccpp

    use, intrinsic :: iso_c_binding,                                   &
                      only: c_ptr
    use            :: ccpp_types,                                      &
                      only: ccpp_t, ccpp_suite_t
    use            :: ccpp_suite,                                      &
                      only: ccpp_suite_init, ccpp_suite_finalize
    use            :: ccpp_fields,                                     &
                      only: ccpp_fields_init, ccpp_fields_finalize
    use            :: ccpp_errors,                                     &
                      only: ccpp_error, ccpp_debug

    implicit none

    private
    public :: ccpp_init,                                               &
              ccpp_finalize,                                           &
              ccpp_initialized

    contains

    !>
    !! CCPP initialization subroutine.
    !!
    !! @param[in]     suitename    The suite name to use/load
    !! @param[in,out] cdata        The ccpp_t type data.
    !! @param[  out]  ierr         Integer error flag.
    !! @param[in]     cdata_target An optional cdata instance to cope the suite from
    !! @param[in]     is_filename  Switch to interpret suitename as filename/filepath
    !!                             (for dynamic build only, default value .false.)
    !
    subroutine ccpp_init(suitename, cdata, ierr, cdata_target, is_filename)
        character(len=*),     intent(in)           :: suitename
        type(ccpp_t), target, intent(inout)        :: cdata
        integer,              intent(  out)        :: ierr
        type(ccpp_t), target, intent(in), optional :: cdata_target
        logical,              intent(in), optional :: is_filename
        ! Local variables
        logical            :: is_filename_local
        character(len=256) :: filename_local

        ierr = 0

        call ccpp_debug('Called ccpp_init')

#ifndef STATIC
        if (present(is_filename)) then
            is_filename_local = is_filename
        else
            is_filename_local = .false.
        end if

        if (is_filename_local) then
            if (len(trim(suitename))>len(filename_local)) then
                call ccpp_error('Length of suitename=filename exceeds length of local filename variable')
                ierr = 1
                return
            end if
            filename_local = trim(suitename)
        else
            if (len('./suite_' // trim(suitename) // '.xml')>len(filename_local)) then
                call ccpp_error('Length of suitename + 12 exceeds length of local filename variable')
                ierr = 1
                return
            end if
            filename_local = './suite_' // trim(suitename) // '.xml'
        end if

        if (present(cdata_target)) then
            ! Copy the suite from the target cdata instance
            cdata%suite => cdata_target%suite
            cdata%suite_iscopy = .True.
        else
            ! Initialize the suite from the file
            cdata%suite => cdata%suite_target
            cdata%suite_iscopy = .False.
            call ccpp_suite_init(filename_local, cdata%suite, ierr)
            if (ierr /= 0) then
                call ccpp_error('In initializing the CCPP suite')
                return
            end if
        end if

        ! Initialize the fields
        call ccpp_fields_init(cdata, ierr)
        if (ierr /= 0) then
            call ccpp_error('In initializing the CCPP fields')
            return
        end if
#endif

        ! Set flag indicating initialization state of cdata
        cdata%initialized = .true.

    end subroutine ccpp_init

    !>
    !! CCPP finalization subroutine.
    !!
    !! @param[in,out] cdata    The ccpp_t type data.
    !! @param[  out]  ierr     Integer error flag.
    !
    subroutine ccpp_finalize(cdata, ierr)
        type(ccpp_t), target, intent(inout) :: cdata
        integer,              intent(  out) :: ierr

        ierr = 0

        call ccpp_debug('Called ccpp_finalize')

#ifndef STATIC
        if (cdata%suite_iscopy) then
           nullify(cdata%suite)
           cdata%suite_iscopy = .False.
           return
        end if

        ! Finalize the suite
        call ccpp_suite_finalize(cdata%suite, ierr)
        if (ierr /= 0) then
                call ccpp_error('In finalizing the CCPP suite')
                return
        end if

        ! Finalize the fields
        call ccpp_fields_finalize(cdata, ierr)
        if (ierr /= 0) then
                call ccpp_error('In finalizing the CCPP fields')
                return
        end if

        nullify(cdata%suite)
#endif

        ! Set flag indicating initialization state of cdata
        cdata%initialized = .false.

    end subroutine ccpp_finalize

    !>
    !! CCPP test initialization routine
    !!
    !! @param[in]     cdata        The ccpp_t type data
    !! @return        initialized  .true. or .false.
    !
    function ccpp_initialized(cdata) result(initialized)
        type(ccpp_t), target, intent(in) :: cdata
        logical                          :: initialized

        call ccpp_debug('Called ccpp_initialized')

        initialized = cdata%initialized

    end function ccpp_initialized

end module ccpp
