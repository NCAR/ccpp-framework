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
                      only: ccpp_t
    use            :: ccpp_suite,                                      &
                      only: ccpp_suite_init, ccpp_suite_fini
    use            :: ccpp_fields,                                     &
                      only: ccpp_fields_init, ccpp_fields_fini
    use            :: ccpp_ipd,                                        &
                      only: ccpp_ipd_init, ccpp_ipd_fini
    use            :: ccpp_errors,                                     &
                      only: ccpp_error

    implicit none

    private
    public :: ccpp_init,                                               &
              ccpp_fini

    contains

    !>
    !! CCPP initialization subroutine.
    !!
    !! @param[in]     filename The file name of the XML scheme file to load.
    !! @param[in,out] cdata    The ccpp_t type data.
    !! @param[  out]  ierr     Integer error flag.
    !
    subroutine ccpp_init(filename, cdata, ierr)
        character(len=*),       intent(in)    :: filename
        type(ccpp_t),           intent(inout) :: cdata
        integer,                intent(  out) :: ierr

        ierr = 0

        ! Initialize the suite
        call ccpp_suite_init(filename, cdata%suite, ierr)
        if (ierr /= 0) then
                call ccpp_error('In initializing the CCPP suite')
                return
        end if

        ! Initialize the scheme calls
        call ccpp_ipd_init(cdata, ierr)
        if (ierr /= 0) then
                call ccpp_error('In initializing the CCPP scheme calls')
                return
        end if

        ! Initialize the fields
        call ccpp_fields_init(cdata, ierr)
        if (ierr /= 0) then
                call ccpp_error('In initializing the CCPP fields')
                return
        end if

    end subroutine ccpp_init

    !>
    !! CCPP finalization subroutine.
    !!
    !! @param[in,out] cdata    The ccpp_t type data.
    !! @param[  out]  ierr     Integer error flag.
    !
    subroutine ccpp_fini(cdata, ierr)
        type(ccpp_t),           intent(inout) :: cdata
        integer,                intent(  out) :: ierr

        ierr = 0

        ! Finalize the suite
        call ccpp_suite_fini(cdata%suite, ierr)
        if (ierr /= 0) then
                call ccpp_error('In finalizing the CCPP suite')
                return
        end if

        ! Finalize the scheme calls
        call ccpp_ipd_fini(cdata, ierr)
        if (ierr /= 0) then
                call ccpp_error('In finalizing the CCPP scheme calls')
                return
        end if

        ! Finalize the fields
        call ccpp_fields_fini(cdata, ierr)
        if (ierr /= 0) then
                call ccpp_error('In finalizing the CCPP fields')
                return
        end if

    end subroutine ccpp_fini

end module ccpp
