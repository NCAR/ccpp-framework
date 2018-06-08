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
!! @brief Physics scheme infrastructure module.
!
module ccpp_scheme

    use            :: ccpp_types,                                      &
                      only: ccpp_scheme_t, CCPP_STAGES
    use            :: ccpp_errors,                                     &
                      only: ccpp_error, ccpp_debug
    use            :: ccpp_strings,                                    &
                      only: ccpp_cstr
    use            :: ccpp_dl,                                         &
                      only: ccpp_dl_open, ccpp_dl_close

    implicit none

    private
    public :: ccpp_scheme_init,                                        &
              ccpp_scheme_finalize,                                    &
              ccpp_scheme_load,                                        &
              ccpp_scheme_unload

    contains

    !>
    !! Scheme initialization subroutine.
    !!
    !! @param[in,out] scheme   The ccpp_scheme_t type to initalize
    !! @param[  out]  ierr     Integer error flag
    !
    subroutine ccpp_scheme_init(scheme, ierr)

        type(ccpp_scheme_t), intent(inout)  :: scheme
        integer            , intent(  out)  :: ierr

        integer                            :: i

        call ccpp_debug('Called ccpp_scheme_init')

        ierr = 0

        scheme%functions_max = size(CCPP_STAGES)

        allocate(scheme%functions(1:scheme%functions_max))
        do i=1,scheme%functions_max
            scheme%functions(i)%name = trim(scheme%get_function_name(trim(CCPP_STAGES(i))))
        end do

    end subroutine ccpp_scheme_init

    !>
    !! Scheme finalization subroutine.
    !!
    !! @param[in,out] scheme   The ccpp_scheme_t type to finalize
    !! @param[  out]  ierr     Integer error flag
    !
    subroutine ccpp_scheme_finalize(scheme, ierr)

        type(ccpp_scheme_t), intent(inout)  :: scheme
        integer            , intent(  out)  :: ierr

        integer                            :: i

        call ccpp_debug('Called ccpp_scheme_finalize')

        ierr = 0

        if (.not.(allocated(scheme%functions))) return

        do i=1,scheme%functions_max
            if (allocated(scheme%functions(i)%name)) &
                  deallocate(scheme%functions(i)%name)
        end do

        deallocate(scheme%functions)

    end subroutine ccpp_scheme_finalize

    !>
    !! Scheme loading subroutine.
    !!
    !! @param[in,out] scheme   The ccpp_scheme_t type to load
    !! @param[  out]  ierr     Integer error flag
    !
    subroutine ccpp_scheme_load(scheme, ierr)

        type(ccpp_scheme_t), intent(inout)  :: scheme
        integer            , intent(  out)  :: ierr

        integer                            :: i

        call ccpp_debug('Called ccpp_scheme_load')

        ierr = 0

        do i=1, scheme%functions_max
            associate (f => scheme%functions(i))
                call ccpp_debug('Loading ' // trim(f%name) &
                                // ' from ' // trim(scheme%library))
                ierr = ccpp_dl_open(ccpp_cstr(f%name),    &
                                    ccpp_cstr(scheme%library), &
                                    ccpp_cstr(scheme%version), &
                                    f%function_hdl,       &
                                    f%library_hdl)
                if (ierr /= 0) then
                    call ccpp_error('A problem occured loading ' &
                                    // trim(f%name) // ' from '  &
                                    // trim(scheme%library))
                    return
                end if
            end associate
        end do

    end subroutine ccpp_scheme_load

    !>
    !! Scheme unloading subroutine.
    !!
    !! @param[in,out] scheme   The ccpp_scheme_t type to unload
    !! @param[  out]  ierr     Integer error flag
    !
    subroutine ccpp_scheme_unload(scheme, ierr)

        type(ccpp_scheme_t), intent(inout)  :: scheme
        integer            , intent(  out)  :: ierr

        integer                            :: i

        call ccpp_debug('Called ccpp_scheme_unload')

        ierr = 0

        do i=1, scheme%functions_max
            associate (f => scheme%functions(i))
                ierr = ccpp_dl_close(f%library_hdl)
                if (ierr /= 0) then
                    call ccpp_error('A problem occured closing ' &
                                    // trim(scheme%library))
                    return
                end if
            end associate
        end do

    end subroutine ccpp_scheme_unload

end module ccpp_scheme
