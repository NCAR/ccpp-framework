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
!! @brief A Test Atmospheric Driver Program.
!!
program test_check

!! \section arg_table_test_check
!! | local_name   | standard_name              | long_name                  | units   | rank | type      |   kind   | intent | optional |
!! |-------------------------------------------|----------------------------|---------|------|-----------|----------|--------|----------|
!! | gravity      | gravitational_acceleration | gravitational acceleration | m s-2   |    0 | real      |          | none   | F        |
!! | u            | x_wind                     | zonal wind                 | m s-1   |    2 | real      |          | none   | F        |
!! | v            | y_wind                     | meridional wind            | m s-1   |    2 | real      |          | none   | F        |
!! | tsfc         | surface_skin_temperature   | surface skin temperature   | K       |    1 | real      |          | none   | F        |
!!

    use, intrinsic :: iso_c_binding,                                   &
                      only: c_loc, c_f_pointer
    use            :: ccpp_api,                                        &
                      only: CCPP_STR_LEN,                              &
                            ccpp_t,                                    &
                            ccpp_init,                                 &
                            ccpp_finalize,                             &
                            ccpp_physics_init,                         &
                            ccpp_physics_run,                          &
                            ccpp_physics_finalize,                     &
                            ccpp_field_add

    implicit none

    type(ccpp_t), target                      :: cdata
    character(len=CCPP_STR_LEN)               :: filename
    integer                                   :: len
    integer                                   :: ierr
    integer                                   :: asize
    real, target                              :: gravity
    real, target, allocatable, dimension(:,:) :: u
    real, target, allocatable, dimension(:,:) :: v
    real, target, allocatable, dimension(:)   :: tsfc

    ierr = 0

    call get_command_argument(1, filename, len, ierr)
    if (ierr /= 0) then
            call exit(1)
    end if

    ! Allocate the data
    asize = 5
    allocate(tsfc(asize), stat=ierr)
    if (ierr /= 0) then
            print *, 'Unable to allocate surface temperature array'
            call exit(1)
    end if

    allocate(u(asize,asize), stat=ierr)
    if (ierr /= 0) then
            print *, 'Unable to allocate U array'
            call exit(1)
    end if

    allocate(v(asize,asize), stat=ierr)
    if (ierr /= 0) then
            print *, 'Unable to allocate U array'
            call exit(1)
    end if

    ! Generate data to pass into a physics driver
    gravity = 9.80665
    tsfc = [290.0, 291.0, 292.0, 293.0, 294.0]
    u = 0.0
    v = 10.0

    ! Initalize the CCPP framework (with the filename
    ! of the suite to load instead of the suite name)
    call ccpp_init(trim(filename), cdata, ierr, is_filename=.true.)
    if (ierr /= 0) then
        call exit(1)
    end if

    call ccpp_field_add(cdata, 'ccpp_error_flag', cdata%errflg, ierr, 'flag')
    if (ierr /= 0) then
        call exit(1)
    end if

    call ccpp_field_add(cdata, 'ccpp_error_message', cdata%errmsg, ierr, 'none')
    if (ierr /= 0) then
        call exit(1)
    end if

    call ccpp_field_add(cdata, 'ccpp_loop_counter', cdata%loop_cnt, ierr, 'index')
    if (ierr /= 0) then
        call exit(1)
    end if

    ! Add all the fields we want to expose to the physics driver.
    call ccpp_field_add(cdata, 'gravitational_acceleration', gravity, ierr, 'm s-2')
    if (ierr /= 0) then
        call exit(1)
    end if

    call ccpp_field_add(cdata, 'surface_skin_temperature', tsfc, ierr, 'K')
    if (ierr /= 0) then
        call exit(1)
    end if

    call ccpp_field_add(cdata, 'x_wind', u, ierr, 'm s-1')
    if (ierr /= 0) then
        call exit(1)
    end if

    call ccpp_field_add(cdata, 'y_wind', v, ierr, 'm s-1')
    if (ierr /= 0) then
        call exit(1)
    end if

    ! Initialize the test scheme
    call ccpp_physics_init(cdata, ierr=ierr)
    if (ierr /= 0) then
        call exit(1)
    end if

    ! Run the test scheme
    call ccpp_physics_run(cdata, scheme_name="test", ierr=ierr)
    if (ierr /= 0) then
        print *, "Call to scheme test failed, error message: '" // trim(cdata%errmsg) // "'"
        call exit(1)
    end if

    print *, 'In test dummy main'
    print *, 'gravity: ', gravity
    print *, 'tsfc:  ', tsfc(1:2)
    print *, 'u: ', u(1,1)
    print *, 'v: ', v(1,1)

    ! Finalize the test scheme
    call ccpp_physics_finalize(cdata, ierr=ierr)
    if (ierr /= 0) then
        call exit(1)
    end if

    ! Finalize the CCPP framework
    call ccpp_finalize(cdata, ierr)

    if (allocated(tsfc)) then
        deallocate(tsfc)
    end if

    if (allocated(u)) then
        deallocate(u)
    end if

    if (allocated(v)) then
        deallocate(v)
    end if

end program test_check
