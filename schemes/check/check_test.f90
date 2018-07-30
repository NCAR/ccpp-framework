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
!! @brief A checking physics modules.
!!
!
module test

    implicit none

    private
    public :: test_init, test_run, test_finalize

    contains

    subroutine test_init()
    end subroutine test_init

    subroutine test_finalize()
    end subroutine test_finalize

!! \section arg_table_test_run
!! | local_name   | standard_name              | long_name                                | units   | rank | type      |   kind   | intent | optional |
!! |-------------------------------------------|------------------------------------------|---------|------|-----------|----------|--------|----------|
!! | gravity      | gravitational_acceleration | gravitational acceleration               | m s-2   |    0 | real      |          | in     | F        |
!! | u            | x_wind                     | zonal wind                               | m s-1   |    2 | real      |          | inout  | F        |
!! | v            | y_wind                     | meridional wind                          | m s-1   |    2 | real      |          | inout  | F        |
!! | tsfc         | surface_skin_temperature   | surface skin temperature                 | K       |    1 | real      |          | in     | T        |
!! | errflg       | ccpp_error_flag            | error flag for error handling in CCPP    | flag    |    0 | integer   |          | out    | F        |
!! | errmsg       | ccpp_error_message         | error message for error handling in CCPP | none    |    0 | character | len=*    | out    | F        |
!!
    subroutine test_run(gravity, u, v, tsfc, errflg, errmsg)
        implicit none
        real,             intent(inout) :: gravity
        real,             intent(inout) :: u(:,:)
        real,             intent(inout) :: v(:,:)
        real,             intent(in)    :: tsfc(:)
        integer,          intent(out)   :: errflg
        character(len=*), intent(out)   :: errmsg

        errflg = 0
        errmsg = ''

        print *, 'In physics test_run'
        print *, 'gravity: ', gravity
        print *, 'tsfc:    ', tsfc
        print *, 'updating u to be 10m/s'
        u = 10.0
        print *, 'updating v to be -10m/s'
        v = -10.0

    end subroutine test_run

end module test
