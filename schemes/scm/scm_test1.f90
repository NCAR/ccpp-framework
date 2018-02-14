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

module scm_test1

    use scm_test1_kinds, only: sp, dp, qp

    implicit none

    private
    public :: scm_test1_init, scm_test1_run, scm_test1_finalize

    contains

    subroutine scm_test1_init()
    end subroutine scm_test1_init

!> \section arg_table_scm_test1_run
!! | local var name | longname          | description                                           | units         | rank | type    |    kind   | intent | optional |
!! |----------------|-------------------|-------------------------------------------------------|---------------|------|---------|-----------|--------|----------|
!! | t              | air_temperature   | layer mean air temperature                            | K             |    1 | real    | dp | inout  | F        |
!! | u              | x_wind            | x component of layer wind                             | m s-1         |    1 | real    | dp | inout  | F        |
!! | v              | y_wind            | y component of layer wind                             | m s-1         |    1 | real    | dp | inout  | F        |
!!
    subroutine scm_test1_run(t, u, v)
        implicit none
        real(kind=dp), intent(inout) :: t(:), u(:), v(:)

        print *, 'In scm_test1_run'


    end subroutine scm_test1_run

    subroutine scm_test1_finalize()
    end subroutine scm_test1_finalize

end module scm_test1
