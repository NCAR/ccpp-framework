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

module scm

    implicit none

    private
    public :: run_1, &
              run_2

    contains

    subroutine run_1(t, u, v, q_v)
        implicit none
        real, intent(inout) :: t(:), u(:), v(:), q_v(:)

        print *, 'In SCM_1_Run'


    end subroutine run_1

    subroutine run_2(t, u, v, q_v)
        implicit none
        real, intent(inout) :: t(:), u(:), v(:), q_v(:)

        print *, 'In SCM_2_Run'

    end subroutine run_2

end module scm
