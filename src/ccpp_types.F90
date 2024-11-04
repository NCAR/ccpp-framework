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
!! @brief Type definitions module.
!!
!! @details The types module provides definitions for
!!          atmospheric driver to call the CCPP.
!
module ccpp_types

    ! use mpi_f08, only: MPI_Comm

!! \section arg_table_ccpp_types
!! \htmlinclude ccpp_types.html
!!

    implicit none

    private
    public :: ccpp_t, one
    ! public :: MPI_Comm

    !> @var Definition of constant one
    integer, parameter :: one = 1

    !> @var The default loop counter indicating outside of a subcycle loop
    integer, parameter :: CCPP_DEFAULT_LOOP_CNT = -999
    integer, parameter :: CCPP_DEFAULT_LOOP_MAX = -999

    !> @var The default values for block, chunk and thread numbers indicating invalid data
    integer, parameter :: CCPP_DEFAULT_BLOCK_NUMBER = -999
    integer, parameter :: CCPP_DEFAULT_CHUNK_NUMBER = -999
    integer, parameter :: CCPP_DEFAULT_THREAD_NUMBER = -999

    !> @var The default maximum number of threads for CCPP
    integer, parameter :: CCPP_DEFAULT_THREAD_COUNT = -999

!! \section arg_table_ccpp_t
!! \htmlinclude ccpp_t.html
!!
    !>
    !! @brief CCPP physics type.
    !!
    !! Generic type that contains all components to run the CCPP.
    !!
    !! - Array of fields to all the data needing to go
    !!   the physics drivers.
    !! - The suite definitions in a ccpp_suite_t type.
    !
    type :: ccpp_t
       ! CCPP-internal variables for physics schemes
       integer                                             :: errflg = 0
       character(len=512)                                  :: errmsg = ''
       integer                                             :: loop_cnt = CCPP_DEFAULT_LOOP_CNT
       integer                                             :: loop_max = CCPP_DEFAULT_LOOP_MAX
       integer                                             :: blk_no = CCPP_DEFAULT_BLOCK_NUMBER
       integer                                             :: chunk_no = CCPP_DEFAULT_CHUNK_NUMBER
       integer                                             :: thrd_no = CCPP_DEFAULT_THREAD_NUMBER
       integer                                             :: thrd_cnt = CCPP_DEFAULT_THREAD_COUNT
       integer                                             :: ccpp_instance = 1

    contains

       procedure :: initialized  => ccpp_t_initialized

    end type ccpp_t

contains

    function ccpp_t_initialized(ccpp_d) result(initialized)
       implicit none
       !
       class(ccpp_t) :: ccpp_d
       logical :: initialized
       !
       initialized = ccpp_d%thrd_no /= CCPP_DEFAULT_THREAD_NUMBER .or. &
                     ccpp_d%blk_no /= CCPP_DEFAULT_BLOCK_NUMBER .or. &
                     ccpp_d%chunk_no /= CCPP_DEFAULT_CHUNK_NUMBER
    end function ccpp_t_initialized

end module ccpp_types
