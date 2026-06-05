!>\file const_dim_consumer.F90
!! Consumes the two suite-owned workspaces produced by const_dim_producer and
!! verifies their contents. Both are dimensioned by number_of_ccpp_constituents;
!! cwork was allocated by the framework (init_fields, Case 2a) and awork by the
!! producing scheme (_run, Case 2b). Receiving them through plain (non-allocatable)
!! dummies exercises capgen passing the allocated components to a consumer.

module const_dim_consumer

  use ccpp_kinds, only: kind_phys

  implicit none

  private
  public :: const_dim_consumer_run

contains

  !! \section arg_table_const_dim_consumer_run Argument Table
  !! \htmlinclude const_dim_consumer_run.html
  !!
  subroutine const_dim_consumer_run(cwork, awork, errmsg, errcode)
    real(kind=kind_phys), intent(in)  :: cwork(:)
    real(kind=kind_phys), intent(in)  :: awork(:)
    character(len=*),     intent(out) :: errmsg
    integer,              intent(out) :: errcode

    integer :: m

    errmsg = ''
    errcode = 0

    ! Case 2a: framework-allocated suite workspace, filled by the producer.
    do m = 1, size(cwork)
      if (cwork(m) /= real(10 * m, kind_phys)) then
        errcode = 1
        errmsg = 'Case 2a: framework-allocated suite workspace has wrong value'
        return
      end if
    end do

    ! Case 2b: scheme-allocated suite workspace, filled by the producer.
    do m = 1, size(awork)
      if (awork(m) /= real(100 * m, kind_phys)) then
        errcode = 1
        errmsg = 'Case 2b: scheme-allocated suite workspace has wrong value'
        return
      end if
    end do
  end subroutine const_dim_consumer_run

end module const_dim_consumer
