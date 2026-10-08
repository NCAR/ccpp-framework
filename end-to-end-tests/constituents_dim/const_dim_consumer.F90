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
  subroutine const_dim_consumer_run(cwork, awork, qbase, qtend, errmsg, errcode)
    real(kind=kind_phys), intent(in)  :: cwork(:)
    real(kind=kind_phys), intent(in)  :: awork(:)
    real(kind=kind_phys), intent(in)  :: qbase(:, :)
    real(kind=kind_phys), intent(in)  :: qtend(:, :)
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

    ! Rule (b): qbase (base constituent) and qtend (constituent tendency) carry
    ! NO constituent flag here; capgen infers them from the producer's flags and
    ! reads the same framework columns (vars_layer / vars_layer_tend).
    if (any(qbase /= 42.0_kind_phys)) then
      errcode = 1
      errmsg = 'rule b: unflagged base-constituent consumer read the wrong value'
      return
    end if
    if (any(qtend /= 7.0_kind_phys)) then
      errcode = 1
      errmsg = 'rule b: unflagged constituent-tendency consumer read the wrong value'
      return
    end if
  end subroutine const_dim_consumer_run

end module const_dim_consumer
