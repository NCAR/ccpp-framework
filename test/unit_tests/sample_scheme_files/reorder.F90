MODULE reorder

  USE ccpp_kinds, ONLY: kind_phys

  IMPLICIT NONE
  PRIVATE

  PUBLIC :: reorder_init
  PUBLIC :: reorder_run
  PUBLIC :: reorder_finalize

CONTAINS

  !> \section arg_table_reorder_run  Argument Table
  !! \htmlinclude arg_table_reorder_run.html
  !!
  subroutine reorder_run(foo, timestep, temp_prev, temp_layer, qv, ps,    &
       errmsg, errflg)

    integer,            intent(in)    :: foo
    real(kind_phys),    intent(in)    :: timestep
    real(kind_phys),    intent(inout) :: qv(:)
    real(kind_phys),    intent(inout) :: ps(:)
    REAL(kind_phys),    intent(in)    :: temp_prev(:)
    REAL(kind_phys),    intent(inout) :: temp_layer(foo)
    character(len=512), intent(out)   :: errmsg
    integer,            intent(out)   :: errflg
    !----------------------------------------------------------------

    integer :: col_index

    errmsg = ''
    errflg = 0

    do col_index = 1, foo
       temp_layer(col_index) = temp_layer(col_index) + temp_prev(col_index)
       qv(col_index) = qv(col_index) + 1.0_kind_phys
    end do

  END SUBROUTINE reorder_run

  !> \section arg_table_reorder_init  Argument Table
  !! \htmlinclude arg_table_reorder_init.html
  !!
  subroutine reorder_init (errmsg, errflg)

    character(len=512),      intent(out)   :: errmsg
    integer,                 intent(out)   :: errflg

    errmsg = ''
    errflg = 0

  end subroutine reorder_init

  !> \section arg_table_reorder_finalize  Argument Table
  !! \htmlinclude arg_table_reorder_finalize.html
  !!
  subroutine reorder_finalize (errmsg, errflg)

    character(len=512),      intent(out)   :: errmsg
    integer,                 intent(out)   :: errflg

    errmsg = ''
    errflg = 0

  end subroutine reorder_finalize

END MODULE reorder
