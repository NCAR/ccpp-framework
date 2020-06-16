! Test parameterization with no vertical level
!

MODULE mismatch_intent

  USE ccpp_kinds, ONLY: kind_phys

  IMPLICIT NONE
  PRIVATE

  PUBLIC :: mismatch_intent_init
  PUBLIC :: mismatch_intent_run
  PUBLIC :: mismatch_intent_finalize

CONTAINS

  !> \section arg_table_mismatch_intent_run  Argument Table
  !! \htmlinclude arg_table_mismatch_intent_run.html
  !!
  subroutine mismatch_intent_run(foo, timestep, temp_prev, temp_layer, qv, ps,    &
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

  END SUBROUTINE mismatch_intent_run

  !> \section arg_table_mismatch_intent_init  Argument Table
  !! \htmlinclude arg_table_mismatch_intent_init.html
  !!
  subroutine mismatch_intent_init (errmsg, errflg)

    character(len=512),      intent(out)   :: errmsg
    integer,                 intent(out)   :: errflg

    ! This routine currently does nothing

    errmsg = ''
    errflg = 0

  end subroutine mismatch_intent_init

  !> \section arg_table_mismatch_intent_finalize  Argument Table
  !! \htmlinclude arg_table_mismatch_intent_finalize.html
  !!
  subroutine mismatch_intent_finalize (errmsg, errflg)

    character(len=512),      intent(out)   :: errmsg
    integer,                 intent(out)   :: errflg

    ! This routine currently does nothing

    errmsg = ''
    errflg = 0

  end subroutine mismatch_intent_finalize

END MODULE mismatch_intent
