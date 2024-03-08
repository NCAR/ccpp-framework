! Test parameterization with no vertical level
!

MODULE temp_adjust

  USE ccpp_kinds, ONLY: kind_phys

  IMPLICIT NONE
  PRIVATE

  PUBLIC :: temp_adjust_init
  PUBLIC :: temp_adjust_run
  PUBLIC :: temp_adjust_finalize

CONTAINS

  !> \section arg_table_temp_adjust_run  Argument Table
  !! \htmlinclude arg_table_temp_adjust_run.html
  !!
  subroutine temp_adjust_run(foo, timestep, temp_prev, temp_layer, qv, ps,    &
       errmsg, errflg, innie, outie, optsie)

    integer,                   intent(in)    :: foo
    real(kind_phys),           intent(in)    :: timestep
    real(kind_phys),           intent(inout),optional :: qv(:)
    real(kind_phys),           intent(inout) :: ps(:)
    REAL(kind_phys),           intent(in)    :: temp_prev(:)
    REAL(kind_phys),           intent(inout) :: temp_layer(foo)
    character(len=512),        intent(out)   :: errmsg
    integer,         optional, intent(out)   :: errflg
    real(kind_phys), optional, intent(in)    :: innie
    real(kind_phys), optional, intent(out)   :: outie
    real(kind_phys), optional, intent(inout) :: optsie
    !----------------------------------------------------------------

    integer :: col_index

    errmsg = ''
    if (present(errflg)) then
       errflg = 0
    end if

    do col_index = 1, foo
       temp_layer(col_index) = temp_layer(col_index) + temp_prev(col_index)
       if (present(qv)) qv(col_index) = qv(col_index) + 1.0_kind_phys
    end do
    if (present(innie) .and. present(outie) .and. present(optsie)) then
       outie = innie * optsie
       optsie = optsie + 1.0_kind_phys
    end if

  END SUBROUTINE temp_adjust_run

  !> \section arg_table_temp_adjust_init  Argument Table
  !! \htmlinclude arg_table_temp_adjust_init.html
  !!
  subroutine temp_adjust_init (errmsg, errflg)

    character(len=512),      intent(out)   :: errmsg
    integer,                 intent(out)   :: errflg

    ! This routine currently does nothing

    errmsg = ''
    errflg = 0

  end subroutine temp_adjust_init

  !> \section arg_table_temp_adjust_finalize  Argument Table
  !! \htmlinclude arg_table_temp_adjust_finalize.html
  !!
  subroutine temp_adjust_finalize (errmsg, errflg)

    character(len=512),      intent(out)   :: errmsg
    integer,                 intent(out)   :: errflg

    ! This routine currently does nothing

    errmsg = ''
    errflg = 0

  end subroutine temp_adjust_finalize

END MODULE temp_adjust
