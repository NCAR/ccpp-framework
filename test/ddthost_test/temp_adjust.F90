! Test parameterization with no vertical level
!

module temp_adjust

  use ccpp_kinds, only: kind_phys

  implicit none
  private

  public :: temp_adjust_init
  public :: temp_adjust_run
  public :: temp_adjust_finalize

contains

  !> \section arg_table_temp_adjust_run  Argument Table
  !! \htmlinclude arg_table_temp_adjust_run.html
  !!
  subroutine temp_adjust_run(foo, timestep, temp_prev, temp_layer, qv, ps,    &
       to_promote, promote_pcnst, errmsg, errflg, innie, outie, optsie)

    integer,                   intent(in)    :: foo
    real(kind_phys),           intent(in)    :: timestep
    real(kind_phys),           intent(inout),optional :: qv(:)
    real(kind_phys),           intent(inout) :: ps(:)
    real(kind_phys),           intent(in)    :: temp_prev(:)
    real(kind_phys),           intent(inout) :: temp_layer(foo)
    real(kind_phys),           intent(in)    :: to_promote(:)
    real(kind_phys),           intent(in)    :: promote_pcnst(:)
    character(len=512),        intent(out)   :: errmsg
    integer,                   intent(out)   :: errflg
    real(kind_phys), optional, intent(in)    :: innie
    real(kind_phys), optional, intent(out)   :: outie
    real(kind_phys), optional, intent(inout) :: optsie
    !----------------------------------------------------------------

    integer :: col_index

    errmsg = ''
    errflg = 0

    do col_index = 1, foo
       temp_layer(col_index) = temp_layer(col_index) + temp_prev(col_index)
       if (present(qv)) qv(col_index) = qv(col_index) + 1.0_kind_phys
    end do
    if (present(innie) .and. present(outie) .and. present(optsie)) then
       outie = innie * optsie
       optsie = optsie + 1.0_kind_phys
    end if

  end subroutine temp_adjust_run

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

end module temp_adjust
