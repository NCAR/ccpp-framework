! Test parameterization with no vertical level
!

MODULE temp_adjust

  USE ccpp_kinds, ONLY: kind_phys

  IMPLICIT NONE
  PRIVATE

  PUBLIC :: temp_adjust_register
  PUBLIC :: temp_adjust_init
  PUBLIC :: temp_adjust_run
  PUBLIC :: temp_adjust_finalize

  logical :: module_level_config = .false.

CONTAINS

  !> \section arg_table_temp_adjust_register Argument Table
  !! \htmlinclude arg_table_temp_adjust_register.hml
  !!
  subroutine temp_adjust_register(config_var, errmsg, errflg)
      logical, intent(in) :: config_var
      character(len=512),        intent(out)   :: errmsg
      integer,                   intent(out)   :: errflg

      module_level_config = config_var
      errflg = 0
      errmsg = ''

  end subroutine temp_adjust_register

  !> \section arg_table_temp_adjust_run  Argument Table
  !! \htmlinclude arg_table_temp_adjust_run.html
  !!
  subroutine temp_adjust_run(foo, timestep, interstitial_var, temp_prev, temp_layer, qv, ps,    &
       to_promote, promote_pcnst, errmsg, errflg, innie, outie, optsie)

    integer,                   intent(in)    :: foo
    real(kind_phys),           intent(in)    :: timestep
    real(kind_phys),           intent(inout),optional :: qv(:)
    real(kind_phys),           intent(inout) :: ps(:)
    REAL(kind_phys),           intent(in)    :: temp_prev(:)
    REAL(kind_phys),           intent(inout) :: temp_layer(foo)
    real(kind_phys),           intent(in)    :: to_promote(:)
    real(kind_phys),           intent(in)    :: promote_pcnst(:)
    integer,                   intent(out)   :: interstitial_var(:)
    character(len=512),        intent(out)   :: errmsg
    integer,                   intent(out)   :: errflg
    real(kind_phys), optional, intent(in)    :: innie
    real(kind_phys), optional, intent(out)   :: outie
    real(kind_phys), optional, intent(inout) :: optsie
    !----------------------------------------------------------------

    integer :: col_index

    errmsg = ''
    errflg = 0

    interstitial_var = 6
    if (size(interstitial_var) /= 3) then
       errflg = 1
       errmsg = 'interstitial variable not allocated properly!'
       return
    end if

    if (.not. module_level_config) then
       ! do nothing
       return
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
  subroutine temp_adjust_finalize (interstitial_var, errmsg, errflg)

    integer,                 intent(in)    :: interstitial_var(:)
    character(len=512),      intent(out)   :: errmsg
    integer,                 intent(out)   :: errflg

    ! This routine currently does nothing

    errmsg = ''
    errflg = 0
    if (size(interstitial_var) /= 3) then
       errflg = 1
       errmsg = 'interstitial variable not allocated properly!'
       return
    end if
    if (interstitial_var(1) /= 6) then
       errflg = 1
       errmsg = 'interstitial variable not set properly!'
    end if

  end subroutine temp_adjust_finalize

END MODULE temp_adjust
