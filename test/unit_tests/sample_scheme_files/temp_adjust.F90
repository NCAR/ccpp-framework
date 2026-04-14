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

  !> \section arg_table_temp_adjust_register  Argument Table
  !! \htmlinclude arg_table_temp_adjust_register.html
  !!
  subroutine temp_adjust_register(config_var, dyn_const, errflg, errmsg)
    logical, intent(in) :: config_var
    type(ccpp_constituent_properties_t), allocatable, intent(out) :: dyn_const
    character(len=512), intent(out) :: errmsg
    integer, intent(out) :: errflg

    if ( .not. config_var) then
      return
    end if

    allocate(dyn_const(1))
    call dyn_const(1)%instantiate(std_name="dyn_const", long_name='dyn const', &
        diag_name='DYNCONST', units='kg kg-1', default_value=1._kind_phys, &
        vertical_dim='vertical_layer_dimension', advected=.true., &
        errcode=errflg, errmsg=errmsg)

  end subroutine temp_adjust_register

  !> \section arg_table_temp_adjust_run  Argument Table
  !! \htmlinclude arg_table_temp_adjust_run.html
  !!
  subroutine temp_adjust_run(foo, timestep, temp_prev, temp_layer, qv, ps, &
      errmsg, errflg)

    integer, intent(in) :: foo
    real(kind=kind_phys), intent(in) :: timestep
    real(kind=kind_phys), intent(inout) :: qv(:)
    real(kind=kind_phys), intent(inout) :: ps(:)
    real(kind=kind_phys), intent(in) :: temp_prev(:)
    real(kind=kind_phys), intent(inout) :: temp_layer(foo)
    character(len=512), intent(out) :: errmsg
    integer, intent(out) :: errflg
    !----------------------------------------------------------------

    integer :: col_index

    errmsg = ''
    errflg = 0

    do col_index = 1, foo
      temp_layer(col_index) = temp_layer(col_index) + temp_prev(col_index)
      qv(col_index) = qv(col_index) + 1.0_kind_phys
    end do

  end subroutine temp_adjust_run

  !> \section arg_table_temp_adjust_init  Argument Table
  !! \htmlinclude arg_table_temp_adjust_init.html
  !!
  subroutine temp_adjust_init(errmsg, errflg)

    character(len=512), intent(out) :: errmsg
    integer, intent(out) :: errflg

    ! This routine currently does nothing

    errmsg = ''
    errflg = 0

  end subroutine temp_adjust_init

  !> \section arg_table_temp_adjust_finalize  Argument Table
  !! \htmlinclude arg_table_temp_adjust_finalize.html
  !!
  subroutine temp_adjust_finalize(errmsg, errflg)

    character(len=512), intent(out) :: errmsg
    integer, intent(out) :: errflg

    ! This routine currently does nothing

    errmsg = ''
    errflg = 0

  end subroutine temp_adjust_finalize

end module temp_adjust
