! Test parameterization with a runtime constituents
!  properties object outside of the register phase

module dlc_liq

  use ccpp_kinds, only: kind_phys
  use ccpp_constituent_prop_mod, only: ccpp_constituent_properties_t

  implicit none
  private

  public :: dlc_liq_init

contains

  !> \section arg_table_dlc_liq_init  Argument Table
  !! \htmlinclude arg_table_dlc_liq_init.html
  !!
  subroutine dlc_liq_init(dyn_const, errmsg, errcode)
    type(ccpp_constituent_properties_t), allocatable, intent(out) :: dyn_const(:)
    character(len=512), intent(out) :: errmsg
    integer, intent(out) :: errcode

    character(len=256) :: stdname

    errmsg = ''
    errcode = 0
    allocate(dyn_const(1), stat=errcode)
    if (errcode /= 0) then
      errmsg = 'Error allocating dyn_const in dlc_liq_init'
      return
    end if
    call dyn_const(1)%instantiate(std_name="dyn_const3", long_name='dyn const3', &
        diag_name='DYNCONST3', units='kg kg-1', default_value=1._kind_phys, &
        vertical_dim='vertical_layer_dimension', advected=.true., &
        errcode=errcode, errmsg=errmsg)
    call dyn_const(1)%standard_name(stdname, errcode=errcode, errmsg=errmsg)

  end subroutine dlc_liq_init

end module dlc_liq
