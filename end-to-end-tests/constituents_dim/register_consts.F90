!>\file register_consts.F90
!! Register-phase scheme that registers three dynamic constituents. Declaring a
!! ccpp_constituent_properties_t(:) argument is what activates capgen's
!! constituent machinery, giving the suite a meaningful
!! number_of_ccpp_constituents (= 3 here) for the rest of this test.

module register_consts

  use ccpp_kinds, only: kind_phys
  use ccpp_constituent_prop_mod, only: ccpp_constituent_properties_t

  implicit none

  private
  public :: register_consts_register

contains

  !! \section arg_table_register_consts_register Argument Table
  !! \htmlinclude register_consts_register.html
  !!
  subroutine register_consts_register(dyn_const, errmsg, errcode)
    type(ccpp_constituent_properties_t), allocatable, intent(out) :: dyn_const(:)
    character(len=512), intent(out) :: errmsg
    integer,            intent(out) :: errcode

    errmsg = ''
    errcode = 0

    allocate(dyn_const(3), stat=errcode)
    if (errcode /= 0) then
      errmsg = 'Error allocating dyn_const in register_consts_register'
      return
    end if

    call dyn_const(1)%instantiate( &
        std_name='test_constituent_one', long_name='test constituent one', &
        diag_name='TEST_CONST_1', units='kg kg-1', default_value=0._kind_phys, &
        vertical_dim='vertical_layer_dimension', advected=.true., &
        errcode=errcode, errmsg=errmsg)
    if (errcode /= 0) return
    call dyn_const(2)%instantiate( &
        std_name='test_constituent_two', long_name='test constituent two', &
        diag_name='TEST_CONST_2', units='kg kg-1', default_value=0._kind_phys, &
        vertical_dim='vertical_layer_dimension', advected=.true., &
        errcode=errcode, errmsg=errmsg)
    if (errcode /= 0) return
    call dyn_const(3)%instantiate( &
        std_name='test_constituent_three', long_name='test constituent three', &
        diag_name='TEST_CONST_3', units='kg kg-1', default_value=0._kind_phys, &
        vertical_dim='vertical_layer_dimension', advected=.true., &
        errcode=errcode, errmsg=errmsg)
  end subroutine register_consts_register

end module register_consts
