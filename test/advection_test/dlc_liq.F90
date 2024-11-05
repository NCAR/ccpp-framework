! Test parameterization with a runtime constituents
!  properties object outside of the register phase

MODULE dlc_liq

   USE ccpp_kinds, ONLY: kind_phys
   use ccpp_constituent_prop_mod, only: ccpp_constituent_properties_t

   IMPLICIT NONE
   PRIVATE

   PUBLIC :: dlc_liq_init

CONTAINS

   !> \section arg_table_dlc_liq_init  Argument Table
   !! \htmlinclude arg_table_dlc_liq_init.html
   !!
   subroutine dlc_liq_init(dyn_const, errmsg, errflg)
      type(ccpp_constituent_properties_t), allocatable, intent(out) :: dyn_const(:)
      character(len=512), intent(out) :: errmsg
      integer,            intent(out) :: errflg

      character(len=256) :: stdname

      errmsg = ''
      errflg = 0
      allocate(dyn_const(1), stat=errflg)
      if (errflg /= 0) then
         errmsg = 'Error allocating dyn_const in dlc_liq_init'
         return
      end if
      call dyn_const(1)%instantiate(std_name="dyn_const3", long_name='dyn const3', &
           units='kg kg-1', default_value=1._kind_phys,                            &
           vertical_dim='vertical_layer_dimension', advected=.true.,               &
           errcode=errflg, errmsg=errmsg)
      call dyn_const(1)%standard_name(stdname, errcode=errflg, errmsg=errmsg)

   end subroutine dlc_liq_init

END MODULE dlc_liq
