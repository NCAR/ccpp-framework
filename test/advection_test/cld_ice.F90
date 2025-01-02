! Test parameterization with advected species
!

MODULE cld_ice

   USE ccpp_kinds, ONLY: kind_phys

   IMPLICIT NONE
   PRIVATE

   PUBLIC :: cld_ice_register
   PUBLIC :: cld_ice_init
   PUBLIC :: cld_ice_run
   PUBLIC :: cld_ice_final

   real(kind_phys), private :: tcld = HUGE(1.0_kind_phys)

CONTAINS

   !> \section arg_table_cld_ice_register  Argument Table
   !! \htmlinclude arg_table_cld_ice_register.html
   !!
   subroutine cld_ice_register(dyn_const_ice, errmsg, errcode)
      use ccpp_constituent_prop_mod, only: ccpp_constituent_properties_t
      type(ccpp_constituent_properties_t), allocatable, intent(out) :: dyn_const_ice(:)
      integer,                             intent(out) :: errcode
      character(len=512),                  intent(out) :: errmsg

      errmsg = ''
      errcode = 0
      allocate(dyn_const_ice(2), stat=errcode)
      if (errcode /= 0) then
         errmsg = 'Error allocating dyn_const in cld_ice_dynamic_constituents'
         return
      end if
      call dyn_const_ice(1)%instantiate(std_name='dyn_const1', long_name='dyn const1', &
           units='kg kg-1', default_value=0._kind_phys,                            &
           vertical_dim='vertical_layer_dimension', advected=.true.,               &
           min_value=1000._kind_phys, water_species=.true., mixing_ratio_type='wet', &
           errcode=errcode, errmsg=errmsg)
      call dyn_const_ice(2)%instantiate(std_name='dyn_const2_wrt_moist_air', long_name='dyn const2', &
           units='kg kg-1', default_value=0._kind_phys,                            &
           vertical_dim='vertical_layer_dimension', advected=.true.,               &
           water_species=.false., errcode=errcode, errmsg=errmsg)

   end subroutine cld_ice_register

   !> \section arg_table_cld_ice_run  Argument Table
   !! \htmlinclude arg_table_cld_ice_run.html
   !!
   subroutine cld_ice_run(ncol, timestep, temp, qv, ps, cld_ice_array,              &
        errmsg, errflg)

      integer,            intent(in)    :: ncol
      real(kind_phys),    intent(in)    :: timestep
      real(kind_phys),    intent(inout) :: temp(:,:)
      real(kind_phys),    intent(inout) :: qv(:,:)
      real(kind_phys),    intent(in)    :: ps(:)
      REAL(kind_phys),    intent(inout) :: cld_ice_array(:,:)
      character(len=512), intent(out)   :: errmsg
      integer,            intent(out)   :: errflg
      !----------------------------------------------------------------

      integer         :: icol
      integer         :: ilev
      real(kind_phys) :: frz

      errmsg = ''
      errflg = 0

      ! Apply state-of-the-art thermodynamics :)
      do icol = 1, ncol
         do ilev = 1, size(temp, 2)
            if (temp(icol, ilev) < tcld) then
               frz = MAX(qv(icol, ilev) - 0.5_kind_phys, 0.0_kind_phys)
               cld_ice_array(icol, ilev) = cld_ice_array(icol, ilev) + frz
               qv(icol, ilev) = qv(icol, ilev) - frz
               if (frz > 0.0_kind_phys) then
                  temp(icol, ilev) = temp(icol, ilev) + 1.0_kind_phys
               end if
            end if
         end do
      end do

   END SUBROUTINE cld_ice_run

   !> \section arg_table_cld_ice_init  Argument Table
   !! \htmlinclude arg_table_cld_ice_init.html
   !!
   subroutine cld_ice_init(tfreeze, cld_ice_array, errmsg, errflg)

      real(kind_phys),    intent(in)    :: tfreeze
      real(kind_phys),    intent(inout) :: cld_ice_array(:,:)
      character(len=512), intent(out)   :: errmsg
      integer,            intent(out)   :: errflg

      errmsg = ''
      errflg = 0
      cld_ice_array = 0.0_kind_phys
      tcld = tfreeze - 20.0_kind_phys

   end subroutine cld_ice_init

   !> \section arg_table_cld_ice_final  Argument Table
   !! \htmlinclude arg_table_cld_ice_final.html
   !!

   !> @{
   !! This routine does nothing, but it tests if blank
   !! lines and doxygen comments between metadata hooks
   !! and the subroutine are parsed correctly.
   !! @{

   subroutine cld_ice_final(errmsg, errflg)

      character(len=512), intent(out)   :: errmsg
      integer,            intent(out)   :: errflg

      errmsg = ''
      errflg = 0

   end subroutine cld_ice_final

   !! @}
   !! @}

END MODULE cld_ice
