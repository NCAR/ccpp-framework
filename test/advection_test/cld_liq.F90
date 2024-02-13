! Test parameterization with advected species
!

MODULE cld_liq

   USE ccpp_kinds, ONLY: kind_phys

   IMPLICIT NONE
   PRIVATE

   PUBLIC :: cld_liq_init
   PUBLIC :: cld_liq_run
   PUBLIC :: cld_liq_dynamic_constituents

CONTAINS

   !> \section arg_table_cld_liq_run  Argument Table
   !! \htmlinclude arg_table_cld_liq_run.html
   !!
   subroutine cld_liq_run(ncol, timestep, tcld, temp, qv, ps, cld_liq_array,    &
        errmsg, errflg)

      integer,            intent(in)    :: ncol
      real(kind_phys),    intent(in)    :: timestep
      real(kind_phys),    intent(in)    :: tcld
      real(kind_phys),    intent(inout) :: temp(:,:)
      real(kind_phys),    intent(inout) :: qv(:,:)
      real(kind_phys),    intent(in)    :: ps(:)
      REAL(kind_phys),    intent(inout) :: cld_liq_array(:,:)
      character(len=512), intent(out)   :: errmsg
      integer,            intent(out)   :: errflg
      !----------------------------------------------------------------

      integer         :: icol
      integer         :: ilev
      real(kind_phys) :: cond

      errmsg = ''
      errflg = 0

      ! Apply state-of-the-art thermodynamics :)
      do icol = 1, ncol
         do ilev = 1, size(temp, 2)
            if ( (qv(icol, ilev) > 0.0_kind_phys) .and.                       &
                 (temp(icol, ilev) <= tcld)) then
               cond = MIN(qv(icol, ilev), 0.1_kind_phys)
               cld_liq_array(icol, ilev) = cld_liq_array(icol, ilev) + cond
               qv(icol, ilev) = qv(icol, ilev) - cond
               if (cond > 0.0_kind_phys) then
                  temp(icol, ilev) = temp(icol, ilev) + (cond * 5.0_kind_phys)
               end if
            end if
         end do
      end do

   END SUBROUTINE cld_liq_run

   !> \section arg_table_cld_liq_init  Argument Table
   !! \htmlinclude arg_table_cld_liq_init.html
   !!
   subroutine cld_liq_init(tfreeze, cld_liq_array, tcld, errmsg, errflg)

      real(kind_phys),    intent(in)  :: tfreeze
      real(kind_phys),    intent(out) :: cld_liq_array(:,:)
      real(kind_phys),    intent(out) :: tcld
      character(len=512), intent(out) :: errmsg
      integer,            intent(out) :: errflg

      ! This routine currently does nothing

      errmsg = ''
      errflg = 0
      cld_liq_array = 0.0_kind_phys
      tcld = tfreeze - 20.0_kind_phys

   end subroutine cld_liq_init

   subroutine cld_liq_dynamic_constituents(dyn_const, errcode, errmsg)
      use ccpp_constituent_prop_mod, only: ccpp_constituent_properties_t
      type(ccpp_constituent_properties_t), allocatable, intent(out) :: dyn_const(:)
      integer,                             intent(out) :: errcode
      character(len=512),                  intent(out) :: errmsg

      errmsg = ''
      errcode = 0
      allocate(dyn_const(1), stat=errcode)
      if (errcode /= 0) then
         errmsg = 'Error allocating dyn_const in cld_liq_dynamic_constituents'
      end if
      call dyn_const(1)%instantiate(std_name="dyn_const3", long_name='dyn const3', &
           units='kg kg-1', default_value=1._kind_phys,                            &
           vertical_dim='vertical_layer_dimension', advected=.true.,               &
           errcode=errcode, errmsg=errmsg)

   end subroutine cld_liq_dynamic_constituents


END MODULE cld_liq
