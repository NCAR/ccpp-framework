! Test parameterization with advected species
!

MODULE cld_ice

   USE ccpp_kinds, ONLY: kind_phys

   IMPLICIT NONE
   PRIVATE

   PUBLIC :: cld_ice_init
   PUBLIC :: cld_ice_run
   PUBLIC :: cld_ice_final

   real(kind_phys), private :: tcld = HUGE(1.0_kind_phys)

CONTAINS

   !> \section arg_table_cld_ice_run  Argument Table
   !! \htmlinclude arg_table_cld_ice_run.html
   !!
   subroutine cld_ice_run(ncol, timestep, temp, qv, ps, cld_ice,              &
        errmsg, errflg)

      integer,            intent(in)    :: ncol
      real(kind_phys),    intent(in)    :: timestep
      real(kind_phys),    intent(inout) :: temp(:,:)
      real(kind_phys),    intent(inout) :: qv(:,:)
      real(kind_phys),    intent(in)    :: ps(:)
      REAL(kind_phys),    intent(inout) :: cld_ice(:,:)
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
               cld_ice(icol, ilev) = cld_ice(icol, ilev) + frz
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
   subroutine cld_ice_init(tfreeze, cld_ice, errmsg, errflg)

      real(kind_phys),    intent(in)    :: tfreeze
      real(kind_phys),    intent(inout) :: cld_ice(:,:)
      character(len=512), intent(out)   :: errmsg
      integer,            intent(out)   :: errflg

      errmsg = ''
      errflg = 0
      cld_ice = 0.0_kind_phys
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
