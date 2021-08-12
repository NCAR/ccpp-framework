! Test parameterization with advected species
!

MODULE cld_liq

   USE ccpp_kinds, ONLY: kind_phys

   IMPLICIT NONE
   PRIVATE

   PUBLIC :: cld_liq_init
   PUBLIC :: cld_liq_run

CONTAINS

   !> \section arg_table_cld_liq_run  Argument Table
   !! \htmlinclude arg_table_cld_liq_run.html
   !!
   subroutine cld_liq_run(ncol, timestep, tcld, temp, qv, ps, cld_liq,        &
        errmsg, errflg)

      integer,            intent(in)    :: ncol
      real(kind_phys),    intent(in)    :: timestep
      real(kind_phys),    intent(in)    :: tcld
      real(kind_phys),    intent(inout) :: temp(:,:)
      real(kind_phys),    intent(inout) :: qv(:,:)
      real(kind_phys),    intent(in)    :: ps(:)
      REAL(kind_phys),    intent(inout) :: cld_liq(:,:)
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
               cld_liq(icol, ilev) = cld_liq(icol, ilev) + cond
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
   subroutine cld_liq_init(tfreeze, cld_liq, tcld, errmsg, errflg)

      real(kind_phys),    intent(in)  :: tfreeze
      real(kind_phys),    intent(out) :: cld_liq(:,:)
      real(kind_phys),    intent(out) :: tcld
      character(len=512), intent(out) :: errmsg
      integer,            intent(out) :: errflg

      ! This routine currently does nothing

      errmsg = ''
      errflg = 0
      cld_liq = 0.0_kind_phys
      tcld = tfreeze - 20.0_kind_phys

   end subroutine cld_liq_init

END MODULE cld_liq
