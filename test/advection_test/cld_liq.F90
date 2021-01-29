! Test parameterization with advected species
!

MODULE cld_liq

   USE ccpp_kinds, ONLY: kind_phys

   IMPLICIT NONE
   PRIVATE

   PUBLIC :: cld_liq_init
   PUBLIC :: cld_liq_run
   PUBLIC :: cld_liq_finalize

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
      real(kind_phys) :: surplus

      errmsg = ''
      errflg = 0

      ! Apply state-of-the-art thermodynamics :)
      do icol = 1, ncol
         do ilev = 1, size(temp, 2)
            if (temp(icol, ilev) >= tcld) then
               surplus = MAX(qv(icol, ilev) - 0.9_kind_phys, 0.0_kind_phys)
               cld_liq(icol, ilev) = cld_liq(icol, ilev) + surplus
               qv(icol, ilev) = qv(icol, ilev) - surplus
               if (surplus > 0.0_kind_phys) then
                  temp(icol, ilev) = temp(icol, ilev) + 0.5_kind_phys
               end if
            end if
         end do
      end do

   END SUBROUTINE cld_liq_run

   !> \section arg_table_cld_liq_init  Argument Table
   !! \htmlinclude arg_table_cld_liq_init.html
   !!
   subroutine cld_liq_init(tfreeze, cld_liq, tcld, errmsg, errflg)

      real,               intent(in)  :: tfreeze
      real(kind_phys),    intent(out) :: cld_liq(:,:)
      real,               intent(out) :: tcld
      character(len=512), intent(out) :: errmsg
      integer,            intent(out) :: errflg

      ! This routine currently does nothing

      errmsg = ''
      errflg = 0
      cld_liq = 0.0_kind_phys
      tcld = tfreeze - 20.0_kind_phys

   end subroutine cld_liq_init

END MODULE cld_liq
