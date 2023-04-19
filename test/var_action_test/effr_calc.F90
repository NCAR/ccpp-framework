!Test unit conversions for intent in, inout, out variables
!

module effr_calc

   use ccpp_kinds, only: kind_phys

   implicit none
   private

   public :: effr_calc_run

contains

   !> \section arg_table_effr_calc_run  Argument Table
   !! \htmlinclude arg_table_effr_calc_run.html
   !!
   subroutine effr_calc_run(ncol, nlev, effrr_in, effrl_inout, &
                            effri_out, effrs_inout, errmsg, errflg)

      integer,            intent(in)    :: ncol
      integer,            intent(in)    :: nlev
      real(kind_phys),    intent(in)    :: effrr_in(:,:)
      real(kind_phys),    intent(inout) :: effrl_inout(:,:)
      real(kind_phys),    intent(out)   :: effri_out(:,:)
      real(kind_phys),    intent(inout) :: effrs_inout(:,:)
      character(len=512), intent(out)   :: errmsg
      integer,            intent(out)   :: errflg
      !----------------------------------------------------------------

      real(kind_phys), parameter :: re_qc_min = 2.5E-6 ! 2.5 microns, in meters
      real(kind_phys), parameter :: re_qc_max = 5.0E-5 ! 50 microns, in meters
      real(kind_phys), parameter :: re_qi_avg = 7.5E-5 ! 75 microns, in meters
      real(kind_phys) :: effrr_local(ncol,nlev)

      errmsg = ''
      errflg = 0

      effrr_local = effrr_in
      effrl_inout = min(max(effrl_inout,re_qc_min),re_qc_max)
      effri_out   = re_qi_avg
      effrs_inout = effrs_inout + 1.0E-5 ! 10 microns in meters

   end subroutine effr_calc_run

end module effr_calc
