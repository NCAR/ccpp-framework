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
      real(8),intent(inout) :: effrs_inout(:,:)
      character(len=512), intent(out)   :: errmsg
      integer,            intent(out)   :: errflg
      !----------------------------------------------------------------

      real(kind_phys), parameter :: re_qc_min = 2.5 ! microns
      real(kind_phys), parameter :: re_qc_max = 50. ! microns
      real(kind_phys), parameter :: re_qi_avg = 75. ! microns
      real(kind_phys) :: effrr_local(ncol,nlev)

      errmsg = ''
      errflg = 0

      effrr_local = effrr_in
      effrl_inout = min(max(effrl_inout,re_qc_min),re_qc_max)
      effri_out   = re_qi_avg
      effrs_inout = effrs_inout + 10.0 ! in micrometer

   end subroutine effr_calc_run

end module effr_calc
