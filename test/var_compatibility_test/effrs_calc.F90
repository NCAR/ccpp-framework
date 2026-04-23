!Test unit conversions for intent in, inout, out variables
!

module effrs_calc

   use ccpp_kinds, only: kind_phys

   implicit none
   private

   public :: effrs_calc_run

 contains
   !> \section arg_table_effrs_calc_run  Argument Table
   !! \htmlinclude arg_table_effrs_calc_run.html
   !!
   subroutine effrs_calc_run(effrs_inout, errmsg, errflg)

      real(kind_phys),    intent(inout) :: effrs_inout(:,:)
      character(len=512), intent(out)   :: errmsg
      integer,            intent(out)   :: errflg

      !----------------------------------------------------------------

      errmsg = ''
      errflg = 0

      effrs_inout = effrs_inout + (10.E-6_kind_phys / 3._kind_phys) ! in meters

   end subroutine effrs_calc_run

end module effrs_calc
