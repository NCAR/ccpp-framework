!Test unit conversions for intent in, inout, out variables
!

module effr_diag

   use ccpp_kinds, only: kind_phys

   implicit none
   private

   public :: effr_diag_run

contains

   !> \section arg_table_effr_diag_run  Argument Table
   !! \htmlinclude arg_table_effr_diag_run.html
   !!
   subroutine effr_diag_run( effrr_in,  errmsg, errflg)

      real(kind_phys),    intent(in)    :: effrr_in(:,:)
      character(len=512), intent(out)   :: errmsg
      integer,            intent(out)   :: errflg
      !----------------------------------------------------------------
      real(kind_phys) :: effrr_min, effrr_max

      errmsg = ''
      errflg = 0

      call cmp_effr_diag(effrr_in, effrr_min, effrr_max)

   end subroutine effr_diag_run
   
   subroutine cmp_effr_diag(effr, effr_min, effr_max)
     real(kind_phys), intent(in)  :: effr(:,:)
     real(kind_phys), intent(out) :: effr_min, effr_max

     ! Do some diagnostic calcualtions...
     effr_min = minval(effr)
     effr_max = maxval(effr)
     
   end subroutine cmp_effr_diag
end module effr_diag
