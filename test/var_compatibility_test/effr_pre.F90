!Test unit conversions for intent in, inout, out variables
!

module effr_pre

   use ccpp_kinds, only: kind_phys

   implicit none
   private

   public :: effr_pre_run

contains

   !> \section arg_table_effr_pre_run  Argument Table
   !! \htmlinclude arg_table_effr_pre_run.html
   !!
   subroutine effr_pre_run( effrr_inout,  errmsg, errflg)

      real(kind_phys),    intent(inout) :: effrr_inout(:,:)
      character(len=512), intent(out)   :: errmsg
      integer,            intent(out)   :: errflg
      !----------------------------------------------------------------
      real(kind_phys) :: effrr_min, effrr_max

      errmsg = ''
      errflg = 0

      ! Do some pre-processing on effrr...
      effrr_inout(:,:) = effrr_inout(:,:)*1._kind_phys

   end subroutine effr_pre_run
  
 end module effr_pre
