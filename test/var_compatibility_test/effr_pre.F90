!Test unit conversions for intent in, inout, out variables
!

module effr_pre

   use ccpp_kinds, only: kind_phys

   implicit none
   private

   public :: effr_pre_run, effr_pre_init

contains
   !> \section arg_table_effr_pre_init Argument Table
   !! \htmlinclude arg_table_effr_pre_init.html
   !!
   subroutine effr_pre_init(errmsg, errflg)
     character(len=512), intent(out)   :: errmsg
     integer,            intent(out)   :: errflg

     errmsg = ''
     errflg = 0

   end subroutine effr_pre_init

   !> \section arg_table_effr_pre_run  Argument Table
   !! \htmlinclude arg_table_effr_pre_run.html
   !!
   subroutine effr_pre_run( effrr_inout, scalar_var,  errmsg, errflg)

      real(kind_phys),    intent(inout) :: effrr_inout(:,:)
      real(kind_phys),    intent(in)    :: scalar_var
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
