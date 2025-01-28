!Test unit conversions for intent in, inout, out variables
!

module effr_post

   use ccpp_kinds, only: kind_phys

   implicit none
   private

   public :: effr_post_run, effr_post_init

contains

   !> \section arg_table_effr_post_init Argument Table
   !! \htmlinclude arg_table_effr_post_init.html
   !!
   subroutine effr_post_init(errmsg, errflg)
     character(len=512), intent(out)   :: errmsg
     integer,            intent(out)   :: errflg

     errmsg = ''
     errflg = 0

   end subroutine effr_post_init

   !> \section arg_table_effr_post_run  Argument Table
   !! \htmlinclude arg_table_effr_post_run.html
   !!
   subroutine effr_post_run( effrr_inout,  errmsg, errflg)

      real(kind_phys),    intent(inout) :: effrr_inout(:,:)
      character(len=512), intent(out)   :: errmsg
      integer,            intent(out)   :: errflg
      !----------------------------------------------------------------
      real(kind_phys) :: effrr_min, effrr_max

      errmsg = ''
      errflg = 0

      ! Do some post-processing on effrr...
      effrr_inout(:,:) = effrr_inout(:,:)*1._kind_phys

   end subroutine effr_post_run
  
 end module effr_post
