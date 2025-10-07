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
   subroutine effr_post_init(scheme_order, errmsg, errflg)
     character(len=512), intent(out)   :: errmsg
     integer,            intent(out)   :: errflg
     integer,            intent(inout) :: scheme_order

     errmsg = ''
     errflg = 0

     if (scheme_order .ne. 3) then
        errflg = 1
        errmsg = 'ERROR: effr_post_init() needs to be called third'
        return
     else
        scheme_order = scheme_order + 1
     endif

   end subroutine effr_post_init

   !> \section arg_table_effr_post_run  Argument Table
   !! \htmlinclude arg_table_effr_post_run.html
   !!
   subroutine effr_post_run( effrr_inout,  scalar_var, errmsg, errflg)

      real(kind_phys),    intent(inout) :: effrr_inout(:,:)
      real(kind_phys),    intent(in)    :: scalar_var
      character(len=512), intent(out)   :: errmsg
      integer,            intent(out)   :: errflg
      !----------------------------------------------------------------
      real(kind_phys) :: effrr_min, effrr_max

      errmsg = ''
      errflg = 0

      ! Do some post-processing on effrr...
      effrr_inout(:,:) = effrr_inout(:,:)*1._kind_phys

      if (scalar_var .ne. 1013.0) then
         errmsg	= 'ERROR: effr_post_run():  scalar_var should be 1013.0'
         errflg	= 1
      endif

   end subroutine effr_post_run
  
 end module effr_post
