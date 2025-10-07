!Test unit conversions for intent in, inout, out variables
!

module effr_diag

   use ccpp_kinds, only: kind_phys

   implicit none
   private

   public :: effr_diag_run, effr_diag_init

contains

  !> \section arg_table_effr_diag_init Argument Table
  !! \htmlinclude arg_table_effr_diag_init.html
  !!
  subroutine effr_diag_init(scheme_order, errmsg, errflg)
    character(len=512), intent(out)   :: errmsg
    integer,            intent(out)   :: errflg
    integer,            intent(inout) :: scheme_order
 
    errmsg = ''
    errflg = 0

    if (scheme_order .ne. 4) then
        errflg = 1
        errmsg = 'ERROR: effr_diag_init() needs to be called fourth'
        return
     else
        scheme_order = scheme_order + 1
     endif

  end subroutine effr_diag_init

   !> \section arg_table_effr_diag_run  Argument Table
   !! \htmlinclude arg_table_effr_diag_run.html
   !!
   subroutine effr_diag_run( effrr_in,  scalar_var, errmsg, errflg)

      real(kind_phys),    intent(in)    :: effrr_in(:,:)
      integer,            intent(in)    :: scalar_var
      character(len=512), intent(out)   :: errmsg
      integer,            intent(out)   :: errflg
      !----------------------------------------------------------------
      real(kind_phys) :: effrr_min, effrr_max

      errmsg = ''
      errflg = 0

      call cmp_effr_diag(effrr_in, effrr_min, effrr_max)

      if (scalar_var .ne. 380) then
         errmsg	= 'ERROR: effr_diag_run():  scalar_var should be 380'
         errflg	= 1
      endif
   end subroutine effr_diag_run
   
   subroutine cmp_effr_diag(effr, effr_min, effr_max)
     real(kind_phys), intent(in)  :: effr(:,:)
     real(kind_phys), intent(out) :: effr_min, effr_max

     ! Do some diagnostic calcualtions...
     effr_min = minval(effr)
     effr_max = maxval(effr)
     
   end subroutine cmp_effr_diag
end module effr_diag
