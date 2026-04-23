!>\file stub.F90
!! This file contains a stub CCPP scheme that does nothing
!! except requesting the minimum, mandatory variables.

module stub

   implicit none
   private
   public :: stub_init, stub_finalize

   contains

!! \section arg_table_stub_init Argument Table
!! \htmlinclude stub_init.html
!!
   subroutine stub_init(errmsg, errflg)
      character(len=*), intent(out) :: errmsg
      integer,          intent(out) :: errflg
      ! Initialize CCPP error handling variables
      errmsg = ''
      errflg = 0
   end subroutine stub_init

!! \section arg_table_stub_finalize Argument Table
!! \htmlinclude stub_finalize.html
!!
   subroutine stub_finalize(errmsg, errflg)
      character(len=*), intent(out) :: errmsg
      integer,          intent(out) :: errflg
      ! Initialize CCPP error handling variables
      errmsg = ''
      errflg = 0
   end subroutine stub_finalize

end module stub
