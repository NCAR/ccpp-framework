module host_ccpp_ddt

   implicit none
   private

   !> \section arg_table_ccpp_info_t  Argument Table
   !! \htmlinclude arg_table_ccpp_info_t.html
   !!
   type, public :: ccpp_info_t
      integer            :: col_start ! horizontal_loop_begin
      integer            :: col_end   ! horizontal_loop_end
      character(len=512) :: errmsg    ! ccpp_error_message
      integer            :: errflg    ! ccpp_error_code
   end type ccpp_info_t

end module host_ccpp_ddt
