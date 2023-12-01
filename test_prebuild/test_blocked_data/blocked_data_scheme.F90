!>\file blocked_data_scheme.F90
!! This file contains a blocked_data_scheme CCPP scheme that does nothing
!! except requesting the minimum, mandatory variables.

module blocked_data_scheme

   implicit none
   private
   public :: blocked_data_scheme_init, &
             blocked_data_scheme_timestep_init, &
             blocked_data_scheme_run, &
             blocked_data_scheme_timestep_finalize, &
             blocked_data_scheme_finalize

   ! This is for unit testing only
   integer, parameter, dimension(4) :: data_array_sizes = (/6,6,6,3/)

   contains

!! \section arg_table_blocked_data_scheme_init Argument Table
!! \htmlinclude blocked_data_scheme_init.html
!!
   subroutine blocked_data_scheme_init(data_array, errmsg, errflg)
      character(len=*), intent(out) :: errmsg
      integer,          intent(out) :: errflg
      integer,          intent(in)  :: data_array(:)
      ! Initialize CCPP error handling variables
      errmsg = ''
      errflg = 0
      ! Check size of data array
      write(0,'(a,i3)') 'In blocked_data_scheme_init: checking size of data array to be', sum(data_array_sizes)
      if (size(data_array)/=sum(data_array_sizes)) then
         write(errmsg,'(2(a,i3))') "Error, expected size(data_array)==", sum(data_array_sizes), "but got ", size(data_array)
         errflg = 1
         return
      end if
   end subroutine blocked_data_scheme_init

!! \section arg_table_blocked_data_scheme_timestep_init Argument Table
!! \htmlinclude blocked_data_scheme_timestep_init.html
!!
   subroutine blocked_data_scheme_timestep_init(data_array, errmsg, errflg)
      character(len=*), intent(out) :: errmsg
      integer,          intent(out) :: errflg
      integer,          intent(in)  :: data_array(:)
      ! Initialize CCPP error handling variables
      errmsg = ''
      errflg = 0
      ! Check size of data array
      write(0,'(a,i3)') 'In blocked_data_scheme_timestep_init: checking size of data array to be', sum(data_array_sizes)
      if (size(data_array)/=sum(data_array_sizes)) then
         write(errmsg,'(2(a,i3))') "Error, expected size(data_array)==", sum(data_array_sizes), " but got ", size(data_array)
         errflg = 1
         return
      end if
   end subroutine blocked_data_scheme_timestep_init

!! \section arg_table_blocked_data_scheme_run Argument Table
!! \htmlinclude blocked_data_scheme_run.html
!!
   subroutine blocked_data_scheme_run(nb, data_array, errmsg, errflg)
      character(len=*), intent(out) :: errmsg
      integer,          intent(out) :: errflg
      integer,          intent(in)  :: nb
      integer,          intent(in)  :: data_array(:)
      ! Initialize CCPP error handling variables
      errmsg = ''
      errflg = 0
      ! Check size of data array
      write(0,'(2(a,i3))') 'In blocked_data_scheme_run: checking size of data array for block', nb, ' to be', data_array_sizes(nb)
      if (size(data_array)/=data_array_sizes(nb)) then
         write(errmsg,'(a,i4)') "Error in blocked_data_scheme_run, expected size(data_array)==6, got ", size(data_array)
         errflg = 1
         return
      end if
   end subroutine blocked_data_scheme_run

   !! \section arg_table_blocked_data_scheme_timestep_finalize Argument Table
   !! \htmlinclude blocked_data_scheme_timestep_finalize.html
   !!
      subroutine blocked_data_scheme_timestep_finalize(data_array, errmsg, errflg)
         character(len=*), intent(out) :: errmsg
         integer,          intent(out) :: errflg
         integer,          intent(in)  :: data_array(:)
         ! Initialize CCPP error handling variables
         errmsg = ''
         errflg = 0
         ! Check size of data array
         write(0,'(a,i3)') 'In blocked_data_scheme_timestep_finalize: checking size of data array to be', sum(data_array_sizes)
         if (size(data_array)/=sum(data_array_sizes)) then
            write(errmsg,'(2(a,i3))') "Error, expected size(data_array)==", sum(data_array_sizes), "but got ", size(data_array)
            errflg = 1
            return
         end if
      end subroutine blocked_data_scheme_timestep_finalize

!! \section arg_table_blocked_data_scheme_finalize Argument Table
!! \htmlinclude blocked_data_scheme_finalize.html
!!
   subroutine blocked_data_scheme_finalize(data_array, errmsg, errflg)
      character(len=*), intent(out) :: errmsg
      integer,          intent(out) :: errflg
      integer,          intent(in)  :: data_array(:)
      ! Initialize CCPP error handling variables
      errmsg = ''
      errflg = 0
      ! Check size of data array
      write(0,'(a,i3)') 'In blocked_data_scheme_finalize: checking size of data array to be', sum(data_array_sizes)
      if (size(data_array)/=sum(data_array_sizes)) then
         write(errmsg,'(2(a,i3))') "Error, expected size(data_array)==", sum(data_array_sizes), "but got ", size(data_array)
         errflg = 1
         return
      end if
   end subroutine blocked_data_scheme_finalize

end module blocked_data_scheme
