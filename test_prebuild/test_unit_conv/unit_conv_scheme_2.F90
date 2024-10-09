!>\file unit_conv_scheme_2.F90
!! This file contains a unit_conv_scheme_2 CCPP scheme that does nothing
!! except requesting the minimum, mandatory variables.

module unit_conv_scheme_2

   use, intrinsic :: iso_fortran_env, only: error_unit
   use ccpp_kinds, only : kind_phys
   implicit none

   private
   public :: unit_conv_scheme_2_run

   !! This is for unit testing only
   real(kind_phys), parameter :: target_value = 1.0E-3_kind_phys

   contains

!! \section arg_table_unit_conv_scheme_2_run Argument Table
!! \htmlinclude unit_conv_scheme_2_run.html
!!
   subroutine unit_conv_scheme_2_run(data_array, data_array_opt, errmsg, errflg)
      character(len=*), intent(out)   :: errmsg
      integer,          intent(out)   :: errflg
      real(kind_phys),  intent(inout) :: data_array(:)
      real(kind_phys),  intent(inout), optional :: data_array_opt(:)

      ! Initialize CCPP error handling variables
      errmsg = ''
      errflg = 0
      ! Check values in data array
      write(error_unit,'(a,e12.4))') 'In unit_conv_scheme_2_run: checking min/max values of data array to be approximately ', target_value
      if (minval(data_array)<0.99*target_value .or. maxval(data_array)>1.01*target_value) then
         write(errmsg,'(3(a,e12.4),a)') 'Error in unit_conv_scheme_2_run, expected values of approximately ', &
                                        target_value, '  but got [ ', minval(data_array), ' : ', maxval(data_array), ' ]'
         errflg = 1
         return
      end if
      ! Check for presence of optional data array, then check its values
      write(error_unit,'(a))') 'In unit_conv_scheme_2_run: checking for presence of optional data array'
      if (.not. present(data_array_opt)) then
         write(error_unit,'(a))') 'Error in unit_conv_scheme_2_run, optional data array expected but not present'
         errflg = 1
         return
      endif
      write(error_unit,'(a,e12.4))') 'In unit_conv_scheme_2_run: checking min/max values of optional data array to be approximately ', target_value
      if (minval(data_array_opt)<0.99*target_value .or. maxval(data_array_opt)>1.01*target_value) then
         write(errmsg,'(3(a,e12.4),a)') 'Error in unit_conv_scheme_2_run, expected values of approximately ', &
                                        target_value, '  but got [ ', minval(data_array_opt), ' : ', maxval(data_array_opt), ' ]'
         errflg = 1
         return
      end if
   end subroutine unit_conv_scheme_2_run

end module unit_conv_scheme_2
