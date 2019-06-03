module test_host_mod

   use ccpp_kinds,     only: kind_phys
   use test_host_data, only: physics_state

   implicit none
   public

   integer                      :: ntimes_loop
   !> \section arg_table_test_host_mod  Argument Table
   !! \htmlinclude arg_table_test_host_host.html
   !!
   integer,         parameter   :: ncols = 10
   integer,         parameter   :: pver = 5
   integer,         parameter   :: pverp = 6
   integer,         parameter   :: pcnst = 2
   real(kind_phys), allocatable :: temp_midpoints(:,:)
   real(kind_phys)              :: temp_interfaces(ncols, pverp)
   real(kind_phys)              :: dt
   real(kind_phys), parameter   :: temp_inc = 0.05_kind_phys
   type(physics_state)          :: phys_state
   integer                      :: num_model_times = -1
   integer,         allocatable :: model_times(:)

   integer,         parameter   :: num_time_steps = 2
   real(kind_phys), parameter   :: tolerance = 1.0e-13_kind_phys
   real(kind_phys)              :: tint_save(ncols, pverp)

   public :: init_temp
   public :: compare_temp
   public :: check_model_times

contains

   subroutine init_temp()

      integer :: col
      integer :: lev

      temp_midpoints = 0.0_kind_phys
      do lev = 1, pverp
         do col = 1, ncols
            temp_interfaces(col, lev) = real(((lev - 1) * ncols) + col, kind=kind_phys)
            tint_save(col, lev) = temp_interfaces(col, lev)
         end do
      end do

   end subroutine init_temp

   logical function check_model_times()

     check_model_times = (num_model_times > 0)
     if (check_model_times) then
        check_model_times = (size(model_times) == num_model_times)
        if (.not. check_model_times) then
           write(6, '(2(a,i0))') 'model_times size mismatch, ',               &
                size(model_times), ' should be ', num_model_times
        end if
     else
        write(6, '(a,i0,a)') 'num_model_times mismatch, ',num_model_times,    &
             ' should be greater than zero'
     end if

   end function check_model_times

   logical function compare_temp()

      integer         :: col
      integer         :: lev
      real(kind_phys) :: avg

      compare_temp = .true.

      do lev = 1, pver
         do col = 1, ncols
            avg = (tint_save(col,lev) + tint_save(col,lev+1))
            avg = 1.0_kind_phys + (avg / 2.0_kind_phys)
            avg = avg + (temp_inc * num_time_steps)
            if (abs((temp_midpoints(col, lev) - avg) / avg) > tolerance) then
               if (compare_temp) then
                  write(6, '("  COL  LEV      T MIDPOINTS        EXPECTED")')
               end if
               write(6, '(2i5,2(3x,es15.7))') col, lev,                       &
                    temp_midpoints(col, lev), avg
               compare_temp = .false.
            end if
         end do
      end do

   end function compare_temp

end module test_host_mod
