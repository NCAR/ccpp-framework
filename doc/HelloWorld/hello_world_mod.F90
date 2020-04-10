module hello_world_mod

   use ccpp_kinds, only: kind_phys

   implicit none
   public

   integer            :: ntimes_loop
   !> \section arg_table_hello_world_mod  Argument Table
   !! \htmlinclude arg_table_hello_world_host.html
   !!
   integer, parameter :: ncols = 10
   integer, parameter :: pver = 5
   integer, parameter :: pverp = 6
   real(kind_phys)    :: temp_midpoints(ncols, pver)
   real(kind_phys)    :: temp_interfaces(ncols, pverp)
   real(kind_phys)    :: dt

   public :: init_temp
   public :: compare_temp

contains

   subroutine init_temp()

      integer :: col
      integer :: lev

      temp_midpoints = 0.0_kind_phys
      do lev = 1, pverp
         do col = 1, ncols
            temp_interfaces(col, lev) = real(((lev - 1) * ncols) + col, kind=kind_phys)
         end do
      end do

   end subroutine init_temp

   logical function compare_temp()

      integer         :: col
      integer         :: lev
      real(kind_phys) :: avg

      compare_temp = .true.

      do lev = 1, pver
         do col = 1, ncols
            avg = (temp_interfaces(col,lev) + temp_interfaces(col,lev+1))
            avg = 1.0_kind_phys + (avg / 2.0_kind_phys)
            if (temp_midpoints(col, lev) /= avg) then
               write(6, *) col, lev, temp_midpoints(col, lev), avg
               compare_temp = .false.
            end if
         end do
      end do

   end function compare_temp

end module hello_world_mod
