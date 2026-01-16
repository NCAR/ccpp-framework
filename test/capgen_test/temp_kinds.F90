!Define Fortran kinds for use specifically
!within PUMAS.  This allows PUMAS to control
!the precision in its internal routines without
!having to depend on a specific host model
!implemention.

module temp_kinds

   implicit none
   private

   integer, public, parameter :: temp_r8 = selected_real_kind(12) !8-byte real

end module temp_kinds
