! Define a new Fortran kind for use within
! various temp_* test files.

module temp_kinds

   implicit none
   private

   integer, public, parameter :: temp_r8 = selected_real_kind(12) !8-byte real
   integer, public, parameter :: temp_i8 = selected_int_kind (13) !8-byte integer

end module temp_kinds
