module ddt1_plus

   use ccpp_kinds, only: kind_phys

   private
   implicit none

   !!
   type, public :: ddt1_t
      real, pointer :: undocumented_array(:) => NULL()
   contains
      procedure :: this_is_a_documented_object
   end type ddt1_t

   !! \section arg_table_ddt2_t
   !! \htmlinclude ddt2_t.html
   !!
   type, public :: ddt2_t
      integer,                 private :: num_vars = 0
      real(kind_phys), allocatable     :: vars(:,:,:)

   end type ddt2_t

CONTAINS

   logical function this_is_a_documented_object(this)
      class(ddt1_t) :: intent(in) :: this

      this_is_a_documented_object = .false.

   end function this_is_a_documented_object

end module ddt1_plus
