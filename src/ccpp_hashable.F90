module ccpp_hashable

   implicit none
   private

   ! Public interfaces
   public :: new_hashable_char
   public :: new_hashable_int

   type, abstract, public :: ccpp_hashable_t
      ! The hashable type is a base type that contains a hash key.
   contains
      procedure(ccpp_hashable_get_key), deferred :: key
   end type ccpp_hashable_t

   type, public, extends(ccpp_hashable_t) :: ccpp_hashable_char_t
      character(len=:), private, allocatable :: name
   contains
      procedure :: key => ccpp_hashable_char_get_key
   end type ccpp_hashable_char_t

   type, public, extends(ccpp_hashable_t) :: ccpp_hashable_int_t
      integer, private :: value
   contains
      procedure :: key => ccpp_hashable_int_get_key
      procedure :: val => ccpp_hashable_int_get_val
   end type ccpp_hashable_int_t

   ! Abstract interface for key procedure of ccpp_hashable_t class
   abstract interface
      function ccpp_hashable_get_key(hashable)
         import :: ccpp_hashable_t
         class(ccpp_hashable_t), intent(in) :: hashable
         character(len=:), allocatable      :: ccpp_hashable_get_key
      end function ccpp_hashable_get_key
   end interface

CONTAINS

   !#######################################################################

   subroutine new_hashable_char(name_in, new_obj)
      character(len=*), intent(in)        :: name_in
      type(ccpp_hashable_char_t), pointer :: new_obj

      if (associated(new_obj)) then
         deallocate(new_obj)
      end if
      allocate(new_obj)
      new_obj%name = name_in
   end subroutine new_hashable_char

   !#######################################################################

   function ccpp_hashable_char_get_key(hashable)
      ! Return the hashable char class key (name)
      class(ccpp_hashable_char_t), intent(in) :: hashable
      character(len=:), allocatable           :: ccpp_hashable_char_get_key

      ccpp_hashable_char_get_key = hashable%name
   end function ccpp_hashable_char_get_key

   !#######################################################################

   subroutine new_hashable_int(val_in, new_obj)
      integer, intent(in)                :: val_in
      type(ccpp_hashable_int_t), pointer :: new_obj

      if (associated(new_obj)) then
         deallocate(new_obj)
      end if
      allocate(new_obj)
      new_obj%value = val_in
   end subroutine new_hashable_int

   !#######################################################################

   function ccpp_hashable_int_get_key(hashable)
      ! Return the hashable int class key (value ==> string)
      class(ccpp_hashable_int_t), intent(in) :: hashable
      character(len=:), allocatable          :: ccpp_hashable_int_get_key

      character(len=32) :: key_str

      write(key_str, '(i0)') hashable%val()
      ccpp_hashable_int_get_key = trim(key_str)
   end function ccpp_hashable_int_get_key

   !#######################################################################

   integer function ccpp_hashable_int_get_val(hashable)
      ! Return the hashable int class value
      class(ccpp_hashable_int_t), intent(in) :: hashable

      ccpp_hashable_int_get_val = hashable%value
   end function ccpp_hashable_int_get_val

end module ccpp_hashable
