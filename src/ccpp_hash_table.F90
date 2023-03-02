!!XXgoldyXX: To do, statistics output
module ccpp_hash_table

   use ccpp_hashable, only: ccpp_hashable_t

   implicit none
   private

   !
   !  Constants used in hashing function gen_hash_key.
   !

   integer, parameter :: gen_hash_key_offset = 21467 ! z'000053db'

   integer, parameter :: tbl_max_idx = 15
   integer, parameter, dimension(0:tbl_max_idx) :: tbl_gen_hash_key =         &
        (/ 61, 59, 53, 47, 43, 41, 37, 31, 29, 23, 17, 13, 11, 7, 3, 1 /)

   integer, parameter :: table_factor_size = 8     ! Table size / # entries
   integer, parameter :: table_overflow_factor = 4 ! # entries / Overflow size

   type :: table_entry_t
      ! Any table entry contains a key and a value
      class(ccpp_hashable_t), pointer             :: entry_value => NULL()
      type(table_entry_t),    pointer             :: next => NULL()
   contains
      final :: finalize_table_entry
   end type table_entry_t

   type, public :: ccpp_hash_table_t
      ! ccpp_hash_table_t contains all information to build and use a hash table
      ! It also keeps track of statistics such as collision frequency and size
      integer, private                          :: table_size = -1
      integer, private                          :: key_offset = gen_hash_key_offset
      type(table_entry_t), private, allocatable :: table(:)
 ! Statistics
      integer, private                          :: num_keys = 0
      integer, private                          :: num_key_collisions = 0
      integer, private                          :: max_collision = 0
   contains
      procedure :: is_initialized => hash_table_is_initialized
      procedure :: initialize     => hash_table_initialize_table
      procedure :: key_hash       => hash_table_key_hash
      procedure :: add_hash_key   => hash_table_add_hash_key
      procedure :: table_value    => hash_table_table_value
      procedure :: num_values     => hash_table_num_values
      procedure :: clear          => hash_table_clear_table
   end type ccpp_hash_table_t

   type, public :: ccpp_hash_iterator_t
      ! ccpp_hash_iterator contains information allowing iteration through all
      !   entries in a hash table
      integer,                 private          :: index = 0
      type(table_entry_t),     private, pointer :: table_entry => NULL()
      type(ccpp_hash_table_t), private, pointer :: hash_table => NULL()
   contains
      procedure :: initialize => hash_iterator_initialize
      procedure :: key        => hash_iterator_key
      procedure :: next       => hash_iterator_next_entry
      procedure :: valid      => hash_iterator_is_valid
      procedure :: value      => hash_iterator_value
      procedure :: finalize   => hash_iterator_finalize
   end type ccpp_hash_iterator_t

   !! Private interfaces
   private :: have_error      ! Has a called routine detected an error?
   private :: clear_optstring ! Clear a string, if present

CONTAINS

   !#######################################################################
   !
   ! Hash table methods
   !
   !#######################################################################

   logical function have_error(errmsg)
      ! Return .true. iff <errmsg> is present and contains text

      ! Dummy argument
      character(len=*), optional, intent(in) :: errmsg

      have_error = present(errmsg)
      if (have_error) then
         have_error = len_trim(errmsg) > 0
      end if
   end function have_error

   !#######################################################################

   subroutine clear_optstring(str)
      ! clear <str> if it is present

      ! Dummy argument
      character(len=*), optional, intent(inout) :: str

      if (present(str)) then
         str = ''
      end if
   end subroutine clear_optstring

   !#######################################################################

   elemental subroutine finalize_table_entry(te)

      ! Dummy argument
      type(table_entry_t), intent(inout) :: te
      ! Local variable
      type(table_entry_t), pointer :: temp

      if (associated(te%entry_value)) then
         nullify(te%entry_value) ! We may not own the memory
         temp => te%next
         nullify(te%next)
         if (associated(temp)) then
            deallocate(temp)
            nullify(temp)
         end if
      end if

   end subroutine finalize_table_entry

   !#######################################################################

   logical function hash_table_is_initialized(this)
      ! Return .true. iff <this> is an initialized hash table

      ! Dummy argument
      class(ccpp_hash_table_t)      :: this

      hash_table_is_initialized = allocated(this%table)

   end function hash_table_is_initialized

   !#######################################################################

   subroutine hash_table_initialize_table(this, tbl_size, key_off)
      ! Initialize this table.

      ! Dummy arguments
      class(ccpp_hash_table_t)      :: this
      integer,           intent(in) :: tbl_size   ! new table size
      integer, optional, intent(in) :: key_off    ! key offset

      ! Clear this table so it can be initialized
      if (allocated(this%table)) then
         deallocate(this%table)
      end if
      this%num_keys = 0
      this%num_key_collisions = 0
      this%max_collision = 0
      ! Avoid too-large tables
      this%table_size = ishft(1, MIN(tbl_size, bit_size(1) - 2))
      allocate(this%table(this%table_size))
      if (present(key_off)) then
         this%key_offset = key_off
      end if
   end subroutine hash_table_initialize_table

   !#######################################################################

   integer function hash_table_key_hash(this, string, errmsg) result(hash_key)
      !
      !-----------------------------------------------------------------------
      !
      ! Purpose: Generate a hash key on the interval [0 .. tbl_hash_pri_sz-1]
      !          given a character string.
      !
      ! Algorithm is a variant of perl's internal hashing function.
      !
      !-----------------------------------------------------------------------
      !
      !
      !  Dummy Arguments:
      !
      class(ccpp_hash_table_t)                :: this
      character(len=*),           intent(in)  :: string
      character(len=*), optional, intent(out) :: errmsg
      character(len=*), parameter             :: subname = 'HASH_TABLE_KEY_HASH'
      !
      !  Local.
      !
      integer :: hash
      integer :: index
      integer :: ind_fact
      integer :: hash_fact

      hash = this%key_offset
      ind_fact = 0
      do index = 1, len_trim(string)
         ind_fact = ind_fact + 1
         if (ind_fact > tbl_max_idx) then
            ind_fact = 1
         end if
         hash_fact = tbl_gen_hash_key(ind_fact)
         hash = ieor(hash, (ichar(string(index:index)) * hash_fact))
      end do

      hash_key = iand(hash, this%table_size - 1) + 1
      if ((hash_key < 1) .or. (hash_key > this%table_size)) then
         if (present(errmsg)) then
            write(errmsg, '(2a,2(i0,a))') subname, ' ERROR: Key Hash, ',      &
                 hash_key, ' out of bounds, [1, ', this%table_size, ']'
         else
            write(6, '(2a,2(i0,a))') subname, ' ERROR: Key Hash, ',           &
                 hash_key, ' out of bounds, [1, ', this%table_size, ']'
            STOP 1
         end if
      end if

   end function hash_table_key_hash

   !#######################################################################

   function hash_table_table_value(this, key, errmsg) result(tbl_val)
      !
      !-----------------------------------------------------------------------
      !
      ! Purpose: Return the the key value of <key>
      !
      !          If the object is not found, return NULL
      !
      !-----------------------------------------------------------------------
      !
      !  Dummy Arguments:
      !
      class(ccpp_hash_table_t)                      :: this
      character(len=*),                 intent(in)  :: key
      character(len=*),       optional, intent(out) :: errmsg
      class(ccpp_hashable_t), pointer               :: tbl_val
      !
      !  Local.
      !
      integer                        :: hash_key
      type(table_entry_t), pointer   :: next_ptr
      character(len=*),    parameter :: subname = 'HASH_TABLE_TABLE_INDEX'

      call clear_optstring(errmsg)
      nullify(tbl_val)
      hash_key = this%key_hash(key, errmsg=errmsg)
      if (have_error(errmsg)) then
         errmsg = trim(errmsg)//', called from '//subname
      else if (associated(this%table(hash_key)%entry_value)) then
         if (this%table(hash_key)%entry_value%key() == trim(key)) then
            tbl_val => this%table(hash_key)%entry_value
         else
            next_ptr => this%table(hash_key)%next
            do
               if (associated(next_ptr)) then
                  if (associated(next_ptr%entry_value)) then
                     if (next_ptr%entry_value%key() == trim(key)) then
                        tbl_val => next_ptr%entry_value
                        exit
                     end if
                  end if
                  next_ptr => next_ptr%next
               else
                  exit
               end if
            end do
         end if
      end if

      if ((.not. associated(tbl_val)) .and. present(errmsg)) then
         if (.not. have_error(errmsg)) then ! Still need to test for empty
            write(errmsg, *) subname, ": No entry for '", trim(key), "'"
         end if
      end if

   end function hash_table_table_value

   !#######################################################################

   subroutine hash_table_add_hash_key(this, newval, errmsg)
      !
      !-----------------------------------------------------------------------
      !
      ! Purpose: Add <newval> to this hash table using its key
      !          Its key must not be an empty string
      !          It is an error to try to add a key more than once
      !
      !
      !-----------------------------------------------------------------------

      !  Dummy arguments:
      class(ccpp_hash_table_t)                      :: this
      class(ccpp_hashable_t), target                :: newval
      character(len=*),       optional, intent(out) :: errmsg
      ! Local variables
      integer                          :: hash_ind
      integer                          :: ovflw_len
      character(len=:),    allocatable :: newkey
      type(table_entry_t), pointer     :: next_ptr
      type(table_entry_t), pointer     :: new_entry
      character(len=*),    parameter   :: subname = 'HASH_TABLE_ADD_HASH_KEY'

      call clear_optstring(errmsg)
      nullify(new_entry)
      newkey = newval%key()
      hash_ind = this%key_hash(newkey, errmsg=errmsg)
      ! Check for this entry
      if (have_error(errmsg)) then
         errmsg = trim(errmsg)//', called from '//subname
      else if (associated(this%table_value(newkey))) then
         if (present(errmsg)) then
            write(errmsg, *) subname, " ERROR: key, '", newkey,               &
                 "' already in table"
         end if
      else
         if (associated(this%table(hash_ind)%entry_value)) then
            ! We have a collision, make a new entry
            allocate(new_entry)
            new_entry%entry_value => newval
            ! Now, find a spot
            if (associated(this%table(hash_ind)%next)) then
               ovflw_len = 1
               next_ptr => this%table(hash_ind)%next
               do
                  if (associated(next_ptr%next)) then
                     ovflw_len = ovflw_len + 1
                     next_ptr => next_ptr%next
                  else
                     exit
                  end if
               end do
               ovflw_len = ovflw_len + 1
               next_ptr%next => new_entry
            else
               this%num_key_collisions = this%num_key_collisions + 1
               this%table(hash_ind)%next => new_entry
               ovflw_len = 1
            end if
            nullify(new_entry)
            this%max_collision = MAX(this%max_collision, ovflw_len)
         else
            this%table(hash_ind)%entry_value => newval
         end if
         this%num_keys = this%num_keys + 1
      end if

   end subroutine hash_table_add_hash_key

   !#######################################################################

   integer function hash_table_num_values(this) result(numval)
      !
      !-----------------------------------------------------------------------
      !
      ! Purpose: Return the number of populated table values
      !
      !-----------------------------------------------------------------------

      !  Dummy argument:
      class(ccpp_hash_table_t)                      :: this

      numval = this%num_keys

   end function hash_table_num_values

   !#######################################################################

   subroutine hash_table_clear_table(this)
      !
      !-----------------------------------------------------------------------
      !
      ! Purpose: Deallocate the hash table and all of its entries
      !
      !-----------------------------------------------------------------------

      !  Dummy argument:
      class(ccpp_hash_table_t)     :: this

      ! Clear all the table entries
      if (this%is_initialized()) then
         if (allocated(this%table)) then
            ! This should deallocate the entire chain of entries
            deallocate(this%table)
         end if
      end if
      this%table_size = -1
      this%num_keys = 0
      this%num_key_collisions = 0
      this%max_collision = 0

   end subroutine hash_table_clear_table

   !#######################################################################
   !
   ! Hash iterator methods
   !
   !#######################################################################

   subroutine hash_iterator_initialize(this, hash_table)
      ! Initialize a hash_table iterator to the first value in the hash table
      ! Note that the table_entry pointer is only used for the "next" field
      !    in the hash table (entry itself is not a pointer).

      ! Dummy arguments
      class(ccpp_hash_iterator_t)      :: this
      class(ccpp_hash_table_t), target :: hash_table

      class(ccpp_hash_table_t), target, allocatable, save :: temp_hash_table

      temp_hash_table = hash_table

      this%hash_table => temp_hash_table
      this%index = 0
      nullify(this%table_entry)
      do
         this%index = this%index + 1
         if (associated(hash_table%table(this%index)%entry_value)) then
            exit
         else if (this%index > hash_table%table_size) then
            this%index = 0
         end if
      end do
   end subroutine hash_iterator_initialize

   !#######################################################################

   function hash_iterator_key(this) result(key)
      ! Return the key for this hash iterator entry

      ! Dummy arguments
      class(ccpp_hash_iterator_t)   :: this
      character(len=:), allocatable :: key

      if (this%valid()) then
         if (associated(this%table_entry)) then
            key = this%table_entry%entry_value%key()
         else
            key = this%hash_table%table(this%index)%entry_value%key()
         end if
      else
         key = ''
      end if

   end function hash_iterator_key

   !#######################################################################

   subroutine hash_iterator_next_entry(this)
      ! Set the iterator to the next valid hash table value

      ! Dummy argument
      class(ccpp_hash_iterator_t)      :: this
      ! Local variable
      logical :: has_table_entry
      logical :: has_table_next

      if (this%index > 0) then
         ! We have initialized this table, so look for next entry
         has_table_entry = associated(this%table_entry)
         if (has_table_entry) then
            has_table_next = associated(this%table_entry%next)
         else
            has_table_next = .false.
         end if
         if (has_table_next) then
            this%table_entry => this%table_entry%next
         else if ((.not. has_table_entry) .and.                               &
              associated(this%hash_table%table(this%index)%next)) then
            this%table_entry => this%hash_table%table(this%index)%next
         else
            do
               if (this%index >= this%hash_table%table_size) then
                  this%index = 0
                  nullify(this%table_entry)
                  exit
               else
                  this%index = this%index + 1
                  nullify(this%table_entry)
                  ASSOCIATE(t_entry => this%hash_table%table(this%index))
                     if (associated(t_entry%entry_value)) then
                        exit
                     end if
                  END ASSOCIATE
               end if
            end do
         end if
      else
         ! This is an invalid iterator state
         nullify(this%table_entry)
      end if

   end subroutine hash_iterator_next_entry

   !#######################################################################

   logical function hash_iterator_is_valid(this) result(valid)
      ! Return .true. iff this iterator is in a valid (active entry) state

      ! Dummy arguments
      class(ccpp_hash_iterator_t)      :: this

      valid = .false.
      if ( (this%index > 0) .and.                                             &
           (this%index <= this%hash_table%table_size)) then
         valid = .true.
      end if

   end function hash_iterator_is_valid

   !#######################################################################

   function hash_iterator_value(this) result(val)
      ! Return the value or this hash iterator entry

      ! Dummy arguments
      class(ccpp_hash_iterator_t)     :: this
      class(ccpp_hashable_t), pointer :: val

      if (this%valid()) then
         if (associated(this%table_entry)) then
            val => this%table_entry%entry_value
         else
            val => this%hash_table%table(this%index)%entry_value
         end if
      else
         nullify(val)
      end if

   end function hash_iterator_value

   subroutine hash_iterator_finalize(this)
      ! Deallocate hash_table object

      ! Dummy arguments
      class(ccpp_hash_iterator_t)    :: this

      nullify(this%hash_table)

   end subroutine hash_iterator_finalize

end module ccpp_hash_table
