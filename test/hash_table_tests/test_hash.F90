module test_hash_utils
   use ccpp_hashable, only: ccpp_hashable_char_t

   implicit none
   private

   public :: test_table

   integer, parameter, public :: max_terrs = 16

   type, public :: hash_object_t
      type(ccpp_hashable_char_t), pointer :: item => NULL()
   end type hash_object_t

   private add_error

CONTAINS

   subroutine add_error(msg, num_errs, errors)
      ! Dummy arguments
      character(len=*),                intent(in)    :: msg
      integer,                         intent(inout) :: num_errs
      character(len=*),                intent(inout) :: errors(:)

      if (num_errs < max_terrs) then
         num_errs = num_errs + 1
         write(errors(num_errs), *) trim(msg)
      end if

   end subroutine add_error

   subroutine test_table(hash_table, table_size, num_tests, num_errs, errors)
      use ccpp_hash_table, only: ccpp_hash_table_t, ccpp_hash_iterator_t
      use ccpp_hashable,   only: ccpp_hashable_t, new_hashable_char

      ! Dummy arguments
      type(ccpp_hash_table_t), target, intent(inout) :: hash_table
      integer,                         intent(in)    :: table_size
      integer,                         intent(out)   :: num_tests
      integer,                         intent(out)   :: num_errs
      character(len=*),                intent(inout) :: errors(:)
      ! Local variables
      integer,                parameter :: num_test_entries = 4
      integer,                parameter :: key_len = 10
      character(len=key_len)            :: hash_names(num_test_entries) = (/  &
           'foo       ', 'bar       ', 'foobar    ', 'big daddy ' /)
      logical                           :: hash_found(num_test_entries)

      type(hash_object_t)               :: hash_chars(num_test_entries)
      class(ccpp_hashable_t), pointer   :: test_ptr => NULL()
      type(ccpp_hash_iterator_t)        :: hash_iter
      character(len=key_len)            :: test_key
      character(len=len(errors(1)))     :: errmsg
      integer                           :: index

      write(6, '(a,i0)') "Testing hash table, size = ", table_size
      num_tests = 0
      num_errs = 0
      ! Make sure hash table is *not* initialized
      if (hash_table%is_initialized()) then
         call add_error("Error: hash table initialized too early",            &
              num_errs, errors)
      end if
      num_tests = num_tests + 1
      ! Initialize hash table
      call hash_table%initialize(table_size)
      ! Make sure hash table is *is* initialized
      if (.not. hash_table%is_initialized()) then
         call add_error("Error: hash table *not* initialized", num_errs, errors)
      end if
      num_tests = num_tests + 1
      do index = 1, num_test_entries
         call new_hashable_char(hash_names(index), hash_chars(index)%item)
         call hash_table%add_hash_key(hash_chars(index)%item,                 &
              errmsg=errors(num_errs + 1))
         if (len_trim(errors(num_errs + 1)) > 0) then
            num_errs = num_errs + 1
         end if
         if (num_errs > max_terrs) then
            exit
         end if
      end do

      if (num_errs == 0) then
         ! We have populated the table, let's do some tests
         ! First, make sure we can find existing entries
         do index = 1, num_test_entries
            test_ptr => hash_table%table_value(hash_names(index),             &
                 errmsg=errors(num_errs + 1))
            if (len_trim(errors(num_errs + 1)) > 0) then
               num_errs = num_errs + 1
            else if (trim(test_ptr%key()) /= trim(hash_names(index))) then
               num_errs = num_errs + 1
               write(errmsg, *) "ERROR: Found '", trim(test_ptr%key()),       &
                    "', expected '", trim(hash_names(index)), "'"
               call add_error(trim(errmsg), num_errs, errors)
            end if
            if (num_errs > max_terrs) then
               exit
            end if
         end do
         num_tests = num_tests + 1
         ! Next, make sure we do not find a non-existent entry
         test_ptr => hash_table%table_value(trim(hash_names(1))//'_oops',     &
              errmsg=errors(num_errs + 1))
         if (len_trim(errors(num_errs + 1)) == 0) then
            write(errmsg, *) "ERROR: Found an entry for '",                   &
                 trim(hash_names(1))//'_oops', "'"
            call add_error(trim(errmsg), num_errs, errors)
         end if
         num_tests = num_tests + 1
         ! Make sure we get an error if we try to add a duplicate key
         call hash_table%add_hash_key(hash_chars(2)%item,                     &
              errmsg=errors(num_errs + 1))
         if (len_trim(errors(num_errs + 1)) == 0) then
            num_errs = num_errs + 1
            write(errors(num_errs), *)                                        &
                 "ERROR: Allowed duplicate entry for '",                      &
                 hash_chars(2)%item%key(), "'"
         end if
         num_tests = num_tests + 1
         ! Check that the total number of table entries is correct
         if (hash_table%num_values() /= num_test_entries) then
            write(errmsg, '(2(a,i0))') "ERROR: Wrong table value count, ",    &
                 hash_table%num_values(), ', should be ', num_test_entries
            call add_error(errmsg, num_errs, errors)
         end if
         num_tests = num_tests + 1
         ! Test iteration through hash table
         hash_found(:) = .false.
         call hash_iter%initialize(hash_table)
         num_tests = num_tests + 1
         do
            if (hash_iter%valid()) then
               test_key = hash_iter%key()
               index = 1
               do
                  if (trim(test_key) == trim(hash_names(index))) then
                     hash_found(index) = .true.
                     exit
                  else if (index >= num_test_entries) then
                     write(errmsg, '(3a)')                                    &
                          "ERROR: Unexpected table entry, '",                 &
                          trim(test_key), "'"
                     call add_error(errmsg, num_errs, errors)
                  end if
                  index = index + 1
               end do
               call hash_iter%next()
            else
               exit
            end if
         end do
         call hash_iter%finalize()
         if (ANY(.not. hash_found)) then
            write(errmsg, '(a,i0,a)') "ERROR: ",                              &
                 COUNT(.not. hash_found), " test keys not found in table."
            call add_error(errmsg, num_errs, errors)
         end if
      end if
      ! Finally, clear the hash table (should deallocate everything)
      call hash_table%clear()
      ! Make sure hash table is *not* initialized
      if (hash_table%is_initialized()) then
         call add_error("Error: hash table initialized after clear",          &
              num_errs, errors)
      end if
      num_tests = num_tests + 1
      ! Cleanup
      do index = 1, num_test_entries
         deallocate(hash_chars(index)%item)
      end do

   end subroutine test_table

end module test_hash_utils

program test_hash
   use ccpp_hash_table, only: ccpp_hash_table_t
   use test_hash_utils, only: test_table, max_terrs

   integer,                parameter :: num_table_sizes = 5
   integer,                parameter :: max_errs = max_terrs * num_table_sizes
   integer,                parameter :: err_size = 128
   integer,                parameter :: test_sizes(num_table_sizes) = (/      &
        0, 1, 2, 4, 20 /)

   type(ccpp_hash_table_t), target   :: hash_table
   integer                           :: index
   integer                           :: errcnt = 0
   integer                           :: num_tests = 0
   integer                           :: total_errcnt = 0
   integer                           :: total_tests = 0
   character(len=err_size)           :: errors(max_errs)

   errors = ''
   do index = 1, num_table_sizes
      call test_table(hash_table, test_sizes(index), num_tests, errcnt,       &
           errors(total_errcnt+1:))
      total_tests = total_tests + num_tests
      total_errcnt = total_errcnt + errcnt
   end do

   if (total_errcnt > 0) then
      write(6, '(a,i0,a)') 'FAIL, ', total_errcnt, ' errors found'
      do index = 1, total_errcnt
         write(6, *) trim(errors(index))
      end do
      STOP 1
   else
      write(6, '(a,i0,a)') "All ", total_tests, " hash table tests passed!"
      STOP 0
   end if

end program test_hash
