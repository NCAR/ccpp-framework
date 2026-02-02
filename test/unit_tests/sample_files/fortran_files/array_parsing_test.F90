!Test array specifications
!

MODULE array_spec_test

   USE ccpp_kinds, ONLY: kind_phys

   IMPLICIT NONE
   PRIVATE

   PUBLIC :: array_spec_test_run

CONTAINS

   !> \section arg_table_array_spec_test_run  Argument Table
   !! \htmlinclude arg_table_array_spec_test_run.html
   !!
   SUBROUTINE array_spec_test_run(ncol, lev, good_arr1, good_arr2, good_arr3, &
        good_arr4, good_arr5, bad_arr1, bad_arr2, bad_arr3)

      integer,         intent(in)                      :: ncol, lev
      real(kind_phys), intent(in)                      :: good_arr1(ncol,lev)
      real(kind_phys), intent(in)                      :: good_arr2(:,:)
      real(kind_phys), intent(in), dimension(ncol,lev) :: good_arr3
      real(kind_phys), intent(in), dimension(:,:)      :: good_arr4
      real(kind_phys), intent(in)                      :: good_arr5(ncol,:)
      real(kind_phys), intent(in)                      :: bad_arr1(:,;)
      real(kind_phys), intent(in), dimension(;,:)      :: bad_arr2
      real(kind_phys), intent(in), dimension(:,;)      :: bad_arr3

   END SUBROUTINE array_spec_test_run

END MODULE array_spec_test
