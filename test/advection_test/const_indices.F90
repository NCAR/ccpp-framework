! Test collection of constituent indices
!

MODULE const_indices

   USE ccpp_kinds, ONLY: kind_phys

   IMPLICIT NONE
   PRIVATE

   PUBLIC :: const_indices_init
   PUBLIC :: const_indices_run

CONTAINS

   !> \section arg_table_const_indices_run  Argument Table
   !! \htmlinclude arg_table_const_indices_run.html
   !!
   subroutine const_indices_run(const_std_name, num_consts, test_stdname_array,  &
        const_index, const_inds, errmsg, errflg)
      use ccpp_constituent_prop_mod, only: int_unassigned
      use ccpp_scheme_utils,         only: ccpp_constituent_index
      use ccpp_scheme_utils,         only: ccpp_constituent_indices

      character(len=*),   intent(in)  :: const_std_name
      integer,            intent(in)  :: num_consts
      character(len=*),   intent(in)  :: test_stdname_array(:)
      integer,            intent(out) :: const_index
      integer,            intent(out) :: const_inds(:)
      character(len=512), intent(out) :: errmsg
      integer,            intent(out) :: errflg
      !----------------------------------------------------------------

      integer                         :: indx
      integer                         :: test_indx

      errmsg = ''
      errflg = 0

      ! Find the constituent index for <const_std_name>
      call ccpp_constituent_index(const_std_name, const_index, errflg, errmsg)
      if (errflg == 0) then
         call ccpp_constituent_indices(test_stdname_array, const_inds, errflg, errmsg)
      end if
      ! Check that a non-registered constituent is detectable but
      !   does not cause an error
      if (errflg == 0) then
         call ccpp_constituent_index('unobtainium', test_indx, errflg, errmsg)
         if (test_indx /= int_unassigned) then
            if (errflg == 0) then
               ! Do not add an error if one is already reported
               errflg = 2
               write(errmsg, '(2a,i0,a,i0)') "ccpp_constituent_index called for ", &
                    "'unobtainium' returned an index of ", test_indx, ", not ",    &
                    int_unassigned
            end if
         end if
      end if

   end subroutine const_indices_run

   !> \section arg_table_const_indices_init  Argument Table
   !! \htmlinclude arg_table_const_indices_init.html
   !!
   subroutine const_indices_init(const_std_name, num_consts, test_stdname_array, &
        const_index, const_inds, errmsg, errflg)
      use ccpp_scheme_utils, only: ccpp_constituent_index, ccpp_constituent_indices

      character(len=*),   intent(in)  :: const_std_name
      integer,            intent(in)  :: num_consts
      character(len=*),   intent(in)  :: test_stdname_array(:)
      integer,            intent(out) :: const_index
      integer,            intent(out) :: const_inds(:)
      character(len=512), intent(out) :: errmsg
      integer,            intent(out) :: errflg
      !----------------------------------------------------------------

      integer         :: indx

      errmsg = ''
      errflg = 0

      ! Find the constituent index for <const_std_name>
      call ccpp_constituent_index(const_std_name, const_index, errflg, errmsg)
      if (errflg == 0) then
         call ccpp_constituent_indices(test_stdname_array, const_inds, errflg, errmsg)
      end if

   end subroutine const_indices_init

   !! @}
   !! @}

END MODULE const_indices
