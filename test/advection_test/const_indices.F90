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
