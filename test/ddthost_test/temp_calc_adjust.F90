!Test parameterization with no vertical level and hanging intent(out) variable
!

module temp_calc_adjust

   use ccpp_kinds, only: kind_phys

   implicit none
   private

   public :: temp_calc_adjust_init
   public :: temp_calc_adjust_run
   public :: temp_calc_adjust_finalize

contains

   !> \section arg_table_temp_calc_adjust_run  Argument Table
   !! \htmlinclude arg_table_temp_calc_adjust_run.html
   !!
   subroutine temp_calc_adjust_run(nbox, timestep, temp_level, temp_calc,     &
        errmsg, errflg)

      integer,            intent(in)    :: nbox
      real(kind_phys),    intent(in)    :: timestep
      real(kind_phys),    intent(in)    :: temp_level(:,:)
      real(kind_phys),    intent(out)   :: temp_calc(:)
      character(len=512), intent(out)   :: errmsg
      integer,            intent(out)   :: errflg
      !----------------------------------------------------------------

      integer         :: col_index
      real(kind_phys) :: bar = 1.0_kind_phys

      errmsg = ''
      errflg = 0

      call temp_calc_adjust_nested_subroutine(temp_calc)
      if (check_foo()) then
         call foo(bar)
      end if

   contains

      elemental subroutine temp_calc_adjust_nested_subroutine(temp)

         real(kind_phys),    intent(out)   :: temp
         !-------------------------------------------------------------

         temp = 1.0_kind_phys

      end subroutine temp_calc_adjust_nested_subroutine

      subroutine foo(bar)
         real(kind_phys), intent(inout) :: bar
         bar = bar + 1.0_kind_phys

      end subroutine

      logical function check_foo()
         check_foo = .true.
      end function check_foo

   end subroutine

   !> \section arg_table_temp_calc_adjust_init  Argument Table
   !! \htmlinclude arg_table_temp_calc_adjust_init.html
   !!
   subroutine temp_calc_adjust_init (errmsg, errflg)

      character(len=512),      intent(out)   :: errmsg
      integer,                 intent(out)   :: errflg

      ! This routine currently does nothing

      errmsg = ''
      errflg = 0

   end subroutine temp_calc_adjust_init

   !> \section arg_table_temp_calc_adjust_finalize  Argument Table
   !! \htmlinclude arg_table_temp_calc_adjust_finalize.html
   !!
   subroutine temp_calc_adjust_finalize (errmsg, errflg)

      character(len=512),      intent(out)   :: errmsg
      integer,                 intent(out)   :: errflg

      ! This routine currently does nothing

      errmsg = ''
      errflg = 0

   end subroutine temp_calc_adjust_finalize

end module temp_calc_adjust
