!Test parameterization with no vertical level and hanging intent(out) variable
!

MODULE temp_calc_adjust

  USE ccpp_kinds, ONLY: kind_phys

  IMPLICIT NONE
  PRIVATE

  PUBLIC :: temp_calc_adjust_init
  PUBLIC :: temp_calc_adjust_run
  PUBLIC :: temp_calc_adjust_finalize

CONTAINS

!> \section arg_table_temp_calc_adjust_run  Argument Table
!! \htmlinclude arg_table_temp_calc_adjust_run.html
!!
  SUBROUTINE temp_calc_adjust_run(nbox, timestep, temp_calc, errmsg, errflg)

   integer,            intent(in)    :: nbox
   real(kind_phys),    intent(in)    :: timestep
   REAL(kind_phys),    intent(out)   :: temp_calc(:)
   character(len=512), intent(out)   :: errmsg
   integer,            intent(out)   :: errflg
!----------------------------------------------------------------

   integer :: col_index

    errmsg = ''
    errflg = 0

    temp_calc = 1.0_kind_phys

  END SUBROUTINE temp_calc_adjust_run

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

END MODULE temp_calc_adjust
