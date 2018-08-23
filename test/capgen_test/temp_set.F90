!Test 3D parameterization
!

MODULE temp_set

  USE ccpp_kinds, ONLY: kind_phys

  IMPLICIT NONE
  PRIVATE

  PUBLIC :: temp_set_init
  PUBLIC :: temp_set_timestep_initialize
  PUBLIC :: temp_set_run
  PUBLIC :: temp_set_finalize

CONTAINS

!> \section arg_table_temp_set_run  Argument Table
!! \htmlinclude arg_table_temp_set_run.html
!!
  SUBROUTINE temp_set_run(ncol, lev, ilev, timestep, temp_level, temp_layer,  &
       errmsg, errflg)
!----------------------------------------------------------------
   IMPLICIT NONE
!----------------------------------------------------------------

   integer,            intent(in)    :: ncol, lev, ilev
   REAL(kind_phys),    intent(out)   :: temp_layer(:,:)
   real(kind_phys),    intent(in)    :: timestep
   REAL(kind_phys),    INTENT(inout) :: temp_level(:, :)
   character(len=512), intent(out)   :: errmsg
   integer,            intent(out)   :: errflg
!----------------------------------------------------------------

   integer :: col_index
   integer :: lev_index

    errmsg = ''
    errflg = 0

    if (ilev /= (lev + 1)) then
       errflg = 1
       errmsg = 'Invalid value for ilev, must be lev+1'
       return
    end if

    do col_index = 1, ncol
       do lev_index = 1, lev
          temp_layer(col_index, lev_index) = (temp_level(col_index, lev_index) &
               + temp_level(col_index, lev_index + 1)) / 2.0_kind_phys
       end do
    end do

  END SUBROUTINE temp_set_run

!> \section arg_table_temp_set_init  Argument Table
!! \htmlinclude arg_table_temp_set_init.html
!!
  subroutine temp_set_init(temp_inc_in, temp_inc_set, errmsg, errflg)

    real(kind_phys),         intent(in)    :: temp_inc_in
    real(kind_phys),         intent(out)   :: temp_inc_set
    character(len=512),      intent(out)   :: errmsg
    integer,                 intent(out)   :: errflg

    temp_inc_set = temp_inc_in

    errmsg = ''
    errflg = 0

  end subroutine temp_set_init

!> \section arg_table_temp_set_timestep_initialize  Argument Table
!! \htmlinclude arg_table_temp_set_timestep_initialize.html
!!
  subroutine temp_set_timestep_initialize(ncol, ilev, temp_inc, temp_level,   &
       errmsg, errflg)

    integer,            intent(in)    :: ncol, ilev
    real(kind_phys),    intent(in)    :: temp_inc
    real(kind_phys),    intent(inout) :: temp_level(ncol, ilev)
    character(len=512), intent(out)   :: errmsg
    integer,            intent(out)   :: errflg

    errmsg = ''
    errflg = 0

    temp_level = temp_level + temp_inc

  end subroutine temp_set_timestep_initialize

!> \section arg_table_temp_set_finalize  Argument Table
!! \htmlinclude arg_table_temp_set_finalize.html
!!
  subroutine temp_set_finalize(errmsg, errflg)

    character(len=512),      intent(out)   :: errmsg
    integer,                 intent(out)   :: errflg

    ! This routine currently does nothing

    errmsg = ''
    errflg = 0

  end subroutine temp_set_finalize

END MODULE temp_set
