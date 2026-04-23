!Hello demonstration parameterization
!

MODULE hello_scheme

  USE ccpp_kinds, ONLY: kind_phys

  IMPLICIT NONE
  PRIVATE

  PUBLIC :: hello_scheme_init
  PUBLIC :: hello_scheme_run
  PUBLIC :: hello_scheme_finalize

CONTAINS

!> \section arg_table_hello_scheme_run  Argument Table
!! \htmlinclude arg_table_hello_scheme_run.html
!!
  SUBROUTINE hello_scheme_run(ncol, lev, ilev, timestep, temp_level,          &
       temp_layer, errmsg, errflg)
!----------------------------------------------------------------
   IMPLICIT NONE
!----------------------------------------------------------------

   integer,            intent(in)    :: ncol, lev, ilev
   REAL(kind_phys),    intent(inout) :: temp_level(:, :)
   real(kind_phys),    intent(in)    :: timestep
   REAL(kind_phys),    INTENT(out)   :: temp_layer(:, :)
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

  END SUBROUTINE hello_scheme_run

!> \section arg_table_hello_scheme_init  Argument Table
!! \htmlinclude arg_table_hello_scheme_init.html
!!
  subroutine hello_scheme_init (errmsg, errflg)

    character(len=512),      intent(out)   :: errmsg
    integer,                 intent(out)   :: errflg

    ! This routine currently does nothing

    errmsg = ''
    errflg = 0

  end subroutine hello_scheme_init

!> \section arg_table_hello_scheme_finalize  Argument Table
!! \htmlinclude arg_table_hello_scheme_finalize.html
!!
  subroutine hello_scheme_finalize (errmsg, errflg)

    character(len=512),      intent(out)   :: errmsg
    integer,                 intent(out)   :: errflg

    ! This routine currently does nothing

    errmsg = ''
    errflg = 0

  end subroutine hello_scheme_finalize

END MODULE hello_scheme
