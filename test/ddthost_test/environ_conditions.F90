module environ_conditions

  use ccpp_kinds, only: kind_phys

  implicit none
  private

  public :: environ_conditions_init
  public :: environ_conditions_run
  public :: environ_conditions_finalize

  integer, parameter :: input_model_times = 3
  integer, parameter :: input_model_values(input_model_times) = (/ 31, 37, 41 /)

contains

!> \section arg_table_environ_conditions_run  Argument Table
!! \htmlinclude arg_table_environ_conditions_run.html
!!
  subroutine environ_conditions_run(psurf, errmsg, errflg)

    ! This routine currently does nothing -- should update values

    real(kind_phys),         intent(in)    :: psurf(:)
    character(len=512),      intent(out)   :: errmsg
    integer,                 intent(out)   :: errflg

    errmsg = ''
    errflg = 0

  end subroutine environ_conditions_run

!> \section arg_table_environ_conditions_init  Argument Table
!! \htmlinclude arg_table_environ_conditions_init.html
!!
  subroutine environ_conditions_init (nbox, O3, HNO3, ntimes, model_times,    &
       errmsg, errflg)

   integer,              intent(in)  :: nbox
   real(kind_phys),      intent(out) :: O3(:)
   real(kind_phys),      intent(out) :: HNO3(:)
   integer,              intent(out) :: ntimes
   integer, allocatable, intent(out) :: model_times(:)
   character(len=512),   intent(out) :: errmsg
   integer,              intent(out) :: errflg
!----------------------------------------------------------------

   integer :: i, j

    errmsg = ''
    errflg = 0

    ! This may be replaced with MusicBox json environmental conditions reader???

    do i = 1, nbox
      O3(i)   = real(i, kind_phys) * 1.e-6_kind_phys
      HNO3(i) = real(i, kind_phys) * 1.e-9_kind_phys
    end do

    ntimes = input_model_times
    allocate(model_times(ntimes))
    model_times = input_model_values

  end subroutine environ_conditions_init

!> \section arg_table_environ_conditions_finalize  Argument Table
!! \htmlinclude arg_table_environ_conditions_finalize.html
!!
  subroutine environ_conditions_finalize (ntimes, model_times, errmsg, errflg)

    integer,            intent(in)  :: ntimes
    integer,            intent(in)  :: model_times(:)
    character(len=512), intent(out) :: errmsg
    integer,            intent(out) :: errflg

    ! This routine checks the size and values of model_times
    if (ntimes /= input_model_times) then
       errflg = 1
       write(errmsg, '(2(a,i0))') 'ntimes mismatch, ', ntimes, ' should be ', &
            input_model_times
    else if (size(model_times) /= input_model_times) then
       errflg = 1
       write(errmsg, '(2(a,i0))') 'model_times size mismatch, ',              &
            size(model_times), ' should be ', input_model_times
    else if (any(model_times /= input_model_values)) then
       errflg = 1
       write(errmsg, *) 'model_times mismatch, ',                             &
            model_times, ' should be ', input_model_values
    else
       errmsg = ''
       errflg = 0
    end if

  end subroutine environ_conditions_finalize

end module environ_conditions
