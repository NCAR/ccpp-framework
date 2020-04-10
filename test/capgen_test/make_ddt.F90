!Hello demonstration parameterization
!

MODULE make_ddt

  USE ccpp_kinds, ONLY: kind_phys

  IMPLICIT NONE
  PRIVATE

  PUBLIC :: make_ddt_init
  PUBLIC :: make_ddt_run
  PUBLIC :: make_ddt_finalize
  PUBLIC :: vmr_type

!> \section arg_table_vmr_type  Argument Table
!! \htmlinclude arg_table_vmr_type.html
!!
  type vmr_type
   integer :: nvmr
   real(kind_phys), allocatable :: vmr_array(:,:)
  end type


CONTAINS

!> \section arg_table_make_ddt_run  Argument Table
!! \htmlinclude arg_table_make_ddt_run.html
!!
  SUBROUTINE make_ddt_run(nbox, O3, HNO3, vmr, errmsg, errflg)
!----------------------------------------------------------------
   IMPLICIT NONE
!----------------------------------------------------------------

   integer,            intent(in)    :: nbox
   REAL(kind_phys),    intent(in)    :: O3(:)
   REAL(kind_phys),    intent(in)    :: HNO3(:)
   type(vmr_type),     intent(inout) :: vmr
   character(len=512), intent(out)   :: errmsg
   integer,            intent(out)   :: errflg
!----------------------------------------------------------------

    errmsg = ''
    errflg = 0


    ! NOTE -- This is prototyping one approach to passing a large number of
    ! chemical VMR values and is the predecssor for adding in methods and maybe
    ! nesting DDTs (especially for aerosols)
    vmr%vmr_array(:,1) = O3(:)
    vmr%vmr_array(:,2) = HNO3(:)

  END SUBROUTINE make_ddt_run

!> \section arg_table_make_ddt_init  Argument Table
!! \htmlinclude arg_table_make_ddt_init.html
!!
  subroutine make_ddt_init(nbox, vmr, errmsg, errflg)

    integer,            intent(in)  :: nbox
    type(vmr_type),     intent(out) :: vmr
    character(len=512), intent(out) :: errmsg
    integer,            intent(out) :: errflg

    ! This routine initializes the vmr array
    vmr%nvmr =  2
    allocate(vmr%vmr_array(nbox, vmr%nvmr))

    errmsg = ''
    errflg = 0

  end subroutine make_ddt_init

!> \section arg_table_make_ddt_finalize  Argument Table
!! \htmlinclude arg_table_make_ddt_finalize.html
!!
  subroutine make_ddt_finalize (errmsg, errflg)

    character(len=512),      intent(out)   :: errmsg
    integer,                 intent(out)   :: errflg

    ! This routine currently does nothing

    errmsg = ''
    errflg = 0

  end subroutine make_ddt_finalize

END MODULE make_ddt
