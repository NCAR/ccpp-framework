!Hello demonstration parameterization
!

module make_ddt

   use ccpp_kinds, only: kind_phys

   implicit none
   private

   public :: make_ddt_init
   public :: make_ddt_run
   public :: make_ddt_timestep_final
   public :: vmr_type

   !> \section arg_table_vmr_type  Argument Table
   !! \htmlinclude arg_table_vmr_type.html
   !!
   type vmr_type
      integer :: nvmr
      real(kind_phys), allocatable :: vmr_array(:,:)
   end type vmr_type


contains

   !> \section arg_table_make_ddt_run  Argument Table
   !! \htmlinclude arg_table_make_ddt_run.html
   !!
   subroutine make_ddt_run(cols, cole, O3, HNO3, vmr, errmsg, errflg)
      !----------------------------------------------------------------
      implicit none
      !----------------------------------------------------------------

      ! Dummy arguments
      integer,            intent(in)    :: cols
      integer,            intent(in)    :: cole
      real(kind_phys),    intent(in)    :: O3(:)
      real(kind_phys),    intent(in)    :: HNO3(:)
      type(vmr_type),     intent(inout) :: vmr
      character(len=512), intent(out)   :: errmsg
      integer,            intent(out)   :: errflg
      ! Local variable
      integer :: nbox
      !----------------------------------------------------------------

      errmsg = ''
      errflg = 0

      ! Check for correct threading behavior
      nbox = cole - cols + 1
      if (SIZE(O3) /= nbox) then
         errflg = 1
         write(errmsg, '(2(a,i0))') 'SIZE(O3) = ', SIZE(O3), ', should be ', nbox
      else if (SIZE(HNO3) /= nbox) then
         errflg = 1
         write(errmsg, '(2(a,i0))') 'SIZE(HNO3) = ', SIZE(HNO3),               &
              ', should be ', nbox
      else
         ! NOTE -- This is prototyping one approach to passing a large number of
         ! chemical VMR values and is the predecessor for adding in methods and
         ! maybe nesting DDTs (especially for aerosols)
         vmr%vmr_array(cols:cole, 1) = O3(:)
         vmr%vmr_array(cols:cole, 2) = HNO3(:)
      end if

   end subroutine make_ddt_run

   !> \section arg_table_make_ddt_init  Argument Table
   !! \htmlinclude arg_table_make_ddt_init.html
   !!
   subroutine make_ddt_init(nbox, vmr, errmsg, errflg)

      ! Dummy arguments
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

   !> \section arg_table_make_ddt_timestep_final  Argument Table
   !! \htmlinclude arg_table_make_ddt_timestep_final.html
   !!
   subroutine make_ddt_timestep_final (ncols, vmr, errmsg, errflg)

      ! Dummy arguments
      integer,            intent(in)  :: ncols
      type(vmr_type),     intent(in)  :: vmr
      character(len=512), intent(out) :: errmsg
      integer,            intent(out) :: errflg
      ! Local variables
      integer         :: index
      real(kind_phys) :: rind

      errmsg = ''
      errflg = 0

      ! This routine checks the array values in vmr
      if (SIZE(vmr%vmr_array, 1) /= ncols) then
         errflg = 1
         write(errmsg, '(2(a,i0))') 'VMR%VMR_ARRAY first dimension size is, ', &
              SIZE(vmr%vmr_array, 1), ', should be, ', ncols
      else
         do index = 1, ncols
            rind = real(index, kind_phys)
            if (vmr%vmr_array(index, 1) /= rind * 1.e-6_kind_phys) then
               errflg = 1
               write(errmsg, '(a,i0,2(a,e12.4))') 'O3(', index, ') = ',        &
                    vmr%vmr_array(index, 1), ', should be, ',                  &
                    rind * 1.e-6_kind_phys
               exit
            else if (vmr%vmr_array(index, 2) /= rind * 1.e-9_kind_phys) then
               errflg = 1
               write(errmsg, '(a,i0,2(a,e12.4))') 'HNO3(', index, ') = ',      &
                    vmr%vmr_array(index, 2), ', should be, ',                  &
                    rind * 1.e-9_kind_phys
               exit
            end if
         end do
      end if

   end subroutine make_ddt_timestep_final

end module make_ddt
