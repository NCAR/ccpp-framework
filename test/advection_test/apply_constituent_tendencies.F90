module apply_constituent_tendencies

  use ccpp_kinds, only: kind_phys

  implicit none
  private

  public :: apply_constituent_tendencies_run

CONTAINS

   !> \section arg_table_apply_constituent_tendencies_run Argument Table
   !!! \htmlinclude apply_constituent_tendencies_run.html
   subroutine apply_constituent_tendencies_run(const_tend, const, errcode, errmsg)
      ! Dummy arguments
      real(kind_phys),    intent(inout) :: const_tend(:,:,:)  ! constituent tendency array
      real(kind_phys),    intent(inout) :: const(:,:,:)       ! constituent state array
      integer,            intent(out)   :: errcode
      character(len=512), intent(out)   :: errmsg

      ! Local variables
      integer :: klev, jcnst, icol

      errcode = 0
      errmsg = ''

      do icol = 1, size(const_tend, 1)
         do klev = 1, size(const_tend, 2)
            do jcnst = 1, size(const_tend, 3)
               const(icol, klev, jcnst) = const(icol, klev, jcnst) + const_tend(icol, klev, jcnst)
            end do
         end do
      end do

      const_tend = 0._kind_phys

   end subroutine apply_constituent_tendencies_run

end module apply_constituent_tendencies
