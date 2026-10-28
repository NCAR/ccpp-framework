! Test parameterization whose local names collide with other identifiers
! in the group cap: <cld_ice_array> is also the local name of the cloud
! ice constituent (see cld_ice) and <ncols> is also the local name of the
! host model horizontal dimension. Both name a scheme-supplied
! interstitial here, so capgen must rename the group-cap locals.
!

module cld_shadow

  use ccpp_kinds, only: kind_phys

  implicit none
  private

  public :: cld_shadow_run

contains

  !> \section arg_table_cld_shadow_run  Argument Table
  !! \htmlinclude arg_table_cld_shadow_run.html
  !!
  subroutine cld_shadow_run(ncol, timestep, cld_ice_array, ncols,       &
       errmsg, errflg)

    integer,            intent(in)  :: ncol
    real(kind_phys),    intent(in)  :: timestep
    real(kind_phys),    intent(out) :: cld_ice_array(:,:)
    real(kind_phys),    intent(out) :: ncols(:)
    character(len=512), intent(out) :: errmsg
    integer,            intent(out) :: errflg
    !----------------------------------------------------------------

    errmsg = ''
    errflg = 0

    cld_ice_array(:ncol,:) = timestep
    ncols(:ncol) = real(ncol, kind_phys)

  end subroutine cld_shadow_run

end module cld_shadow
