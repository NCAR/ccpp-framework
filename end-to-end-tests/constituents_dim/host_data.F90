module host_data

  !! \section arg_table_host_data Argument Table
  !! \htmlinclude host_data.html
  !!
  use ccpp_kinds, only: kind_phys

  implicit none

  private

  public :: ncols, pver, coupler_flux

  ! Small single-chunk domain.
  integer, parameter :: ncols = 4
  integer, parameter :: pver  = 4

  ! Case 1: a host-owned array dimensioned by number_of_ccpp_constituents.
  ! The host allocates it to the runtime constituent count; capgen passes the
  ! whole constituent axis (':') to the consuming scheme.
  real(kind=kind_phys), allocatable, target :: coupler_flux(:, :)

end module host_data
