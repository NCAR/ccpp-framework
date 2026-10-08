module data

  !! \section arg_table_data Argument Table
  !! \htmlinclude data.html
  !!
  use ccpp_kinds, only: kind_phys

  implicit none

  private

  public :: checksum

  ! Host-owned scalar the consuming scheme fills from the suite-owned workspace.
  real(kind=kind_phys) :: checksum

end module data
