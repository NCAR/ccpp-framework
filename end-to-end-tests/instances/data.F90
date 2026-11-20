module data

  !! \section arg_table_data Argument Table
  !! \htmlinclude data.html
  !!
  use ccpp_kinds, only : kind_phys

  implicit none

  private

  public ncols, nspecies, ninstances
  public instance_type, instance_data

  integer, parameter :: ncols = 4
  integer, parameter :: nspecies = 2
  integer, parameter :: ninstances = 2

  !! \section arg_table_instance_type Argument Table
  !! \htmlinclude instance_type.html
  !!
  type instance_type
    real(kind=kind_phys), dimension(1:ncols, 1:nspecies) :: data_array
    real(kind=kind_phys), dimension(1:ncols) :: data_array2
    logical :: opt_array_flag
  end type instance_type

  type(instance_type), dimension(1:ninstances), target :: instance_data

end module data
