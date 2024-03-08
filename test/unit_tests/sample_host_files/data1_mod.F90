module data1_mod

   use ccpp_kinds, only: kind_phys

   !> \section arg_table_data1_mod  Argument Table
   !! \htmlinclude arg_table_data1_mod.html
   real(kind_phys)              :: ps1
   real(kind_phys), allocatable :: xbox(:,:)
   real(kind_phys), allocatable :: switch(:,:)

end module data1_mod
