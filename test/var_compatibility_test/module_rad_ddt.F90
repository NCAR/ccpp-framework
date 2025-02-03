module mod_rad_ddt
  USE ccpp_kinds, ONLY: kind_phys
  implicit none

  public ty_rad_lw, ty_rad_sw

  !> \section arg_table_module_rad_ddt Argument Table
  !! \htmlinclude arg_table_module_rad_ddt.html
  !!
  
  !> \section arg_table_ty_rad_lw  Argument Table
  !! \htmlinclude arg_table_ty_rad_lw.html
  !!
  type ty_rad_lw
     real(kind_phys) :: sfc_up_lw
     real(kind_phys) :: sfc_down_lw
  end type ty_rad_lw

  !> \section arg_table_ty_rad_sw  Argument Table
  !! \htmlinclude arg_table_ty_rad_sw.html
  !!
  type ty_rad_sw
     real(kind_phys) :: sfc_up_sw
     real(kind_phys) :: sfc_down_sw
  end type ty_rad_sw

end module mod_rad_ddt
 
