module wrapped_ddt
   ! CCPP wrapper for external ddt
   use external_module, only: unknown_external_ddt

   !> \section arg_table_wrapped_ddt_t Argument Table
   !! \htmlinclude wrapped_ddt_t.html
   type, public :: wrapped_ddt_t
      type(unknown_external_ddt) :: ext_ddt
   end type

end module wrapped_ddt
