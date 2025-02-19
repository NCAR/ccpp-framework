module dme_adjust

   use ccpp_kinds, only:  kind_phys

   implicit none

contains
!===============================================================================
!> \section arg_table_do_stuff_run Argument Table
!! \htmlinclude do_stuff_run.html
!!
  subroutine do_stuff_run(const_props, twilight_zone, errmsg, errflg)
    !
    ! Arguments
    !
    type(ccpp_constituent_prop_ptr_t), intent(in)    :: const_props(:)
    type(serling_t),                   intent(inout) :: twilight_zone

    character(len=512),                intent(out)   :: errmsg
    integer,                           intent(out)   :: errflg

    errmsg = ' '
    errflg = 0
    twilight_zone('adjust_set')

  end subroutine dme_adjust_run

end module dme_adjust
