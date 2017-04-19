module dummy_scm2

    use, intrinsic :: iso_c_binding,                                   &
                      only: c_f_pointer, c_ptr
    use            :: ccpp_types,                                      &
                      only: ccpp_t
    use            :: ccpp_fields,                                     &
                      only: ccpp_fields_get
    implicit none

    private
    public :: dummy_scm2_cap

    contains

    subroutine dummy_scm2_cap(ptr) bind(c)
        implicit none
        type(c_ptr), intent(inout) :: ptr

        type(ccpp_t), pointer  :: cdata
        real, pointer          :: t(:), u(:), v(:), q_v(:)
        integer                :: ierr

        call c_f_pointer(ptr, cdata)

        call ccpp_fields_get(cdata, 'temperature', t, ierr)
        call ccpp_fields_get(cdata, 'eastward_wind', u, ierr)
        call ccpp_fields_get(cdata, 'northward_wind', v, ierr)
        call ccpp_fields_get(cdata, 'water_vapor_specific_humidity', q_v, ierr)

        call dummy_scm2_run(t, u, v, q_v)
    end subroutine dummy_scm2_cap

    subroutine dummy_scm2_run(t, u, v, q_v)
        implicit none
        real, pointer, intent(inout) :: t(:), u(:), v(:), q_v(:)

        print *, 'In DUMMY_SCM2'


    end subroutine dummy_scm2_run

end module dummy_scm2
