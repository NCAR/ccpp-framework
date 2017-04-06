!>
!! @brief Physics fields module.
!!
!! Routines and functions to interact with physics fields.
!! Most of the work is carried out in C (ccpp_field_idx.c).
!! The IPD contains an array of C pointers to all the
!! fields passed around. This array needs an index so
!! one can field the requested field.
!!
!! Example usage in the atmosphere driver cap.
!! @code{.f90}
!!
!! ! Initialize the field array, specifying the maximum number
!! ! of fields that will be added.
!! call ccpp_field_init(ipd_data, max_fields, ierr)
!! if (ierr /= 0) then
!!   call exit(ierr)
!! end if
!!
!! ! Add a field, for example the eastward_wind.
!! call ccpp_field_add(ipd_data, 'eastward_wind', 'm s-1',  &
!!                         u, ierr)
!! if (ierr /= 0) then
!!   call exit(ierr)
!! end if
!!
!! @endcode
!!
!! Example usage in a physics scheme cap.
!! @code{.f90}
!!
!! ! Extract a field, for example the eastward_wind.
!! call ccpp_field_data(ipd_data, 'eastward_wind', u, ierr)
!! if (ierr /= 0) then
!!   call exit(ierr)
!! end if
!!
!! @endcode
!
module ccpp_fields

    use, intrinsic :: iso_c_binding
    use            :: kinds,                                            &
                      only: i_sp, r_dp
    use            :: ccpp_types,                                       &
                      only: STR_LEN, suite_t, ipd_t, subcycle_t,        &
                            aip_t, field_t
    use            :: ccpp_strings,                                     &
                      only: ccpp_cstr
    use            :: ccpp_errors,                                      &
                      only: ccpp_warn

    implicit none

    private
    public :: ccpp_field_init,                                         &
              ccpp_field_fini,                                         &
              ccpp_field_find,                                         &
              ccpp_field_add,                                          &
              ccpp_field_data

    interface ccpp_field_add
       module procedure :: ccpp_field_add_i0
       module procedure :: ccpp_field_add_i1
       module procedure :: ccpp_field_add_i2
       module procedure :: ccpp_field_add_i3
       module procedure :: ccpp_field_add_r0
       module procedure :: ccpp_field_add_r1
       module procedure :: ccpp_field_add_r2
       module procedure :: ccpp_field_add_r3
    end interface ccpp_field_add

    interface ccpp_field_data
       module procedure :: ccpp_field_data_i0
       module procedure :: ccpp_field_data_i1
       module procedure :: ccpp_field_data_i2
       module procedure :: ccpp_field_data_i3
       module procedure :: ccpp_field_data_r0
       module procedure :: ccpp_field_data_r1
       module procedure :: ccpp_field_data_r2
       module procedure :: ccpp_field_data_r3
    end interface ccpp_field_data

    interface

       integer(c_int32_t)                                              &
       function ccpp_field_idx_init                                &
                (idx)                                                  &
                bind(c, name='ccpp_field_idx_init')
        import :: c_int32_t, c_ptr
        type(c_ptr)                          :: idx
       end function ccpp_field_idx_init

       integer(c_int32_t)                                              &
       function ccpp_field_idx_fini                                &
                (idx)                                                  &
                bind(c, name='ccpp_field_idx_fini')
        import :: c_int32_t, c_ptr
        type(c_ptr)                          :: idx
       end function ccpp_field_idx_fini

       integer(c_int32_t)                                              &
       function ccpp_field_idx_add                                 &
                (name, idx)                                            &
                bind(c, name='ccpp_field_idx_add')
        import :: c_int32_t, c_char, c_ptr
        character(kind=c_char), dimension(*) :: name
        type(c_ptr)                          :: idx
       end function ccpp_field_idx_add

       integer(c_int32_t)                                              &
       function ccpp_field_idx_find                                &
                (name, idx)                                            &
                bind(c, name='ccpp_field_idx_find')
        import :: c_char, c_int32_t, c_ptr
        character(kind=c_char), dimension(*) :: name
        type(c_ptr)                          :: idx
       end function ccpp_field_idx_find

       !integer(c_int32_t)                                              &
       !function ccpp_field_idx_sort                                &
       !         (idx)                                                  &
       !         bind(c, name='ccpp_field_idx_sort')
       ! import :: c_int32_t, c_ptr
       ! type(c_ptr)                          :: idx
       !end function ccpp_field_idx_sort

       integer(c_int32_t)                                              &
       function ccpp_field_idx_grow                                &
                (idx)                                                  &
                bind(c, name='ccpp_field_idx_grow')
        import :: c_int32_t, c_ptr
        type(c_ptr)                          :: idx
       end function ccpp_field_idx_grow

       integer(c_int32_t)                                              &
       function ccpp_field_idx_max                                 &
                (idx)                                                  &
                bind(c, name='ccpp_field_idx_max')
        import :: c_int32_t, c_ptr
        type(c_ptr)                          :: idx
       end function ccpp_field_idx_max

   end interface

   contains

    subroutine ccpp_field_init(ap_data, ierr)
        type(aip_t),            intent(inout) :: ap_data
        integer,                intent(  out) :: ierr

        integer                               :: fields_max

        ierr = 0

        ierr = ccpp_field_idx_init(ap_data%fields_idx)
        if (ierr /= 0) then
            call ccpp_warn('Unable to initalize ap_data field index')
            return
        end if

        fields_max = ccpp_field_idx_max(ap_data%fields_idx)

        allocate(ap_data%fields(fields_max), stat=ierr)
        if (ierr /= 0) then
            call ccpp_warn('Unable to allocate ap_data fields')
            return
        end if

    end subroutine ccpp_field_init

    subroutine ccpp_field_add_ptr(ap_data, standard_name, units, ptr, &
                                  rank, dims, ierr)
        type(aip_t),                     intent(inout) :: ap_data
        character(len=*),                intent(in)    :: standard_name
        character(len=*),                intent(in)    :: units
        type(c_ptr),                     intent(in)    :: ptr
        integer,               optional, intent(in)    :: rank
        integer, dimension(:), optional, intent(in)    :: dims
        integer,                         intent(  out) :: ierr

        integer                                        :: i
        integer                                        :: fields_max
        type(field_t), allocatable, dimension(:)       :: tmp

        ierr = 0

        ! Add ourselves to the index and get our array position
        i = ccpp_field_idx_add(ccpp_cstr(standard_name), ap_data%fields_idx)
        if (i .lt. 1) then
            call ccpp_warn('Unable to add field index: '//trim(standard_name))
            return
        end if

        fields_max = ccpp_field_idx_max(ap_data%fields_idx)
        if (i .eq. fields_max) then
            ierr = ccpp_field_idx_grow(ap_data%fields_idx)
            fields_max = ccpp_field_idx_max(ap_data%fields_idx)
            allocate(tmp(fields_max), stat=ierr)
            if (ierr /= 0) then
                call ccpp_warn('Unable to grow ap_data fields array')
                return
            end if
            tmp(1:size(ap_data%fields)) = ap_data%fields
            call move_alloc(tmp, ap_data%fields)
        end if

        ap_data%fields(i)%standard_name = standard_name
        ap_data%fields(i)%units         = units
        ap_data%fields(i)%ptr           = ptr

        if (present(rank)) then
            ap_data%fields(i)%rank      = rank
        else
            ap_data%fields(i)%rank      = 0
        end if

        if (present(dims)) then
            allocate(ap_data%fields(i)%dims(rank), stat=ierr)
            if (ierr /= 0) then
                call ccpp_warn('Unable to allocate ap_data fields dims')
                return
            end if
            ap_data%fields(i)%dims      = dims
        end if

    end subroutine ccpp_field_add_ptr

    function ccpp_field_find(ap_data, standard_name, ierr) result(location)
        type(aip_t),            intent(in)    :: ap_data
        character(len=*),       intent(in)    :: standard_name
        integer,                intent(  out) :: ierr

        integer                               :: location

        location = ccpp_field_idx_find(ccpp_cstr(standard_name), &
                                      ap_data%fields_idx)
        if (location <= 0) then
            ierr = 1
        end if

    end function ccpp_field_find

    subroutine ccpp_field_fini(ap_data, ierr)
        type(aip_t),            intent(inout) :: ap_data
        integer,                intent(  out) :: ierr

        ierr = 0

        if (allocated(ap_data%fields)) then
            deallocate(ap_data%fields)
        end if

        ierr = ccpp_field_idx_fini(ap_data%fields_idx)

    end subroutine ccpp_field_fini

    subroutine ccpp_field_add_i0(ap_data, standard_name, units, ptr, ierr)
        type(aip_t),                 intent(inout) :: ap_data
        character(len=*),            intent(in)    :: standard_name
        character(len=*),            intent(in)    :: units
        integer(kind=i_sp), pointer, intent(in)    :: ptr
        integer,                     intent(  out) :: ierr

        ierr = 0
        call ccpp_field_add_ptr(ap_data, standard_name, units, &
                               c_loc(ptr), ierr=ierr)

    end subroutine ccpp_field_add_i0

    subroutine ccpp_field_add_i1(ap_data, standard_name, units, ptr, ierr)
        type(aip_t),                 intent(inout) :: ap_data
        character(len=*),            intent(in)    :: standard_name
        character(len=*),            intent(in)    :: units
        integer(kind=i_sp), pointer, intent(in)    :: ptr(:)
        integer,                     intent(  out) :: ierr

        ierr = 0
        call ccpp_field_add_ptr(ap_data, standard_name, units, &
                               c_loc(ptr), size(ptr), shape(ptr), ierr=ierr)

    end subroutine ccpp_field_add_i1

    subroutine ccpp_field_add_i2(ap_data, standard_name, units, ptr, ierr)
        type(aip_t),                 intent(inout) :: ap_data
        character(len=*),            intent(in)    :: standard_name
        character(len=*),            intent(in)    :: units
        integer(kind=i_sp), pointer, intent(in)    :: ptr(:,:)
        integer,                     intent(  out) :: ierr

        ierr = 0
        call ccpp_field_add_ptr(ap_data, standard_name, units, &
                               c_loc(ptr), size(ptr), shape(ptr), ierr=ierr)

    end subroutine ccpp_field_add_i2

    subroutine ccpp_field_add_i3(ap_data, standard_name, units, ptr, ierr)
        type(aip_t),                 intent(inout) :: ap_data
        character(len=*),            intent(in)    :: standard_name
        character(len=*),            intent(in)    :: units
        integer(kind=i_sp), pointer, intent(in)    :: ptr(:,:,:)
        integer,                     intent(  out) :: ierr

        ierr = 0
        call ccpp_field_add_ptr(ap_data, standard_name, units, &
                               c_loc(ptr), size(ptr), shape(ptr), ierr=ierr)

    end subroutine ccpp_field_add_i3

    subroutine ccpp_field_add_r0(ap_data, standard_name, units, ptr, ierr)
        type(aip_t),                 intent(inout) :: ap_data
        character(len=*),            intent(in)    :: standard_name
        character(len=*),            intent(in)    :: units
        real(kind=r_dp), pointer,    intent(in)    :: ptr
        integer,                     intent(  out) :: ierr

        ierr = 0
        call ccpp_field_add_ptr(ap_data, standard_name, units, &
                               c_loc(ptr), ierr=ierr)

    end subroutine ccpp_field_add_r0

    subroutine ccpp_field_add_r1(ap_data, standard_name, units, ptr, ierr)
        type(aip_t),                 intent(inout) :: ap_data
        character(len=*),            intent(in)    :: standard_name
        character(len=*),            intent(in)    :: units
        real(kind=r_dp), pointer,    intent(in)    :: ptr(:)
        integer,                     intent(  out) :: ierr

        ierr = 0
        call ccpp_field_add_ptr(ap_data, standard_name, units, &
                               c_loc(ptr), size(ptr), shape(ptr), ierr=ierr)

    end subroutine ccpp_field_add_r1

    subroutine ccpp_field_add_r2(ap_data, standard_name, units, ptr, ierr)
        type(aip_t),                 intent(inout) :: ap_data
        character(len=*),            intent(in)    :: standard_name
        character(len=*),            intent(in)    :: units
        real(kind=r_dp), pointer,    intent(in)    :: ptr(:,:)
        integer,                     intent(  out) :: ierr

        ierr = 0
        call ccpp_field_add_ptr(ap_data, standard_name, units, &
                               c_loc(ptr), size(ptr), shape(ptr), ierr=ierr)

    end subroutine ccpp_field_add_r2

    subroutine ccpp_field_add_r3(ap_data, standard_name, units, ptr, ierr)
        type(aip_t),                 intent(inout) :: ap_data
        character(len=*),            intent(in)    :: standard_name
        character(len=*),            intent(in)    :: units
        real(kind=r_dp), pointer,    intent(in)    :: ptr(:,:,:)
        integer,                     intent(  out) :: ierr

        ierr = 0
        call ccpp_field_add_ptr(ap_data, standard_name, units, &
                               c_loc(ptr), size(ptr), shape(ptr), ierr=ierr)

    end subroutine ccpp_field_add_r3

    subroutine ccpp_field_data_i0(ap_data, standard_name, ptr, ierr)
        type(aip_t),                 intent(in)    :: ap_data
        character(len=*),            intent(in)    :: standard_name
        integer(kind=i_sp), pointer, intent(out)   :: ptr
        integer,                     intent(  out) :: ierr

        integer                                    :: idx

        ierr = 0
        ! Lookup the standard name in the index
        idx = ccpp_field_find(ap_data, standard_name, ierr)
        if (ierr /= 0) then
            call ccpp_warn('Unable to find the requested field')
            return
        end if

        call c_f_pointer(ap_data%fields(idx)%ptr, ptr)

    end subroutine ccpp_field_data_i0

    subroutine ccpp_field_data_i1(ap_data, standard_name, ptr, ierr)
        type(aip_t),                 intent(in)    :: ap_data
        character(len=*),            intent(in)    :: standard_name
        integer(kind=i_sp), pointer, intent(out)   :: ptr(:)
        integer,                     intent(  out) :: ierr

        integer                                    :: idx

        ierr = 0
        ! Lookup the standard name in the index
        idx = ccpp_field_find(ap_data, standard_name, ierr)
        if (ierr /= 0) then
            call ccpp_warn('Unable to find the requested field')
            return
        end if

        call c_f_pointer(ap_data%fields(idx)%ptr, ptr, ap_data%fields(idx)%dims)

    end subroutine ccpp_field_data_i1

    subroutine ccpp_field_data_i2(ap_data, standard_name, ptr, ierr)
        type(aip_t),                 intent(in)    :: ap_data
        character(len=*),            intent(in)    :: standard_name
        integer(kind=i_sp), pointer, intent(out)   :: ptr(:,:)
        integer,                     intent(  out) :: ierr

        integer                                    :: idx

        ierr = 0
        ! Lookup the standard name in the index
        idx = ccpp_field_find(ap_data, standard_name, ierr)
        if (ierr /= 0) then
            call ccpp_warn('Unable to find the requested field')
            return
        end if

        call c_f_pointer(ap_data%fields(idx)%ptr, ptr, ap_data%fields(idx)%dims)

    end subroutine ccpp_field_data_i2

    subroutine ccpp_field_data_i3(ap_data, standard_name, ptr, ierr)
        type(aip_t),                 intent(in)    :: ap_data
        character(len=*),            intent(in)    :: standard_name
        integer(kind=i_sp), pointer, intent(out)   :: ptr(:,:,:)
        integer,                     intent(  out) :: ierr

        integer                                    :: idx

        ierr = 0
        ! Lookup the standard name in the index
        idx = ccpp_field_find(ap_data, standard_name, ierr)
        if (ierr /= 0) then
            call ccpp_warn('Unable to find the requested field')
            return
        end if

        call c_f_pointer(ap_data%fields(idx)%ptr, ptr, ap_data%fields(idx)%dims)

    end subroutine ccpp_field_data_i3

    subroutine ccpp_field_data_r0(ap_data, standard_name, ptr, ierr)
        type(aip_t),                 intent(in)    :: ap_data
        character(len=*),            intent(in)    :: standard_name
        real(kind=r_dp), pointer,    intent(out)   :: ptr
        integer,                     intent(  out) :: ierr

        integer                                    :: idx

        ierr = 0
        ! Lookup the standard name in the index
        idx = ccpp_field_find(ap_data, standard_name, ierr)
        if (ierr /= 0) then
            call ccpp_warn('Unable to find the requested field')
            return
        end if

        call c_f_pointer(ap_data%fields(idx)%ptr, ptr)

    end subroutine ccpp_field_data_r0

    subroutine ccpp_field_data_r1(ap_data, standard_name, ptr, ierr)
        type(aip_t),                 intent(in)    :: ap_data
        character(len=*),            intent(in)    :: standard_name
        real(kind=r_dp), pointer,    intent(out)   :: ptr(:)
        integer,                     intent(  out) :: ierr

        integer                                    :: idx

        ierr = 0
        ! Lookup the standard name in the index
        idx = ccpp_field_find(ap_data, standard_name, ierr)
        if (ierr /= 0) then
            call ccpp_warn('Unable to find the requested field')
            return
        end if

        call c_f_pointer(ap_data%fields(idx)%ptr, ptr, ap_data%fields(idx)%dims)

    end subroutine ccpp_field_data_r1

    subroutine ccpp_field_data_r2(ap_data, standard_name, ptr, ierr)
        type(aip_t),                 intent(in)    :: ap_data
        character(len=*),            intent(in)    :: standard_name
        real(kind=r_dp), pointer,    intent(out)   :: ptr(:,:)
        integer,                     intent(  out) :: ierr

        integer                                    :: idx

        ierr = 0
        ! Lookup the standard name in the index
        idx = ccpp_field_find(ap_data, standard_name, ierr)
        if (ierr /= 0) then
            call ccpp_warn('Unable to find the requested field')
            return
        end if

        call c_f_pointer(ap_data%fields(idx)%ptr, ptr, ap_data%fields(idx)%dims)

    end subroutine ccpp_field_data_r2

    subroutine ccpp_field_data_r3(ap_data, standard_name, ptr, ierr)
        type(aip_t),                 intent(in)    :: ap_data
        character(len=*),            intent(in)    :: standard_name
        real(kind=r_dp), pointer,    intent(out)   :: ptr(:,:,:)
        integer,                     intent(  out) :: ierr

        integer                                    :: idx

        ierr = 0
        ! Lookup the standard name in the index
        idx = ccpp_field_find(ap_data, standard_name, ierr)
        if (ierr /= 0) then
            call ccpp_warn('Unable to find the requested field')
            return
        end if

        call c_f_pointer(ap_data%fields(idx)%ptr, ptr, ap_data%fields(idx)%dims)

    end subroutine ccpp_field_data_r3


end module ccpp_fields
