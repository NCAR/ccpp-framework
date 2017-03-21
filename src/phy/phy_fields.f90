!>
!! @brief Physics fields module.
!!
!! @details Routines and functions to interact with physics fields.
!!          Most of the work is carried out in C (phy_field_idx.c).
!
module phy_fields

    use, intrinsic :: iso_c_binding
    use            :: kinds,                                            &
                      only: i_sp, r_dp
    use            :: types,                                            &
                      only: STR_LEN, suite_t, ipd_t, subcycle_t,        &
                            aip_t, field_t
    use            :: strings,                                          &
                      only: fstr, cstr
    use            :: errors,                                           &
                      only: err, warn

    implicit none

    private
    public :: phy_field_init,                                            &
              phy_field_add,                                             &
              phy_field_find,                                            &
              phy_field_sort,                                            &
              phy_field_fini,                                            &
              phy_field_data

    interface phy_field_data
       module procedure :: phy_field_data_i0
       module procedure :: phy_field_data_i1
       module procedure :: phy_field_data_i2
       module procedure :: phy_field_data_i3
       module procedure :: phy_field_data_r0
       module procedure :: phy_field_data_r1
       module procedure :: phy_field_data_r2
       module procedure :: phy_field_data_r3
    end interface phy_field_data

    interface

       integer(c_int32_t)                                              &
       function phy_field_idx_init                                     &
                (total, idx)                                           &
                bind(c, name='phy_field_idx_init')
        import :: c_int32_t, c_ptr
        integer(c_int32_t)                   :: total
        type(c_ptr)                          :: idx
       end function phy_field_idx_init

       integer(c_int32_t)                                              &
       function phy_field_idx_fini                                     &
                (idx)                                                  &
                bind(c, name='phy_field_idx_fini')
        import :: c_int32_t, c_ptr
        type(c_ptr)                          :: idx
       end function phy_field_idx_fini

       integer(c_int32_t)                                              &
       function phy_field_idx_add                                      &
                (name, i, idx)                                         &
                bind(c, name='phy_field_idx_add')
        import :: c_char, c_int32_t, c_ptr
        character(kind=c_char), dimension(*) :: name
        integer(c_int32_t)                   :: i
        type(c_ptr)                          :: idx
       end function phy_field_idx_add

       integer(c_int32_t)                                              &
       function phy_field_idx_find                                     &
                (name, total, idx)                                     &
                bind(c, name='phy_field_idx_find')
        import :: c_char, c_int32_t, c_ptr
        character(kind=c_char), dimension(*) :: name
        integer(c_int32_t)                   :: total
        type(c_ptr)                          :: idx
       end function phy_field_idx_find

       integer(c_int32_t)                                              &
       function phy_field_idx_sort                                     &
                (total, idx)                                           &
                bind(c, name='phy_field_idx_sort')
        import :: c_char, c_int32_t, c_ptr
        integer(c_int32_t)                   :: total
        type(c_ptr)                          :: idx
       end function phy_field_idx_sort

   end interface

   contains

    subroutine phy_field_init(ap_data, total)
        type(aip_t),            intent(inout) :: ap_data
        integer,                intent(in)    :: total

        integer                               :: ierr

        ierr = 0

        ap_data%fields_max = total
        allocate(ap_data%fields(ap_data%fields_max), stat=ierr)
        if (ierr /= 0) then
            call err(ierr, 'Unable to allocate ap_data fields')
        end if

        ierr = phy_field_idx_init(ap_data%fields_max, ap_data%fields_idx)
        if (ierr /= 0) then
            call err(ierr, 'Unable to initalize ap_data field index')
        end if

        ! set the number of fields we already have
        ap_data%fields_n = 0

    end subroutine phy_field_init

    subroutine phy_field_add(ap_data, standard_name, units, ptr, rank, dims)
        type(aip_t),                     intent(inout) :: ap_data
        character(len=*),                intent(in)    :: standard_name
        character(len=*),                intent(in)    :: units
        type(c_ptr),                     intent(in)    :: ptr
        integer,               optional, intent(in)    :: rank
        integer, dimension(:), optional, intent(in)    :: dims

        integer                               :: ierr
        integer                               :: i

        ierr = 0

        if (ap_data%fields_n .eq. ap_data%fields_max) then
            ierr = 1
            call err(ierr, 'Unable to add more fields')
        end if

        ap_data%fields_n = ap_data%fields_n + 1
        i = ap_data%fields_n

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
                call err(ierr, 'Unable to allocate ap_data fields dims')
            end if
            ap_data%fields(i)%dims      = dims
        else
            ap_data%fields(i)%dims      = 0
        end if

        ierr = phy_field_idx_add(cstr(standard_name), i, ap_data%fields_idx)
        if (ierr /= 0) then
            call err(ierr, 'Unable to add field index: '//trim(standard_name))
        end if

    end subroutine phy_field_add

    subroutine phy_field_sort(ap_data)
        type(aip_t),            intent(inout) :: ap_data

        integer                               :: ierr

        ierr = 0

        ierr = phy_field_idx_sort(ap_data%fields_n, ap_data%fields_idx)
        if (ierr /= 0) then
            call err(ierr, 'Unable to sort field indices')
        end if

    end subroutine phy_field_sort

    function phy_field_find(ap_data, standard_name) result(location)
        type(aip_t),            intent(in)    :: ap_data
        character(len=*),       intent(in)    :: standard_name

        integer                               :: location

        location = phy_field_idx_find(cstr(standard_name), &
                                      ap_data%fields_n,    &
                                      ap_data%fields_idx)

    end function phy_field_find

    subroutine phy_field_fini(ap_data)
        type(aip_t),            intent(inout) :: ap_data

        integer                               :: ierr

        ierr = 0

        if (allocated(ap_data%fields)) then
            deallocate(ap_data%fields)
        end if

        ierr = phy_field_idx_fini(ap_data%fields_idx)

        ! set the number of fields we already have
        ap_data%fields_n = 0
        ap_data%fields_max = 0

    end subroutine phy_field_fini

    subroutine phy_field_data_i0(ap_data, standard_name, ptr)
        type(aip_t),                 intent(in)  :: ap_data
        character(len=*),            intent(in)  :: standard_name
        integer(kind=i_sp), pointer, intent(out) :: ptr

        integer                                  :: idx

        idx = 0
        ! Lookup the standard name in the index
        idx = phy_field_find(ap_data, standard_name)

        call c_f_pointer(ap_data%fields(idx)%ptr, ptr)

    end subroutine phy_field_data_i0

    subroutine phy_field_data_i1(ap_data, standard_name, ptr)
        type(aip_t),                 intent(in)  :: ap_data
        character(len=*),            intent(in)  :: standard_name
        integer(kind=i_sp), pointer, intent(out) :: ptr(:)

        integer                                  :: idx

        idx = 0
        ! Lookup the standard name in the index
        idx = phy_field_find(ap_data, standard_name)

        call c_f_pointer(ap_data%fields(idx)%ptr, ptr, ap_data%fields(idx)%dims)

    end subroutine phy_field_data_i1

    subroutine phy_field_data_i2(ap_data, standard_name, ptr)
        type(aip_t),                 intent(in)  :: ap_data
        character(len=*),            intent(in)  :: standard_name
        integer(kind=i_sp), pointer, intent(out) :: ptr(:,:)

        integer                                  :: idx

        idx = 0
        ! Lookup the standard name in the index
        idx = phy_field_find(ap_data, standard_name)

        call c_f_pointer(ap_data%fields(idx)%ptr, ptr, ap_data%fields(idx)%dims)

    end subroutine phy_field_data_i2

    subroutine phy_field_data_i3(ap_data, standard_name, ptr)
        type(aip_t),                 intent(in)  :: ap_data
        character(len=*),            intent(in)  :: standard_name
        integer(kind=i_sp), pointer, intent(out) :: ptr(:,:,:)

        integer                                  :: idx

        idx = 0
        ! Lookup the standard name in the index
        idx = phy_field_find(ap_data, standard_name)

        call c_f_pointer(ap_data%fields(idx)%ptr, ptr, ap_data%fields(idx)%dims)

    end subroutine phy_field_data_i3

    subroutine phy_field_data_r0(ap_data, standard_name, ptr)
        type(aip_t),                 intent(in)  :: ap_data
        character(len=*),            intent(in)  :: standard_name
        real(kind=r_dp), pointer,    intent(out) :: ptr

        integer                                  :: idx

        idx = 0
        ! Lookup the standard name in the index
        idx = phy_field_find(ap_data, standard_name)

        call c_f_pointer(ap_data%fields(idx)%ptr, ptr)

    end subroutine phy_field_data_r0

    subroutine phy_field_data_r1(ap_data, standard_name, ptr)
        type(aip_t),                 intent(in)  :: ap_data
        character(len=*),            intent(in)  :: standard_name
        real(kind=r_dp), pointer, intent(out) :: ptr(:)

        integer                                  :: idx

        idx = 0
        ! Lookup the standard name in the index
        idx = phy_field_find(ap_data, standard_name)

        call c_f_pointer(ap_data%fields(idx)%ptr, ptr, ap_data%fields(idx)%dims)

    end subroutine phy_field_data_r1

    subroutine phy_field_data_r2(ap_data, standard_name, ptr)
        type(aip_t),                 intent(in)  :: ap_data
        character(len=*),            intent(in)  :: standard_name
        real(kind=r_dp), pointer, intent(out) :: ptr(:,:)

        integer                                  :: idx

        idx = 0
        ! Lookup the standard name in the index
        idx = phy_field_find(ap_data, standard_name)

        call c_f_pointer(ap_data%fields(idx)%ptr, ptr, ap_data%fields(idx)%dims)

    end subroutine phy_field_data_r2

    subroutine phy_field_data_r3(ap_data, standard_name, ptr)
        type(aip_t),                 intent(in)  :: ap_data
        character(len=*),            intent(in)  :: standard_name
        real(kind=r_dp), pointer, intent(out) :: ptr(:,:,:)

        integer                                  :: idx

        idx = 0
        ! Lookup the standard name in the index
        idx = phy_field_find(ap_data, standard_name)

        call c_f_pointer(ap_data%fields(idx)%ptr, ptr, ap_data%fields(idx)%dims)

    end subroutine phy_field_data_r3


end module phy_fields
