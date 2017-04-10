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
!! ! Add a field, for example the eastward_wind.
!! call ccpp_fields_add(ipd_data, 'eastward_wind', 'm s-1',  &
!!                      u, ierr)
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
!! call ccpp_fields_get(ipd_data, 'eastward_wind', u, ierr)
!! if (ierr /= 0) then
!!   call exit(ierr)
!! end if
!!
!! @endcode
!
module ccpp_fields

    use, intrinsic :: iso_fortran_env,                                  &
                      only: INT8, INT16, INT32, INT64,                  &
                            REAL32, REAL64, REAL128
    use, intrinsic :: iso_c_binding,                                    &
                      only: c_f_pointer, c_loc, c_ptr, c_int32_t, c_char
    use            :: ccpp_types,                                       &
                      only: ccpp_t, ccpp_field_t
    use            :: ccpp_strings,                                     &
                      only: ccpp_cstr
    use            :: ccpp_errors,                                      &
                      only: ccpp_warn

    implicit none

    private
    public :: ccpp_fields_init,                                        &
              ccpp_fields_fini,                                        &
              ccpp_fields_find,                                        &
              ccpp_fields_add,                                         &
              ccpp_fields_get

    !>
    !! Module precedence for adding a field.
    !
    interface ccpp_fields_add
       module procedure :: ccpp_fields_add_i32_0
       module procedure :: ccpp_fields_add_i32_1
       module procedure :: ccpp_fields_add_i32_2
       module procedure :: ccpp_fields_add_i32_3

       module procedure :: ccpp_fields_add_i64_0
       module procedure :: ccpp_fields_add_i64_1
       module procedure :: ccpp_fields_add_i64_2
       module procedure :: ccpp_fields_add_i64_3

       module procedure :: ccpp_fields_add_r32_0
       module procedure :: ccpp_fields_add_r32_1
       module procedure :: ccpp_fields_add_r32_2
       module procedure :: ccpp_fields_add_r32_3

       module procedure :: ccpp_fields_add_r64_0
       module procedure :: ccpp_fields_add_r64_1
       module procedure :: ccpp_fields_add_r64_2
       module procedure :: ccpp_fields_add_r64_3

       module procedure :: ccpp_fields_add_l_0
       module procedure :: ccpp_fields_add_l_1
       module procedure :: ccpp_fields_add_l_2
       module procedure :: ccpp_fields_add_l_3
    end interface ccpp_fields_add

    !>
    !! Module precedence for getting a field.
    !
    interface ccpp_fields_get
       module procedure :: ccpp_fields_get_i32_0
       module procedure :: ccpp_fields_get_i32_1
       module procedure :: ccpp_fields_get_i32_2
       module procedure :: ccpp_fields_get_i32_3

       module procedure :: ccpp_fields_get_i64_0
       module procedure :: ccpp_fields_get_i64_1
       module procedure :: ccpp_fields_get_i64_2
       module procedure :: ccpp_fields_get_i64_3

       module procedure :: ccpp_fields_get_r32_0
       module procedure :: ccpp_fields_get_r32_1
       module procedure :: ccpp_fields_get_r32_2
       module procedure :: ccpp_fields_get_r32_3

       module procedure :: ccpp_fields_get_r64_0
       module procedure :: ccpp_fields_get_r64_1
       module procedure :: ccpp_fields_get_r64_2
       module procedure :: ccpp_fields_get_r64_3

       module procedure :: ccpp_fields_get_l_0
       module procedure :: ccpp_fields_get_l_1
       module procedure :: ccpp_fields_get_l_2
       module procedure :: ccpp_fields_get_l_3
    end interface ccpp_fields_get

    !>
    !! Interface to all the C field index funtions.
    !
    interface
       integer(c_int32_t)                                              &
       function ccpp_field_idx_init                                    &
                (idx)                                                  &
                bind(c, name='ccpp_field_idx_init')
        import :: c_int32_t, c_ptr
        type(c_ptr)                          :: idx
       end function ccpp_field_idx_init

       integer(c_int32_t)                                              &
       function ccpp_field_idx_fini                                    &
                (idx)                                                  &
                bind(c, name='ccpp_field_idx_fini')
        import :: c_int32_t, c_ptr
        type(c_ptr)                          :: idx
       end function ccpp_field_idx_fini

       integer(c_int32_t)                                              &
       function ccpp_field_idx_add                                     &
                (name, idx)                                            &
                bind(c, name='ccpp_field_idx_add')
        import :: c_int32_t, c_char, c_ptr
        character(kind=c_char), dimension(*) :: name
        type(c_ptr)                          :: idx
       end function ccpp_field_idx_add

       integer(c_int32_t)                                              &
       function ccpp_field_idx_find                                    &
                (name, idx)                                            &
                bind(c, name='ccpp_field_idx_find')
        import :: c_char, c_int32_t, c_ptr
        character(kind=c_char), dimension(*) :: name
        type(c_ptr)                          :: idx
       end function ccpp_field_idx_find

       integer(c_int32_t)                                              &
       function ccpp_field_idx_max                                     &
                (idx)                                                  &
                bind(c, name='ccpp_field_idx_max')
        import :: c_int32_t, c_ptr
        type(c_ptr)                          :: idx
       end function ccpp_field_idx_max

    end interface

    contains

    !>
    !! CCPP fields initialization subroutine.
    !!
    !! @param[inout] cdata    The ccpp_t type data.
    !! @param[  out] ierr     Integer error flag.
    !
    subroutine ccpp_fields_init(cdata, ierr)
        type(ccpp_t),           intent(inout) :: cdata
        integer,                intent(  out) :: ierr

        integer                               :: fields_max

        ierr = 0

        ierr = ccpp_field_idx_init(cdata%fields_idx)
        if (ierr /= 0) then
            call ccpp_warn('Unable to initalize cdata field index')
            return
        end if

        fields_max = ccpp_field_idx_max(cdata%fields_idx)

        allocate(cdata%fields(fields_max), stat=ierr)
        if (ierr /= 0) then
            call ccpp_warn('Unable to allocate cdata fields')
            return
        end if

    end subroutine ccpp_fields_init

    !>
    !! CCPP fields finalization subroutine.
    !!
    !! @param[inout] cdata    The ccpp_t type data.
    !! @param[  out] ierr     Integer error flag.
    !
    subroutine ccpp_fields_fini(cdata, ierr)
        type(ccpp_t),           intent(inout) :: cdata
        integer,                intent(  out) :: ierr

        ierr = 0

        if (allocated(cdata%fields)) then
            deallocate(cdata%fields)
        end if

        ierr = ccpp_field_idx_fini(cdata%fields_idx)

    end subroutine ccpp_fields_fini

    !>
    !! CCPP fields addition subroutine.
    !!
    !! @param[inout] cdata         The ccpp_t type data.
    !! @param[in   ] standard_name The standard name for the data.
    !! @param[in   ] units         The SI units for the data.
    !! @param[in   ] ptr           A C pointer to the data.
    !! @param[in   ] p_rank        Optional rank of the data.
    !! @param[in   ] p_dims        Optional dimensions of the data.
    !! @param[  out] ierr          Integer error flag.
    !
    subroutine ccpp_field_add_ptr(cdata, standard_name, units, ptr, &
                                  p_rank, p_dims, ierr)
        type(ccpp_t),                    intent(inout) :: cdata
        character(len=*),                intent(in)    :: standard_name
        character(len=*),                intent(in)    :: units
        type(c_ptr),                     intent(in)    :: ptr
        integer,               optional, intent(in)    :: p_rank
        integer, dimension(:), optional, intent(in)    :: p_dims
        integer,                         intent(  out) :: ierr

        integer                                        :: i
        integer                                        :: old_fields_max
        integer                                        :: new_fields_max
        type(ccpp_field_t), allocatable, dimension(:)  :: tmp

        ierr = 0

        ! Get the current/old fields max
        old_fields_max = ccpp_field_idx_max(cdata%fields_idx)

        ! Add ourselves to the index and get our array position
        i = ccpp_field_idx_add(ccpp_cstr(standard_name), cdata%fields_idx)
        if (i .lt. 1) then
            call ccpp_warn('Unable to add field index: '//trim(standard_name))
            return
        end if

        ! Get the new fields max
        new_fields_max = ccpp_field_idx_max(cdata%fields_idx)

        if (old_fields_max .lt. new_fields_max) then
            allocate(tmp(new_fields_max), stat=ierr)
            if (ierr /= 0) then
                call ccpp_warn('Unable to grow cdata fields array')
                return
            end if
            tmp(1:size(cdata%fields)) = cdata%fields
            call move_alloc(tmp, cdata%fields)
        end if

        cdata%fields(i)%standard_name = standard_name
        cdata%fields(i)%units         = units
        cdata%fields(i)%ptr           = ptr

        if (present(p_rank)) then
            cdata%fields(i)%rank      = p_rank
        else
            cdata%fields(i)%rank      = 0
        end if

        if (present(p_dims)) then
            allocate(cdata%fields(i)%dims(p_rank), stat=ierr)
            if (ierr /= 0) then
                call ccpp_warn('Unable to allocate cdata fields dims')
                return
            end if
            cdata%fields(i)%dims      = p_dims
        end if

    end subroutine ccpp_field_add_ptr

    !>
    !! CCPP find a fields location/array index by standard name.
    !!
    !! @param[inout] cdata         The ccpp_t type data.
    !! @param[in   ] standard_name The standard name for the data.
    !! @param[  out] ierr          Integer error flag.
    !
    function ccpp_fields_find(cdata, standard_name, ierr) result(location)
        type(ccpp_t),           intent(in)    :: cdata
        character(len=*),       intent(in)    :: standard_name
        integer,                intent(  out) :: ierr

        integer                               :: location

        location = ccpp_field_idx_find(ccpp_cstr(standard_name), &
                                      cdata%fields_idx)
        if (location <= 0) then
            ierr = 1
        end if

    end function ccpp_fields_find

    ! TODO: Subroutine to iterate over all fields.


    !------------------------------------------------------------------!
    !>
    !! Single precision (32-bit) integer field addition subroutines.
    !
    !------------------------------------------------------------------!
    subroutine ccpp_fields_add_i32_0(cdata, standard_name, units, ptr, ierr)
        type(ccpp_t),                intent(inout) :: cdata
        character(len=*),            intent(in)    :: standard_name
        character(len=*),            intent(in)    :: units
        integer(kind=INT32),  intent(in)    :: ptr
        integer,                     intent(  out) :: ierr

        ierr = 0
        call ccpp_field_add_ptr(cdata, standard_name, units, &
                               c_loc(ptr), ierr=ierr)

    end subroutine ccpp_fields_add_i32_0

    subroutine ccpp_fields_add_i32_1(cdata, standard_name, units, ptr, ierr)
        type(ccpp_t),                intent(inout) :: cdata
        character(len=*),            intent(in)    :: standard_name
        character(len=*),            intent(in)    :: units
        integer(kind=INT32),  intent(in)    :: ptr(:)
        integer,                     intent(  out) :: ierr

        ierr = 0
        call ccpp_field_add_ptr(cdata, standard_name, units, &
                               c_loc(ptr), size(ptr), shape(ptr), ierr=ierr)

    end subroutine ccpp_fields_add_i32_1

    subroutine ccpp_fields_add_i32_2(cdata, standard_name, units, ptr, ierr)
        type(ccpp_t),                intent(inout) :: cdata
        character(len=*),            intent(in)    :: standard_name
        character(len=*),            intent(in)    :: units
        integer(kind=INT32),  intent(in)    :: ptr(:,:)
        integer,                     intent(  out) :: ierr

        ierr = 0
        call ccpp_field_add_ptr(cdata, standard_name, units, &
                               c_loc(ptr), size(ptr), shape(ptr), ierr=ierr)

    end subroutine ccpp_fields_add_i32_2

    subroutine ccpp_fields_add_i32_3(cdata, standard_name, units, ptr, ierr)
        type(ccpp_t),                intent(inout) :: cdata
        character(len=*),            intent(in)    :: standard_name
        character(len=*),            intent(in)    :: units
        integer(kind=INT32),  intent(in)    :: ptr(:,:,:)
        integer,                     intent(  out) :: ierr

        ierr = 0
        call ccpp_field_add_ptr(cdata, standard_name, units, &
                               c_loc(ptr), size(ptr), shape(ptr), ierr=ierr)

    end subroutine ccpp_fields_add_i32_3

    !------------------------------------------------------------------!
    !>
    !! Double precision (64-bit) integer field addition subroutines.
    !
    !------------------------------------------------------------------!
    subroutine ccpp_fields_add_i64_0(cdata, standard_name, units, ptr, ierr)
        type(ccpp_t),                intent(inout) :: cdata
        character(len=*),            intent(in)    :: standard_name
        character(len=*),            intent(in)    :: units
        integer(kind=INT64),  intent(in)    :: ptr
        integer,                     intent(  out) :: ierr

        ierr = 0
        call ccpp_field_add_ptr(cdata, standard_name, units, &
                               c_loc(ptr), ierr=ierr)

    end subroutine ccpp_fields_add_i64_0

    subroutine ccpp_fields_add_i64_1(cdata, standard_name, units, ptr, ierr)
        type(ccpp_t),                intent(inout) :: cdata
        character(len=*),            intent(in)    :: standard_name
        character(len=*),            intent(in)    :: units
        integer(kind=INT64),  intent(in)    :: ptr(:)
        integer,                     intent(  out) :: ierr

        ierr = 0
        call ccpp_field_add_ptr(cdata, standard_name, units, &
                               c_loc(ptr), size(ptr), shape(ptr), ierr=ierr)

    end subroutine ccpp_fields_add_i64_1

    subroutine ccpp_fields_add_i64_2(cdata, standard_name, units, ptr, ierr)
        type(ccpp_t),                intent(inout) :: cdata
        character(len=*),            intent(in)    :: standard_name
        character(len=*),            intent(in)    :: units
        integer(kind=INT64),  intent(in)    :: ptr(:,:)
        integer,                     intent(  out) :: ierr

        ierr = 0
        call ccpp_field_add_ptr(cdata, standard_name, units, &
                               c_loc(ptr), size(ptr), shape(ptr), ierr=ierr)

    end subroutine ccpp_fields_add_i64_2

    subroutine ccpp_fields_add_i64_3(cdata, standard_name, units, ptr, ierr)
        type(ccpp_t),                intent(inout) :: cdata
        character(len=*),            intent(in)    :: standard_name
        character(len=*),            intent(in)    :: units
        integer(kind=INT64),  intent(in)    :: ptr(:,:,:)
        integer,                     intent(  out) :: ierr

        ierr = 0
        call ccpp_field_add_ptr(cdata, standard_name, units, &
                               c_loc(ptr), size(ptr), shape(ptr), ierr=ierr)

    end subroutine ccpp_fields_add_i64_3

    !------------------------------------------------------------------!
    !>
    !! Single precision (32-bit) real field addition subroutines.
    !
    !------------------------------------------------------------------!
    subroutine ccpp_fields_add_r32_0(cdata, standard_name, units, ptr, ierr)
        type(ccpp_t),                intent(inout) :: cdata
        character(len=*),            intent(in)    :: standard_name
        character(len=*),            intent(in)    :: units
        real(kind=REAL32),           intent(in)    :: ptr
        integer,                     intent(  out) :: ierr

        ierr = 0
        call ccpp_field_add_ptr(cdata, standard_name, units, &
                               c_loc(ptr), ierr=ierr)

    end subroutine ccpp_fields_add_r32_0

    subroutine ccpp_fields_add_r32_1(cdata, standard_name, units, ptr, ierr)
        type(ccpp_t),                intent(inout) :: cdata
        character(len=*),            intent(in)    :: standard_name
        character(len=*),            intent(in)    :: units
        real(kind=REAL32),           intent(in)    :: ptr(:)
        integer,                     intent(  out) :: ierr

        ierr = 0
        call ccpp_field_add_ptr(cdata, standard_name, units, &
                               c_loc(ptr), size(ptr), shape(ptr), ierr=ierr)

    end subroutine ccpp_fields_add_r32_1

    subroutine ccpp_fields_add_r32_2(cdata, standard_name, units, ptr, ierr)
        type(ccpp_t),                intent(inout) :: cdata
        character(len=*),            intent(in)    :: standard_name
        character(len=*),            intent(in)    :: units
        real(kind=REAL32),           intent(in)    :: ptr(:,:)
        integer,                     intent(  out) :: ierr

        ierr = 0
        call ccpp_field_add_ptr(cdata, standard_name, units, &
                               c_loc(ptr), size(ptr), shape(ptr), ierr=ierr)

    end subroutine ccpp_fields_add_r32_2

    subroutine ccpp_fields_add_r32_3(cdata, standard_name, units, ptr, ierr)
        type(ccpp_t),                intent(inout) :: cdata
        character(len=*),            intent(in)    :: standard_name
        character(len=*),            intent(in)    :: units
        real(kind=REAL32),           intent(in)    :: ptr(:,:,:)
        integer,                     intent(  out) :: ierr

        ierr = 0
        call ccpp_field_add_ptr(cdata, standard_name, units, &
                               c_loc(ptr), size(ptr), shape(ptr), ierr=ierr)

    end subroutine ccpp_fields_add_r32_3

    !------------------------------------------------------------------!
    !>
    !! Double precision (64-bit) real field addition subroutines.
    !
    !------------------------------------------------------------------!
    subroutine ccpp_fields_add_r64_0(cdata, standard_name, units, ptr, ierr)
        type(ccpp_t),                intent(inout) :: cdata
        character(len=*),            intent(in)    :: standard_name
        character(len=*),            intent(in)    :: units
        real(kind=REAL64),           intent(in)    :: ptr
        integer,                     intent(  out) :: ierr

        ierr = 0
        call ccpp_field_add_ptr(cdata, standard_name, units, &
                               c_loc(ptr), ierr=ierr)

    end subroutine ccpp_fields_add_r64_0

    subroutine ccpp_fields_add_r64_1(cdata, standard_name, units, ptr, ierr)
        type(ccpp_t),                intent(inout) :: cdata
        character(len=*),            intent(in)    :: standard_name
        character(len=*),            intent(in)    :: units
        real(kind=REAL64),           intent(in)    :: ptr(:)
        integer,                     intent(  out) :: ierr

        ierr = 0
        call ccpp_field_add_ptr(cdata, standard_name, units, &
                               c_loc(ptr), size(ptr), shape(ptr), ierr=ierr)

    end subroutine ccpp_fields_add_r64_1

    subroutine ccpp_fields_add_r64_2(cdata, standard_name, units, ptr, ierr)
        type(ccpp_t),                intent(inout) :: cdata
        character(len=*),            intent(in)    :: standard_name
        character(len=*),            intent(in)    :: units
        real(kind=REAL64),           intent(in)    :: ptr(:,:)
        integer,                     intent(  out) :: ierr

        ierr = 0
        call ccpp_field_add_ptr(cdata, standard_name, units, &
                               c_loc(ptr), size(ptr), shape(ptr), ierr=ierr)

    end subroutine ccpp_fields_add_r64_2

    subroutine ccpp_fields_add_r64_3(cdata, standard_name, units, ptr, ierr)
        type(ccpp_t),                intent(inout) :: cdata
        character(len=*),            intent(in)    :: standard_name
        character(len=*),            intent(in)    :: units
        real(kind=REAL64),           intent(in)    :: ptr(:,:,:)
        integer,                     intent(  out) :: ierr

        ierr = 0
        call ccpp_field_add_ptr(cdata, standard_name, units, &
                               c_loc(ptr), size(ptr), shape(ptr), ierr=ierr)

    end subroutine ccpp_fields_add_r64_3

    !------------------------------------------------------------------!
    !>
    !! Logical field addition subroutines.
    !
    !------------------------------------------------------------------!
    subroutine ccpp_fields_add_l_0(cdata, standard_name, units, ptr, ierr)
        type(ccpp_t),                intent(inout) :: cdata
        character(len=*),            intent(in)    :: standard_name
        character(len=*),            intent(in)    :: units
        logical,                     intent(in)    :: ptr
        integer,                     intent(  out) :: ierr

        ierr = 0
        call ccpp_field_add_ptr(cdata, standard_name, units, &
                               c_loc(ptr), ierr=ierr)

    end subroutine ccpp_fields_add_l_0

    subroutine ccpp_fields_add_l_1(cdata, standard_name, units, ptr, ierr)
        type(ccpp_t),                intent(inout) :: cdata
        character(len=*),            intent(in)    :: standard_name
        character(len=*),            intent(in)    :: units
        logical,                     intent(in)    :: ptr(:)
        integer,                     intent(  out) :: ierr

        ierr = 0
        call ccpp_field_add_ptr(cdata, standard_name, units, &
                               c_loc(ptr), size(ptr), shape(ptr), ierr=ierr)

    end subroutine ccpp_fields_add_l_1

    subroutine ccpp_fields_add_l_2(cdata, standard_name, units, ptr, ierr)
        type(ccpp_t),                intent(inout) :: cdata
        character(len=*),            intent(in)    :: standard_name
        character(len=*),            intent(in)    :: units
        logical,                     intent(in)    :: ptr(:,:)
        integer,                     intent(  out) :: ierr

        ierr = 0
        call ccpp_field_add_ptr(cdata, standard_name, units, &
                               c_loc(ptr), size(ptr), shape(ptr), ierr=ierr)

    end subroutine ccpp_fields_add_l_2

    subroutine ccpp_fields_add_l_3(cdata, standard_name, units, ptr, ierr)
        type(ccpp_t),                intent(inout) :: cdata
        character(len=*),            intent(in)    :: standard_name
        character(len=*),            intent(in)    :: units
        logical,                     intent(in)    :: ptr(:,:,:)
        integer,                     intent(  out) :: ierr

        ierr = 0
        call ccpp_field_add_ptr(cdata, standard_name, units, &
                               c_loc(ptr), size(ptr), shape(ptr), ierr=ierr)

    end subroutine ccpp_fields_add_l_3

    !------------------------------------------------------------------!
    !>
    !! Single precision (32-bit) integer field retrieval subroutines.
    !
    !------------------------------------------------------------------!
    subroutine ccpp_fields_get_i32_0(cdata, standard_name, ptr, ierr, units)
        type(ccpp_t),                 intent(in)    :: cdata
        character(len=*),             intent(in)    :: standard_name
        integer(kind=INT32), pointer, intent(  out) :: ptr
        integer,                      intent(  out) :: ierr
        character(len=*), optional,   intent(  out) :: units

        integer                                     :: idx

        ierr = 0
        ! Lookup the standard name in the index
        idx = ccpp_fields_find(cdata, standard_name, ierr)
        if (ierr /= 0) then
            call ccpp_warn('Unable to find the requested field')
            return
        end if

        call c_f_pointer(cdata%fields(idx)%ptr, ptr)

        if (present(units)) then
            units = cdata%fields(idx)%units
        end if

    end subroutine ccpp_fields_get_i32_0

    subroutine ccpp_fields_get_i32_1(cdata, standard_name, ptr, ierr, units)
        type(ccpp_t),                 intent(in)    :: cdata
        character(len=*),             intent(in)    :: standard_name
        integer(kind=INT32), pointer, intent(  out) :: ptr(:)
        integer,                      intent(  out) :: ierr
        character(len=*), optional,   intent(  out) :: units

        integer                                     :: idx

        ierr = 0
        ! Lookup the standard name in the index
        idx = ccpp_fields_find(cdata, standard_name, ierr)
        if (ierr /= 0) then
            call ccpp_warn('Unable to find the requested field')
            return
        end if

        call c_f_pointer(cdata%fields(idx)%ptr, ptr, cdata%fields(idx)%dims)

        if (present(units)) then
            units = cdata%fields(idx)%units
        end if

    end subroutine ccpp_fields_get_i32_1

    subroutine ccpp_fields_get_i32_2(cdata, standard_name, ptr, ierr, units)
        type(ccpp_t),                 intent(in)    :: cdata
        character(len=*),             intent(in)    :: standard_name
        integer(kind=INT32), pointer, intent(  out) :: ptr(:,:)
        integer,                      intent(  out) :: ierr
        character(len=*), optional,   intent(  out) :: units

        integer                                     :: idx

        ierr = 0
        ! Lookup the standard name in the index
        idx = ccpp_fields_find(cdata, standard_name, ierr)
        if (ierr /= 0) then
            call ccpp_warn('Unable to find the requested field')
            return
        end if

        call c_f_pointer(cdata%fields(idx)%ptr, ptr, cdata%fields(idx)%dims)

        if (present(units)) then
            units = cdata%fields(idx)%units
        end if

    end subroutine ccpp_fields_get_i32_2

    subroutine ccpp_fields_get_i32_3(cdata, standard_name, ptr, ierr, units)
        type(ccpp_t),                 intent(in)    :: cdata
        character(len=*),             intent(in)    :: standard_name
        integer(kind=INT32), pointer, intent(  out) :: ptr(:,:,:)
        integer,                      intent(  out) :: ierr
        character(len=*), optional,   intent(  out) :: units

        integer                                     :: idx

        ierr = 0
        ! Lookup the standard name in the index
        idx = ccpp_fields_find(cdata, standard_name, ierr)
        if (ierr /= 0) then
            call ccpp_warn('Unable to find the requested field')
            return
        end if

        call c_f_pointer(cdata%fields(idx)%ptr, ptr, cdata%fields(idx)%dims)

        if (present(units)) then
            units = cdata%fields(idx)%units
        end if

    end subroutine ccpp_fields_get_i32_3

    !------------------------------------------------------------------!
    !>
    !! Double precision (64-bit) integer field retrieval subroutines.
    !
    !------------------------------------------------------------------!
    subroutine ccpp_fields_get_i64_0(cdata, standard_name, ptr, ierr, units)
        type(ccpp_t),                 intent(in)    :: cdata
        character(len=*),             intent(in)    :: standard_name
        integer(kind=INT64), pointer, intent(  out) :: ptr
        integer,                      intent(  out) :: ierr
        character(len=*), optional,   intent(  out) :: units

        integer                                     :: idx

        ierr = 0
        ! Lookup the standard name in the index
        idx = ccpp_fields_find(cdata, standard_name, ierr)
        if (ierr /= 0) then
            call ccpp_warn('Unable to find the requested field')
            return
        end if

        call c_f_pointer(cdata%fields(idx)%ptr, ptr)

        if (present(units)) then
            units = cdata%fields(idx)%units
        end if

    end subroutine ccpp_fields_get_i64_0

    subroutine ccpp_fields_get_i64_1(cdata, standard_name, ptr, ierr, units)
        type(ccpp_t),                 intent(in)    :: cdata
        character(len=*),             intent(in)    :: standard_name
        integer(kind=INT64), pointer, intent(  out) :: ptr(:)
        integer,                      intent(  out) :: ierr
        character(len=*), optional,   intent(  out) :: units

        integer                                     :: idx

        ierr = 0
        ! Lookup the standard name in the index
        idx = ccpp_fields_find(cdata, standard_name, ierr)
        if (ierr /= 0) then
            call ccpp_warn('Unable to find the requested field')
            return
        end if

        call c_f_pointer(cdata%fields(idx)%ptr, ptr, cdata%fields(idx)%dims)

        if (present(units)) then
            units = cdata%fields(idx)%units
        end if

    end subroutine ccpp_fields_get_i64_1

    subroutine ccpp_fields_get_i64_2(cdata, standard_name, ptr, ierr, units)
        type(ccpp_t),                 intent(in)    :: cdata
        character(len=*),             intent(in)    :: standard_name
        integer(kind=INT64), pointer, intent(  out) :: ptr(:,:)
        integer,                      intent(  out) :: ierr
        character(len=*), optional,   intent(  out) :: units

        integer                                     :: idx

        ierr = 0
        ! Lookup the standard name in the index
        idx = ccpp_fields_find(cdata, standard_name, ierr)
        if (ierr /= 0) then
            call ccpp_warn('Unable to find the requested field')
            return
        end if

        call c_f_pointer(cdata%fields(idx)%ptr, ptr, cdata%fields(idx)%dims)

        if (present(units)) then
            units = cdata%fields(idx)%units
        end if

    end subroutine ccpp_fields_get_i64_2

    subroutine ccpp_fields_get_i64_3(cdata, standard_name, ptr, ierr, units)
        type(ccpp_t),                 intent(in)    :: cdata
        character(len=*),             intent(in)    :: standard_name
        integer(kind=INT64), pointer, intent(  out) :: ptr(:,:,:)
        integer,                      intent(  out) :: ierr
        character(len=*), optional,   intent(  out) :: units

        integer                                     :: idx

        ierr = 0
        ! Lookup the standard name in the index
        idx = ccpp_fields_find(cdata, standard_name, ierr)
        if (ierr /= 0) then
            call ccpp_warn('Unable to find the requested field')
            return
        end if

        call c_f_pointer(cdata%fields(idx)%ptr, ptr, cdata%fields(idx)%dims)

        if (present(units)) then
            units = cdata%fields(idx)%units
        end if

    end subroutine ccpp_fields_get_i64_3

    !------------------------------------------------------------------!
    !>
    !! Single precision (32-bit) real field retrieval subroutines.
    !
    !------------------------------------------------------------------!
    subroutine ccpp_fields_get_r32_0(cdata, standard_name, ptr, ierr, units)
        type(ccpp_t),               intent(in)    :: cdata
        character(len=*),           intent(in)    :: standard_name
        real(kind=REAL32), pointer, intent(  out) :: ptr
        integer,                    intent(  out) :: ierr
        character(len=*), optional, intent(  out) :: units

        integer                                   :: idx

        ierr = 0
        ! Lookup the standard name in the index
        idx = ccpp_fields_find(cdata, standard_name, ierr)
        if (ierr /= 0) then
            call ccpp_warn('Unable to find the requested field')
            return
        end if

        call c_f_pointer(cdata%fields(idx)%ptr, ptr)

        if (present(units)) then
            units = cdata%fields(idx)%units
        end if

    end subroutine ccpp_fields_get_r32_0

    subroutine ccpp_fields_get_r32_1(cdata, standard_name, ptr, ierr, units)
        type(ccpp_t),               intent(in)    :: cdata
        character(len=*),           intent(in)    :: standard_name
        real(kind=REAL32), pointer, intent(  out) :: ptr(:)
        integer,                    intent(  out) :: ierr
        character(len=*), optional, intent(  out) :: units

        integer                                   :: idx

        ierr = 0
        ! Lookup the standard name in the index
        idx = ccpp_fields_find(cdata, standard_name, ierr)
        if (ierr /= 0) then
            call ccpp_warn('Unable to find the requested field')
            return
        end if

        call c_f_pointer(cdata%fields(idx)%ptr, ptr, cdata%fields(idx)%dims)

        if (present(units)) then
            units = cdata%fields(idx)%units
        end if

    end subroutine ccpp_fields_get_r32_1

    subroutine ccpp_fields_get_r32_2(cdata, standard_name, ptr, ierr, units)
        type(ccpp_t),               intent(in)    :: cdata
        character(len=*),           intent(in)    :: standard_name
        real(kind=REAL32), pointer, intent(  out) :: ptr(:,:)
        integer,                    intent(  out) :: ierr
        character(len=*), optional, intent(  out) :: units

        integer                                   :: idx

        ierr = 0
        ! Lookup the standard name in the index
        idx = ccpp_fields_find(cdata, standard_name, ierr)
        if (ierr /= 0) then
            call ccpp_warn('Unable to find the requested field')
            return
        end if

        call c_f_pointer(cdata%fields(idx)%ptr, ptr, cdata%fields(idx)%dims)

        if (present(units)) then
            units = cdata%fields(idx)%units
        end if

    end subroutine ccpp_fields_get_r32_2

    subroutine ccpp_fields_get_r32_3(cdata, standard_name, ptr, ierr, units)
        type(ccpp_t),               intent(in)    :: cdata
        character(len=*),           intent(in)    :: standard_name
        real(kind=REAL32), pointer, intent(  out) :: ptr(:,:,:)
        integer,                    intent(  out) :: ierr
        character(len=*), optional, intent(  out) :: units

        integer                                   :: idx

        ierr = 0
        ! Lookup the standard name in the index
        idx = ccpp_fields_find(cdata, standard_name, ierr)
        if (ierr /= 0) then
            call ccpp_warn('Unable to find the requested field')
            return
        end if

        call c_f_pointer(cdata%fields(idx)%ptr, ptr, cdata%fields(idx)%dims)

        if (present(units)) then
            units = cdata%fields(idx)%units
        end if

    end subroutine ccpp_fields_get_r32_3

    !------------------------------------------------------------------!
    !>
    !! Double precision (64-bit) real field retrieval subroutines.
    !
    !------------------------------------------------------------------!
    subroutine ccpp_fields_get_r64_0(cdata, standard_name, ptr, ierr, units)
        type(ccpp_t),               intent(in)    :: cdata
        character(len=*),           intent(in)    :: standard_name
        real(kind=REAL64), pointer, intent(  out) :: ptr
        integer,                    intent(  out) :: ierr
        character(len=*), optional, intent(  out) :: units

        integer                                   :: idx

        ierr = 0
        ! Lookup the standard name in the index
        idx = ccpp_fields_find(cdata, standard_name, ierr)
        if (ierr /= 0) then
            call ccpp_warn('Unable to find the requested field')
            return
        end if

        call c_f_pointer(cdata%fields(idx)%ptr, ptr)

        if (present(units)) then
            units = cdata%fields(idx)%units
        end if

    end subroutine ccpp_fields_get_r64_0

    subroutine ccpp_fields_get_r64_1(cdata, standard_name, ptr, ierr, units)
        type(ccpp_t),               intent(in)    :: cdata
        character(len=*),           intent(in)    :: standard_name
        real(kind=REAL64), pointer, intent(  out) :: ptr(:)
        integer,                    intent(  out) :: ierr
        character(len=*), optional, intent(  out) :: units

        integer                                   :: idx

        ierr = 0
        ! Lookup the standard name in the index
        idx = ccpp_fields_find(cdata, standard_name, ierr)
        if (ierr /= 0) then
            call ccpp_warn('Unable to find the requested field')
            return
        end if

        call c_f_pointer(cdata%fields(idx)%ptr, ptr, cdata%fields(idx)%dims)

        if (present(units)) then
            units = cdata%fields(idx)%units
        end if

    end subroutine ccpp_fields_get_r64_1

    subroutine ccpp_fields_get_r64_2(cdata, standard_name, ptr, ierr, units)
        type(ccpp_t),               intent(in)    :: cdata
        character(len=*),           intent(in)    :: standard_name
        real(kind=REAL64), pointer, intent(  out) :: ptr(:,:)
        integer,                    intent(  out) :: ierr
        character(len=*), optional, intent(  out) :: units

        integer                                   :: idx

        ierr = 0
        ! Lookup the standard name in the index
        idx = ccpp_fields_find(cdata, standard_name, ierr)
        if (ierr /= 0) then
            call ccpp_warn('Unable to find the requested field')
            return
        end if

        call c_f_pointer(cdata%fields(idx)%ptr, ptr, cdata%fields(idx)%dims)

        if (present(units)) then
            units = cdata%fields(idx)%units
        end if

    end subroutine ccpp_fields_get_r64_2

    subroutine ccpp_fields_get_r64_3(cdata, standard_name, ptr, ierr, units)
        type(ccpp_t),               intent(in)    :: cdata
        character(len=*),           intent(in)    :: standard_name
        real(kind=REAL64), pointer, intent(  out) :: ptr(:,:,:)
        integer,                    intent(  out) :: ierr
        character(len=*), optional, intent(  out) :: units

        integer                                   :: idx

        ierr = 0
        ! Lookup the standard name in the index
        idx = ccpp_fields_find(cdata, standard_name, ierr)
        if (ierr /= 0) then
            call ccpp_warn('Unable to find the requested field')
            return
        end if

        call c_f_pointer(cdata%fields(idx)%ptr, ptr, cdata%fields(idx)%dims)

        if (present(units)) then
            units = cdata%fields(idx)%units
        end if

    end subroutine ccpp_fields_get_r64_3

    !------------------------------------------------------------------!
    !>
    !! Logical field retrieval subroutines.
    !
    !------------------------------------------------------------------!
    subroutine ccpp_fields_get_l_0(cdata, standard_name, ptr, ierr, units)
        type(ccpp_t),               intent(in)    :: cdata
        character(len=*),           intent(in)    :: standard_name
        logical, pointer,           intent(  out) :: ptr
        integer,                    intent(  out) :: ierr
        character(len=*), optional, intent(  out) :: units

        integer                                   :: idx

        ierr = 0
        ! Lookup the standard name in the index
        idx = ccpp_fields_find(cdata, standard_name, ierr)
        if (ierr /= 0) then
            call ccpp_warn('Unable to find the requested field')
            return
        end if

        call c_f_pointer(cdata%fields(idx)%ptr, ptr)

        if (present(units)) then
            units = cdata%fields(idx)%units
        end if

    end subroutine ccpp_fields_get_l_0

    subroutine ccpp_fields_get_l_1(cdata, standard_name, ptr, ierr, units)
        type(ccpp_t),               intent(in)    :: cdata
        character(len=*),           intent(in)    :: standard_name
        logical, pointer,           intent(  out) :: ptr(:)
        integer,                    intent(  out) :: ierr
        character(len=*), optional, intent(  out) :: units

        integer                                   :: idx

        ierr = 0
        ! Lookup the standard name in the index
        idx = ccpp_fields_find(cdata, standard_name, ierr)
        if (ierr /= 0) then
            call ccpp_warn('Unable to find the requested field')
            return
        end if

        call c_f_pointer(cdata%fields(idx)%ptr, ptr, cdata%fields(idx)%dims)

        if (present(units)) then
            units = cdata%fields(idx)%units
        end if

    end subroutine ccpp_fields_get_l_1

    subroutine ccpp_fields_get_l_2(cdata, standard_name, ptr, ierr, units)
        type(ccpp_t),               intent(in)    :: cdata
        character(len=*),           intent(in)    :: standard_name
        logical, pointer,           intent(  out) :: ptr(:,:)
        integer,                    intent(  out) :: ierr
        character(len=*), optional, intent(  out) :: units

        integer                                   :: idx

        ierr = 0
        ! Lookup the standard name in the index
        idx = ccpp_fields_find(cdata, standard_name, ierr)
        if (ierr /= 0) then
            call ccpp_warn('Unable to find the requested field')
            return
        end if

        call c_f_pointer(cdata%fields(idx)%ptr, ptr, cdata%fields(idx)%dims)

        if (present(units)) then
            units = cdata%fields(idx)%units
        end if

    end subroutine ccpp_fields_get_l_2

    subroutine ccpp_fields_get_l_3(cdata, standard_name, ptr, ierr, units)
        type(ccpp_t),               intent(in)    :: cdata
        character(len=*),           intent(in)    :: standard_name
        logical, pointer,           intent(  out) :: ptr(:,:,:)
        integer,                    intent(  out) :: ierr
        character(len=*), optional, intent(  out) :: units

        integer                                   :: idx

        ierr = 0
        ! Lookup the standard name in the index
        idx = ccpp_fields_find(cdata, standard_name, ierr)
        if (ierr /= 0) then
            call ccpp_warn('Unable to find the requested field')
            return
        end if

        call c_f_pointer(cdata%fields(idx)%ptr, ptr, cdata%fields(idx)%dims)

        if (present(units)) then
            units = cdata%fields(idx)%units
        end if

    end subroutine ccpp_fields_get_l_3
    !------------------------------------------------------------------!

end module ccpp_fields
