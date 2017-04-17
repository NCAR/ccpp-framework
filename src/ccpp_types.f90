!>
!! @brief Type definitions module.
!!
!! @details The types module provides definitions for
!!          atmospheic driver to call the IPD and
!!          the CCPP.
!
module ccpp_types

    use, intrinsic :: iso_c_binding,                                   &
                      only: c_ptr

    implicit none

    private
    public :: STR_LEN,                                                 &
              ccpp_t,                                                  &
              ccpp_field_t,                                            &
              ccpp_suite_t,                                            &
              ccpp_ipd_t,                                              &
              ccpp_subcycle_t

    !> @var STR_LEN Parameter defined for string lengths.
    integer, parameter                          :: STR_LEN = 256

    !>
    !! @breif CCPP field type
    !!
    !! The field type contains all the information/meta-data and data
    !! for fields that need to be passed between the atmosphere driver
    !! and the physics drivers.
    type :: ccpp_field_t
            character(len=STR_LEN)                            :: standard_name
            character(len=STR_LEN)                            :: long_name
            character(len=STR_LEN)                            :: units
            integer                                           :: rank
            integer, allocatable, dimension(:)                :: dims
            type(c_ptr)                                       :: ptr
    end type ccpp_field_t

    !>
    !! @breif CCPP subcycle type
    !!
    !! The subcycle type contains all the scheme names and the number of
    !! times the subcycle will loop. It is a direct mapping to the IPD
    !! suite subcycle XML.
    !
    type :: ccpp_subcycle_t
            integer                                           :: loop
            integer                                           :: schemes_max
            integer                                           :: schemes_n
            character(len=STR_LEN), allocatable, dimension(:) :: schemes
    end type ccpp_subcycle_t

    !>
    !! @breif CCPP IPD type
    !!
    !! The ipd type contains all the subcycles and part number of
    !! the ipd call. It is a direct mapping to the IPD suite ipd 
    !! element in XML.
    !
    type :: ccpp_ipd_t
            integer                                             :: part
            integer                                             :: subcycles_max
            integer                                             :: subcycles_n
            type(ccpp_subcycle_t), allocatable, dimension(:)    :: subcycles
    end type ccpp_ipd_t

    !>
    !! @breif CCPP suite type
    !!
    !! The suite type contains all the ipd parts names and number of
    !! times the subcycle will loop. It is a direct mapping to the
    !! IPD suite subcycle XML.
    !
    type :: ccpp_suite_t
            character(:), allocatable                           :: name
            integer                                             :: ipds_max
            integer                                             :: ipd_n
            type(ccpp_ipd_t), allocatable, dimension(:)         :: ipds
    end type ccpp_suite_t

    !>
    !! @breif CCPP Atmosphere/IPD/Physics type.
    !!
    !! Generic type that contains all components to run the CCPP.
    !!
    !! - Array of fields to all the atmospheric data needing to go
    !!   the physics drivers.
    !! - The suite definitions in a ccpp_suite_t type.
    !
    type :: ccpp_t
            type(c_ptr)                                         :: fields_idx
            type(ccpp_field_t), allocatable, dimension(:)       :: fields
            type(ccpp_suite_t)                                  :: suite
    end type ccpp_t

end module ccpp_types
