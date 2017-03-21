!>
!! @brief Type definitions module.
!!
!! @details The types module provides definitions for
!!          atmospheic driver to call the IPD and
!!          the CCPP.
!
module types

    use, intrinsic :: iso_c_binding,            &
                      only: c_ptr

    implicit none

    private
    public :: STR_LEN,                          &
              aip_t, field_t,                   &
              subcycle_t, ipd_t, suite_t

    !> @var STR_LEN Parameter defined for string lengths.
    integer, parameter                          :: STR_LEN = 256

    !>
    !! @typedef field_t
    !! @breif Field type
    !!
    !! The field type contains all the information/meta-data and data
    !! for fields that need to be passed between the atmosphere driver
    !! and the physics drivers.
    !
    type :: field_t
            character(len=STR_LEN)                            :: standard_name
            character(len=STR_LEN)                            :: long_name
            character(len=STR_LEN)                            :: units
            integer                                           :: rank
            integer, allocatable, dimension(:)                :: dims
            type(c_ptr)                                       :: ptr
    end type field_t



    !>
    !! @typedef subcycle_t
    !! @breif Subcycle type
    !!
    !! The subcycle type contains all the scheme names and the number of
    !! times the subcycle will loop. It is a direct mapping to the IPD
    !! suite subcycle XML.
    !
    type :: subcycle_t
            integer                                           :: loop
            integer                                           :: schemes_max
            integer                                           :: schemes_n
            character(len=STR_LEN), allocatable, dimension(:) :: schemes
    end type subcycle_t

    !! @typedef ipd_t
    !! @breif IPD type
    !!
    !! The ipd type contains all the subcycles and part number of
    !! the ipd call. It is a direct mapping to the IPD suite ipd 
    !! element in XML.
    !
    type :: ipd_t
            integer                                             :: part
            integer                                             :: subcycles_max
            integer                                             :: subcycles_n
            type(subcycle_t), allocatable, dimension(:)         :: subcycles
    end type ipd_t

    !! @typedef suite_t
    !! @breif Suite type
    !!
    !! The suite type contains all the ipd parts names and number of
    !! times the subcycle will loop. It is a direct mapping to the
    !! IPD suite subcycle XML.
    !
    type :: suite_t
            character(:), allocatable                           :: name
            integer                                             :: ipds_max
            integer                                             :: ipd_n
            type(ipd_t), allocatable, dimension(:)              :: ipds
    end type suite_t

    !>
    !! @typedef aip_t
    !! @breif Atmosphere/IPD/Physics type.
    !!
    !! Generic type that contains all components to run the IPD.
    !!
    !! - Array of fields to all the atmospheric data needing
    !!   to go the physics drivers
    !! - The total number of IPD calls.
    !! - The current IPD call number.
    !! - The current physics call number.
    !! - Physical tendency 1
    !! - Physical tendency 2
    !! - Physical tendency 3
    !! - Physical tendency 4
    !! - Physical tendency 5.
    !! - Array containing the number of schemes per IPD call.
    !! - Array containing the schemese to call per IPD call.
    !
    type :: aip_t
            real                                                :: tendency1
            real                                                :: tendency2
            real                                                :: tendency3
            real                                                :: tendency4
            real                                                :: tendency5
            integer,                allocatable, dimension(:)   :: n_schemes
            character(len=STR_LEN), allocatable, dimension(:,:) :: schemes
            type(c_ptr)                                         :: fields_idx !< Data field index
            integer                                             :: fields_max !< Maximum number of fields
            integer                                             :: fields_n   !< Currently used fields
            type(field_t), allocatable, dimension(:)            :: fields     !< Data for fields
            type(suite_t)                                       :: suite      !< XML Suite
    end type aip_t

end module types
