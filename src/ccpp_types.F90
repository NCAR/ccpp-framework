!
! This work (Common Community Physics Package), identified by NOAA, NCAR,
! CU/CIRES, is free of known copyright restrictions and is placed in the
! public domain.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
! THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
! IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
! CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
!

!>
!! @brief Type definitions module.
!!
!! @details The types module provides definitions for
!!          atmospheric driver to call the CCPP.
!
module ccpp_types

#if 0
!! \section arg_table_ccpp_types
!! | local_name                        | standard_name             | long_name                                             | units   | rank | type      |   kind   | intent | optional |
!! |-----------------------------------|-------------------------- |-------------------------------------------------------|---------|------|-----------|----------|--------|----------|
!! | cdata%errflg                      | ccpp_error_flag           | error flag for error handling in CCPP                 | flag    |    0 | integer   |          | none   | F        |
!! | cdata%errmsg                      | ccpp_error_message        | error message for error handling in CCPP              | none    |    0 | character | len=512  | none   | F        |
!! | cdata%loop_cnt                    | ccpp_loop_counter         | loop counter for subcycling loops in CCPP             | index   |    0 | integer   |          | none   | F        |
!!
#endif

    use, intrinsic :: iso_c_binding,                                   &
                      only: c_ptr, c_funptr

    implicit none

    private
    public :: CCPP_STR_LEN,                                            &
              CCPP_STAGES,                                             &
              CCPP_DEFAULT_STAGE,                                      &
              CCPP_DEFAULT_LOOP_CNT,                                   &
              CCPP_GENERIC_KIND,                                       &
              ccpp_t,                                                  &
              ccpp_field_t,                                            &
              ccpp_scheme_t,                                           &
              ccpp_suite_t,                                            &
              ccpp_group_t,                                            &
              ccpp_subcycle_t

    !> @var CCPP_STR_LEN Parameter defined for string lengths.
    integer, parameter   :: CCPP_STR_LEN = 256
    
    !> @var The stages=functions that are defined for each scheme.
    character(len=*), dimension(1:3), parameter :: CCPP_STAGES =       &
                                                    & (/ 'init    ',   &
                                                    &    'run     ',   &
                                                    &    'finalize' /)

    !> @var The default stage if not specified
    character(len=*), parameter :: CCPP_DEFAULT_STAGE = 'run'

    !> @var The default "kind" for a generic pointer / derived data type
    integer, parameter :: CCPP_GENERIC_KIND = -999

    !> @var The default loop counter indicating outside of a subcycle loop
    integer, parameter :: CCPP_DEFAULT_LOOP_CNT = -999

    !>
    !! @brief CCPP field type
    !!
    !! The field type contains all the information/meta-data and data
    !! for fields that need to be passed between the atmosphere driver
    !! and the physics drivers.
    type :: ccpp_field_t
            character(len=CCPP_STR_LEN)                       :: standard_name
            character(len=CCPP_STR_LEN)                       :: long_name
            character(len=CCPP_STR_LEN)                       :: units
            integer                                           :: rank
            integer, allocatable, dimension(:)                :: dims
            integer                                           :: kind
            type(c_ptr)                                       :: ptr
    end type ccpp_field_t

    !>
    !! @brief CCPP scheme function type
    !!
    !! The scheme function type contains one function of a scheme.
    !
    type :: ccpp_function_t
            character(:), allocatable                         :: name
            type(c_ptr)                                       :: function_hdl
            type(c_ptr)                                       :: library_hdl
    end type ccpp_function_t

    !>
    !! @brief CCPP scheme type
    !!
    !! The scheme type contains all the scheme information.
    !
    type :: ccpp_scheme_t
            character(:), allocatable                         :: name
            character(:), allocatable                         :: library
            character(:), allocatable                         :: version
            integer                                           :: functions_max
            type(ccpp_function_t), allocatable, dimension(:)  :: functions
            logical                                           :: initialized = .false.
        contains
            procedure :: get_function_name => scheme_get_function_name
    end type ccpp_scheme_t

    !>
    !! @brief CCPP subcycle type
    !!
    !! The subcycle type contains all the scheme names and the number of
    !! times the subcycle will loop. It is a direct mapping to the group
    !! suite subcycle XML.
    !
    type :: ccpp_subcycle_t
            integer                                           :: loops_max
            integer                                           :: schemes_max
            type(ccpp_scheme_t), allocatable, dimension(:)    :: schemes
    end type ccpp_subcycle_t

    !>
    !! @brief CCPP group type
    !!
    !! The group type contains all the subcycles and the name of
    !! the group call. It is a direct mapping to the group element in XML.
    !
    type :: ccpp_group_t
            character(:), allocatable                           :: name
            integer                                             :: subcycles_max
            type(ccpp_subcycle_t), allocatable, dimension(:)    :: subcycles
    end type ccpp_group_t

    !>
    !! @brief CCPP suite type
    !!
    !! The suite type contains all the group parts names and number of
    !! times the subcycle will loop. It is a direct mapping to the
    !! suite element in XML.
    !
    type :: ccpp_suite_t
            character(:), allocatable                           :: name
            character(:), allocatable                           :: library
            character(:), allocatable                           :: version
            type(ccpp_scheme_t)                                 :: init
            type(ccpp_scheme_t)                                 :: finalize
            integer                                             :: groups_max
            type(ccpp_group_t), allocatable, dimension(:)       :: groups
    end type ccpp_suite_t

    !>
    !! @brief CCPP physics type.
    !!
    !! Generic type that contains all components to run the CCPP.
    !!
    !! - Array of fields to all the data needing to go
    !!   the physics drivers.
    !! - The suite definitions in a ccpp_suite_t type.
    !
    type :: ccpp_t
            type(c_ptr)                                         :: fields_idx
            type(ccpp_field_t), allocatable, dimension(:)       :: fields
            type(ccpp_suite_t), pointer                         :: suite => null()
            type(ccpp_suite_t)                                  :: suite_target
            logical                                             :: suite_iscopy
            logical                                             :: initialized = .false.
            ! CCPP-internal variables for physics schemes
            integer                                             :: errflg = 0
            character(len=512)                                  :: errmsg = ''
            integer                                             :: loop_cnt = CCPP_DEFAULT_LOOP_CNT
    end type ccpp_t

contains

    !>
    !! @brief Internal routine that returns the name of
    !!        a function for a given scheme and stage
    !!
    !! @param[in   ] scheme         The ccpp_scheme_t type
    !! @param[in   ] stage          The current stage
    !! @return       function_name  The name of the function
    !
    pure function scheme_get_function_name(s, stage) result(function_name)

        implicit none

        class(ccpp_scheme_t), intent(in) :: s
        character(len=*),     intent(in) :: stage

        character(:), allocatable        :: function_name

        function_name = trim(s%name) // '_' // trim(stage)

    end function scheme_get_function_name

end module ccpp_types
