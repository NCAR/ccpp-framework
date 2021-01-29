module ccpp_constituent_prop_mod

   ! ccpp_contituent_prop_mod contains types and procedures for storing
   ! and retrieving constituent properties

   implicit none
   private

   integer, parameter :: unassigned = -1

   type, public :: ccpp_constituent_properties_type
      character(len=:), private, allocatable :: std_name
      character(len=:), private, allocatable :: vert_dim
      integer,          private              :: field_ind = unassigned
      logical,          private              :: advected = .false.
   contains
      ! Informational methods
      procedure :: is_initialized     => ccp_is_initialized
      procedure :: stdandard_name     => ccp_get_standard_name
      procedure :: vertical_dimension => ccp_get_vertical_dimension
      procedure :: field_index        => ccp_field_index
      procedure :: is_advected        => ccp_is_advected
      ! Methods that change state
      procedure :: initialize         => ccp_initialize
      procedure :: deallocate         => ccp_deallocate
      procedure :: set_field_index    => ccp_set_field_index
   end type ccpp_constituent_properties_type

   private unassigned
   private handle_allocate_error

CONTAINS

   subroutine handle_allocate_error(astat, fieldname, errflg, errmsg)
      ! Generate an error message if <astat> indicates an allocation failure
      integer,          intent(in)  :: astat
      character(len=*), intent(in)  :: fieldname
      integer,          intent(out) :: errflg
      character(len=*), intent(out) :: errmsg

      if (astat /= 0) then
         errflg = astat
         write(errmsg, '(4a,i0)') 'Error allocating ',                        &
              'ccpp_constituent_properties_type object component, ',          &
              trim(fieldname), ', error code = ', astat
      else
         errflg = 0
         errmsg = ''
      end if

   end subroutine handle_allocate_error

   logical function ccp_is_initialized(this, errflg, errmsg)
      ! Return .true. iff <this> is initialized
      ! If <this> is *not* initialized and <errflg> and/or <errmsg> is present,
      !    fill these fields with an error status
      ! If <this> *is* initialized and <errflg> and/or <errmsg> is present,
      !    clear these fields.
      class(ccpp_constituent_properties_type), intent(in)  :: this
      integer,          optional,              intent(out) :: errflg
      character(len=*), optional,              intent(out) :: errmsg

      ccp_is_initialized = allocated(this%std_name)
      if (ccp_is_initialized) then
         if (present(errflg)) then
            errflg = 0
         end if
         if (present(errmsg)) then
            errmsg = ''
         end if
      else
         if (present(errflg)) then
            errflg = 1
         end if
         if (present(errmsg)) then
            write(errmsg, *) 'ccpp_constituent_properties_type object ',      &
                 'is not initialized'
         end if
      end if

   end function ccp_is_initialized

   subroutine ccp_initialize(this, std_name, vertical_dim, advected,          &
        errflg, errmsg)
      ! Initialize all fields in <this>
      ! Dummy arguments
      class(ccpp_constituent_properties_type), intent(inout) :: this
      character(len=*),                        intent(in)    :: std_name
      character(len=*),                        intent(in)    :: vertical_dim
      logical, optional,                       intent(in)    :: advected
      integer,                                 intent(out)   :: errflg
      character(len=*),                        intent(out)   :: errmsg
      ! Local variable
      integer :: astat

      if (this%is_initialized()) then
         errflg = 1
         write(errmsg, *) 'ccpp_constituent_properties_type object, ',        &
              trim(std_name), ', is already initialized as ', this%std_name
      else
         errflg = 0
         errmsg = ''
         this%std_name = trim(std_name)
      end if
      if (errflg == 0) then
         this%vert_dim = trim(vertical_dim)
      end if
      if (errflg == 0) then
         if (present(advected)) then
            this%advected = advected
         else
            this%advected = .false.
         end if
      end if
      if (errflg /= 0) then
         call this%deallocate()
      end if
   end subroutine ccp_initialize

   subroutine ccp_deallocate(this)
      ! Deallocate memory associated with this constituent property object
      class(ccpp_constituent_properties_type), intent(inout) :: this

      if (allocated(this%std_name)) then
         deallocate(this%std_name)
      end if
      if (allocated(this%vert_dim)) then
         deallocate(this%vert_dim)
      end if
      this%field_ind = unassigned
      this%advected = .false.

   end subroutine ccp_deallocate

   subroutine ccp_get_standard_name(this, std_name, errflg, errmsg)
      ! Return this constituent's standard name
      class(ccpp_constituent_properties_type), intent(in)  :: this
      character(len=*),                        intent(out) :: std_name
      integer,          optional,              intent(out) :: errflg
      character(len=*), optional,              intent(out) :: errmsg

      if (this%is_initialized(errflg, errmsg)) then
         std_name = this%std_name
      end if
   end subroutine ccp_get_standard_name

   subroutine ccp_get_vertical_dimension(this, vert_dim, errflg, errmsg)
      ! Return the standard name of this constituent's vertical dimension
      class(ccpp_constituent_properties_type), intent(in)  :: this
      character(len=*),                        intent(out) :: vert_dim
      integer,          optional,              intent(out) :: errflg
      character(len=*), optional,              intent(out) :: errmsg

      if (this%is_initialized(errflg, errmsg)) then
         vert_dim = this%std_name
      end if
   end subroutine ccp_get_vertical_dimension

   integer function ccp_field_index(this, errflg, errmsg)
      ! Return this constituent's field index (or -1 of not assigned)
      class(ccpp_constituent_properties_type), intent(in)  :: this
      integer,          optional,              intent(out) :: errflg
      character(len=*), optional,              intent(out) :: errmsg

      if (this%is_initialized(errflg, errmsg)) then
         ccp_field_index = this%field_ind
      end if
   end function ccp_field_index

   subroutine ccp_set_field_index(this, findex, errflg, errmsg)
      ! Set this constituent's field index
      ! It is an error to try to set an index if it is already set
      class(ccpp_constituent_properties_type), intent(inout) :: this
      integer,                                 intent(in)    :: findex
      integer,          optional,              intent(out)   :: errflg
      character(len=*), optional,              intent(out)   :: errmsg

      if (this%is_initialized(errflg, errmsg)) then
         if (this%field_ind /= unassigned) then
            this%field_ind = findex
         else
            if (present(errflg)) then
               errflg = 1
            end if
            if (present(errmsg)) then
               write(errmsg, *) 'ccpp_constituent_properties_type ',          &
                    'field index is already set'
            end if
         end if
      end if
   end subroutine ccp_set_field_index

   logical function ccp_is_advected(this, errflg, errmsg)
      class(ccpp_constituent_properties_type), intent(in)  :: this
      integer,          optional,              intent(out) :: errflg
      character(len=*), optional,              intent(out) :: errmsg

      if (this%is_initialized(errflg, errmsg)) then
         ccp_is_advected = this%advected
      end if
   end function ccp_is_advected

end module ccpp_constituent_prop_mod
