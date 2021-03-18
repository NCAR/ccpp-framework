module ccpp_constituent_prop_mod

   ! ccpp_contituent_prop_mod contains types and procedures for storing
   ! and retrieving constituent properties

   use ccpp_hashable,   only: ccpp_hashable_t, ccpp_hashable_char_t
   use ccpp_hash_table, only: ccpp_hash_table_t, ccpp_hash_iterator_t
   use ccpp_kinds,      only: kind_phys

   implicit none
   private

   integer,         parameter :: int_unassigned = -1
   real(kind_phys), parameter :: kphys_unassigned = HUGE(1.0_kind_phys)

   !!XXgoldyXX: NB: We end up with two copies of each metadata object, FIX!!

   type, public, extends(ccpp_hashable_char_t) :: ccpp_constituent_properties_t
      ! A ccpp_constituent_properties_t object holds relevant metadata
      !   for a constituent species and provides interfaces to access that data.
      character(len=:), private, allocatable :: std_name
      character(len=:), private, allocatable :: vert_dim
      integer,          private              :: const_ind = int_unassigned
      integer,          private              :: field_ind = int_unassigned
      logical,          private              :: advected = .false.
   contains
      ! Required hashable method
      procedure :: key => ccp_properties_get_key
      ! Informational methods
      procedure :: is_initialized     => ccp_is_initialized
      procedure :: standard_name      => ccp_get_standard_name
      procedure :: is_layer_var       => ccp_is_layer_var
      procedure :: is_interface_var   => ccp_is_interface_var
      procedure :: is_2d_var          => ccp_is_2d_var
      procedure :: vertical_dimension => ccp_get_vertical_dimension
      procedure :: const_index        => ccp_const_index
      procedure :: field_index        => ccp_field_index
      procedure :: is_advected        => ccp_is_advected
      procedure :: equivalent         => ccp_is_equivalent
      ! Copy method (be sure to update this anytime fields are added)
      procedure :: copyConstituent
      generic :: assignment(=) => copyConstituent
      ! Methods that change state
      procedure :: initialize      => ccp_initialize
      procedure :: deallocate      => ccp_deallocate
      procedure :: set_const_index => ccp_set_const_index
      procedure :: set_field_index => ccp_set_field_index
   end type ccpp_constituent_properties_t

   type, public :: ccpp_model_constituents_t
      ! A ccpp_model_constituents_t object holds all the metadata and field
      !   data for a model run's constituents along with data and methods
      !   to initialize and access the data.
      integer,                 private :: num_layer_vars = 0
      integer,                 private :: num_interface_vars = 0
      integer,                 private :: num_2d_vars = 0
      integer,                 private :: num_layers = 0
      integer,                 private :: num_interfaces = 0
      type(ccpp_hash_table_t), private :: hash_table
      logical,                 private :: table_locked = .false.
      ! These fields are public to allow for efficient (i.e., no copying)
      !   usage even though it breaks object independence
      real(kind_phys), allocatable     :: vars_layer(:,:,:)
      real(kind_phys), allocatable     :: vars_interface(:,:,:)
      real(kind_phys), allocatable     :: vars_2d(:,:)
      type(ccpp_constituent_properties_t), allocatable :: const_metadata(:)
   contains
      ! Return .true. if a constituent matches pattern
      procedure, private :: is_match => ccp_model_const_is_match
      ! Return a constituent from the hash table
      procedure, private :: find_const => ccp_model_const_find_const
      ! Is the table locked (i.e., ready to be used)?
      procedure :: locked => ccp_model_const_locked
      ! Is it okay to add new metadata fields?
      procedure :: okay_to_add => ccp_model_const_okay_to_add
      ! Add a constituent's metadata to the master hash table
      procedure :: new_field => ccp_model_const_add_metadata
      ! Initialize hash table
      procedure :: initialize_table => ccp_model_const_initialize
      ! Freeze hash table and initialize constituent field arrays
      procedure :: lock_table => ccp_model_const_lock
      ! Empty (reset) the entire object
      procedure :: reset => ccp_model_const_reset
      ! Query number of constituents matching pattern
      procedure :: num_constituents => ccp_model_const_num_match
      ! Gather constituent fields matching pattern
      !!XXgoldyXX: Might need a 2D version of this
      procedure :: copy_in => ccp_model_const_copy_in_3d
      ! Update constituent fields matching pattern
      !!XXgoldyXX: Might need a 2D version of this
      procedure :: copy_out => ccp_model_const_copy_out_3d
      ! Return index of constituent matching standard name
      procedure :: const_index => ccp_model_const_index
      ! Return index of field matching standard name
      procedure :: field_index => ccp_model_const_field_index
      ! Return metadata matching standard name
      procedure :: field_metada => ccp_model_const_metadata
   end type ccpp_model_constituents_t

   private int_unassigned
   private handle_allocate_error

CONTAINS

   !########################################################################
   !
   ! CCPP_CONSTITUENT_PROPERTIES_T (constituent metadata) methods
   !
   !########################################################################

   subroutine copyConstituent(outConst, inConst)
      ! Dummy arguments
      class(ccpp_constituent_properties_t), intent(inout) :: outConst
      type(ccpp_constituent_properties_t),  intent(in)    :: inConst

      outConst%std_name = inConst%std_name
      outConst%vert_dim = inConst%vert_dim
      outConst%const_ind = inConst%const_ind
      outConst%field_ind = inConst%field_ind
      outConst%advected = inConst%advected
   end subroutine copyConstituent

   !#######################################################################

   subroutine handle_allocate_error(astat, fieldname, errflg, errmsg)
      ! Generate an error message if <astat> indicates an allocation failure

      ! Dummy arguments
      integer,                    intent(in)  :: astat
      character(len=*),           intent(in)  :: fieldname
      integer,          optional, intent(out) :: errflg
      character(len=*), optional, intent(out) :: errmsg

      if (astat /= 0) then
         if (present(errflg)) then
            errflg = astat
         end if
         if (present(errmsg)) then
            write(errmsg, '(4a,i0)') 'Error allocating ',                     &
                 'ccpp_constituent_properties_t object component, ',          &
                 trim(fieldname), ', error code = ', astat
         end if
      else
         if (present(errflg)) then
            errflg = 0
         end if
         if (present(errmsg)) then
            errmsg = ''
         end if
      end if

   end subroutine handle_allocate_error

   !#######################################################################

   function ccp_properties_get_key(hashable)
      ! Return the constituent properties class key (std_name)

      ! Dummy arguments
      class(ccpp_constituent_properties_t), intent(in) :: hashable
      character(len=:), allocatable                    :: ccp_properties_get_key

      ccp_properties_get_key = hashable%std_name

   end function ccp_properties_get_key

   !#######################################################################

   logical function ccp_is_initialized(this, errflg, errmsg)
      ! Return .true. iff <this> is initialized
      ! If <this> is *not* initialized and <errflg> and/or <errmsg> is present,
      !    fill these fields with an error status
      ! If <this> *is* initialized and <errflg> and/or <errmsg> is present,
      !    clear these fields.

      ! Dummy arguments
      class(ccpp_constituent_properties_t), intent(in)  :: this
      integer,          optional,           intent(out) :: errflg
      character(len=*), optional,           intent(out) :: errmsg

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
            write(errmsg, *) 'ccpp_constituent_properties_t object ',      &
                 'is not initialized'
         end if
      end if

   end function ccp_is_initialized

   !#######################################################################

   subroutine ccp_initialize(this, std_name, vertical_dim, advected,          &
        errflg, errmsg)
      ! Initialize all fields in <this>

      ! Dummy arguments
      class(ccpp_constituent_properties_t), intent(inout) :: this
      character(len=*),                     intent(in)    :: std_name
      character(len=*),                     intent(in)    :: vertical_dim
      logical, optional,                    intent(in)    :: advected
      integer,                              intent(out)   :: errflg
      character(len=*),                     intent(out)   :: errmsg
      ! Local variable
      integer :: astat

      if (this%is_initialized()) then
         errflg = 1
         write(errmsg, *) 'ccpp_constituent_properties_t object, ',        &
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

   !#######################################################################

   subroutine ccp_deallocate(this)
      ! Deallocate memory associated with this constituent property object

      ! Dummy argument
      class(ccpp_constituent_properties_t), intent(inout) :: this

      if (allocated(this%std_name)) then
         deallocate(this%std_name)
      end if
      if (allocated(this%vert_dim)) then
         deallocate(this%vert_dim)
      end if
      this%field_ind = int_unassigned
      this%advected = .false.

   end subroutine ccp_deallocate

   !#######################################################################

   subroutine ccp_get_standard_name(this, std_name, errflg, errmsg)
      ! Return this constituent's standard name

      ! Dummy arguments
      class(ccpp_constituent_properties_t), intent(in)  :: this
      character(len=*),                     intent(out) :: std_name
      integer,          optional,           intent(out) :: errflg
      character(len=*), optional,           intent(out) :: errmsg

      if (this%is_initialized(errflg, errmsg)) then
         std_name = this%std_name
      end if
   end subroutine ccp_get_standard_name

   !#######################################################################

   subroutine ccp_get_vertical_dimension(this, vert_dim, errflg, errmsg)
      ! Return the standard name of this constituent's vertical dimension

      ! Dummy arguments
      class(ccpp_constituent_properties_t), intent(in)  :: this
      character(len=*),                     intent(out) :: vert_dim
      integer,          optional,           intent(out) :: errflg
      character(len=*), optional,           intent(out) :: errmsg

      if (this%is_initialized(errflg, errmsg)) then
         vert_dim = this%vert_dim
      end if
   end subroutine ccp_get_vertical_dimension

   !#######################################################################

   logical function ccp_is_layer_var(this) result(is_layer)
      ! Return .true. iff this constituent has a layer vertical dimension

      ! Dummy arguments
      class(ccpp_constituent_properties_t), intent(in)  :: this
      ! Local variable
      character(len=32) :: dimname

      call this%vertical_dimension(dimname)
      is_layer = trim(dimname) == 'vertical_layer_dimension'

   end function ccp_is_layer_var

   !#######################################################################

   logical function ccp_is_interface_var(this) result(is_interface)
      ! Return .true. iff this constituent has a interface vertical dimension

      ! Dummy arguments
      class(ccpp_constituent_properties_t), intent(in)  :: this
      ! Local variable
      character(len=32) :: dimname

      call this%vertical_dimension(dimname)
      is_interface = trim(dimname) == 'vertical_interface_dimension'

   end function ccp_is_interface_var

   !#######################################################################

   logical function ccp_is_2d_var(this) result(is_2d)
      ! Return .true. iff this constituent has a 2d vertical dimension

      ! Dummy arguments
      class(ccpp_constituent_properties_t), intent(in)  :: this
      ! Local variable
      character(len=32) :: dimname

      call this%vertical_dimension(dimname)
      is_2d = len_trim(dimname) == 0

   end function ccp_is_2d_var

   !#######################################################################

   integer function ccp_const_index(this, errflg, errmsg)
      ! Return this constituent's master index (or -1 of not assigned)

      ! Dummy arguments
      class(ccpp_constituent_properties_t), intent(in)  :: this
      integer,          optional,           intent(out) :: errflg
      character(len=*), optional,           intent(out) :: errmsg

      if (this%is_initialized(errflg, errmsg)) then
         ccp_const_index = this%const_ind
      end if
   end function ccp_const_index

   !#######################################################################

   integer function ccp_field_index(this, errflg, errmsg)
      ! Return this constituent's field index (or -1 of not assigned)

      ! Dummy arguments
      class(ccpp_constituent_properties_t), intent(in)  :: this
      integer,          optional,           intent(out) :: errflg
      character(len=*), optional,           intent(out) :: errmsg

      if (this%is_initialized(errflg, errmsg)) then
         ccp_field_index = this%field_ind
      end if

   end function ccp_field_index

   !#######################################################################

   subroutine ccp_set_const_index(this, index, errflg, errmsg)
      ! Set this constituent's index in the master constituent array
      ! It is an error to try to set an index if it is already set

      ! Dummy arguments
      class(ccpp_constituent_properties_t), intent(inout) :: this
      integer,                              intent(in)    :: index
      integer,          optional,           intent(out)   :: errflg
      character(len=*), optional,           intent(out)   :: errmsg

      if (this%is_initialized(errflg, errmsg)) then
         if (this%const_ind /= int_unassigned) then
            this%const_ind = index
         else
            if (present(errflg)) then
               errflg = 1
            end if
            if (present(errmsg)) then
               write(errmsg, *) 'ccpp_constituent_properties_t ',          &
                    'const index is already set'
            end if
         end if
      end if

   end subroutine ccp_set_const_index

   !#######################################################################

   subroutine ccp_set_field_index(this, findex, errflg, errmsg)
      ! Set this constituent's field index
      ! It is an error to try to set an index if it is already set

      ! Dummy arguments
      class(ccpp_constituent_properties_t), intent(inout) :: this
      integer,                              intent(in)    :: findex
      integer,          optional,           intent(out)   :: errflg
      character(len=*), optional,           intent(out)   :: errmsg

      if (this%is_initialized(errflg, errmsg)) then
         if (this%field_ind == int_unassigned) then
            this%field_ind = findex
         else
            if (present(errflg)) then
               errflg = 1
            end if
            if (present(errmsg)) then
               write(errmsg, *) 'ccpp_constituent_properties_t ',          &
                    'field index is already set'
            end if
         end if
      end if
   end subroutine ccp_set_field_index

   !#######################################################################

   logical function ccp_is_advected(this, errflg, errmsg)

      ! Dummy arguments
      class(ccpp_constituent_properties_t), intent(in)  :: this
      integer,          optional,           intent(out) :: errflg
      character(len=*), optional,           intent(out) :: errmsg

      if (this%is_initialized(errflg, errmsg)) then
         ccp_is_advected = this%advected
      end if
   end function ccp_is_advected

   !#######################################################################

   logical function ccp_is_equivalent(this, oconst,                           &
        errflg, errmsg) result(equiv)

      ! Dummy arguments
      class(ccpp_constituent_properties_t), intent(in)  :: this
      type(ccpp_constituent_properties_t),  intent(in)  :: oconst
      integer,          optional,           intent(out) :: errflg
      character(len=*), optional,           intent(out) :: errmsg

      if (this%is_initialized(errflg, errmsg) .and.                           &
           oconst%is_initialized(errflg, errmsg)) then
         equiv = (trim(this%std_name) == trim(oconst%std_name)) .and.         &
              (trim(this%vert_dim) == trim(oconst%vert_dim))    .and.         &
              (this%advected .eqv. oconst%advected)
      else
         equiv = .false.
      end if

   end function ccp_is_equivalent

   !########################################################################
   !
   ! CCPP_MODEL_CONSTITUENTS_T (constituent field data) methods
   !
   !########################################################################

   logical function ccp_model_const_locked(this, errflg, errmsg, warn_func)
      ! Return .true. iff <this> is locked (i.e., ready to use)
      ! Optionally fill out <errflg> and <errmsg> if object not initialized

      ! Dummy arguments
      class(ccpp_model_constituents_t), intent(in)  :: this
      integer,          optional,       intent(out) :: errflg
      character(len=*), optional,       intent(out) :: errmsg
      character(len=*), optional,       intent(in)  :: warn_func
      ! Local variable
      character(len=*), parameter :: subname = 'ccp_model_const_locked'

      if (present(errflg)) then
         errflg = 0
      end if
      if (present(errmsg)) then
         errmsg = ''
      end if
      ccp_model_const_locked = .false.
      ! Use an initialized hash table as double check
      if (this%hash_table%is_initialized()) then
         ccp_model_const_locked = this%table_locked
         if ( (.not. this%table_locked) .and.                                 &
              present(errmsg) .and. present(warn_func)) then
            ! Write a warning as a courtesy to calling function but do not set
            !   errflg (let caller decide).
            write(errmsg, *) trim(warn_func),                                 &
                 ' WARNING: Model constituents not ready to use'
         end if
      else
         if (present(errflg)) then
            errflg = 1
         end if
         if (present(errmsg)) then
            if (present(warn_func)) then
               write(errmsg, *) trim(warn_func),                              &
                    ' WARNING: Model constituents not initialized'
            else
               write(errmsg, *) subname,                                      &
                    ' WARNING: Model constituents not initialized'
            end if
         end if
      end if

   end function ccp_model_const_locked

   !########################################################################

   logical function ccp_model_const_okay_to_add(this, errflg, errmsg, warn_func)
      ! Return .true. iff <this> is initialized and not locked
      ! Optionally fill out <errflg> and <errmsg> if the conditions are not met.

      ! Dummy arguments
      class(ccpp_model_constituents_t),    intent(inout) :: this
      integer,          optional,          intent(out)   :: errflg
      character(len=*), optional,          intent(out)   :: errmsg
      character(len=*), optional,          intent(in)    :: warn_func
      ! Local variable
      character(len=*), parameter :: subname = 'ccp_model_const_okay_to_add'

      ccp_model_const_okay_to_add = this%hash_table%is_initialized()
      if (ccp_model_const_okay_to_add) then
         ccp_model_const_okay_to_add = .not. this%locked(errflg=errflg,       &
              errmsg=errmsg, warn_func=subname)
         if (.not. ccp_model_const_okay_to_add) then
            if (present(errflg)) then
               errflg = 1
            end if
            if (present(errmsg)) then
               if (present(warn_func)) then
                  write(errmsg, *) trim(warn_func),                           &
                       ' WARNING: Model constituents are locked'
               else
                  errmsg = subname//' WARNING: Model constituents are locked'
               end if
            end if
         end if
      else
         if (present(errflg)) then
            errflg = 1
         end if
         if (present(errmsg)) then
            if (present(warn_func)) then
               write(errmsg, *) trim(warn_func),                              &
                    ' WARNING: Model constituents not initialized'
            else
               errmsg = subname//' WARNING: Model constituents not initialized'
            end if
         end if
      end if

   end function ccp_model_const_okay_to_add

   !########################################################################

   subroutine ccp_model_const_add_metadata(this, field_data, errflg, errmsg)
      ! Add a constituent's metadata to the master hash table

      ! Dummy arguments
      class(ccpp_model_constituents_t),              intent(inout) :: this
      type(ccpp_constituent_properties_t), target,   intent(in)    :: field_data
      integer,                             optional, intent(out)   :: errflg
      character(len=*),                    optional, intent(out)   :: errmsg
      ! Local variables
      character(len=256)          :: error
      character(len=*), parameter :: subnam = 'ccp_model_const_add_metadata'

      if (this%okay_to_add(errflg=errflg, errmsg=errmsg, warn_func=subnam)) then
         error = ''
!!XXgoldyXX: Add check on key to see if incompatible item already there.
         call this%hash_table%add_hash_key(field_data, error)
         if (len_trim(error) > 0) then
            if (present(errflg)) then
               errflg = 1
            end if
            if (present(errmsg)) then
               errmsg = trim(error)
            end if
         else
            ! If we get here we are successful, add to variable count
            if (field_data%is_layer_var()) then
               this%num_layer_vars = this%num_layer_vars + 1
            else if (field_data%is_interface_var()) then
               this%num_interface_vars = this%num_interface_vars + 1
            else if (field_data%is_2d_var()) then
               this%num_2d_vars = this%num_2d_vars + 1
            else
               if (present(errflg)) then
                  errflg = 1
               end if
               if (present(errmsg)) then
                  call field_data%vertical_dimension(error,                   &
                       errflg=errflg, errmsg=errmsg)
                  if (len_trim(errmsg) == 0) then
                     write(errmsg, *) "ERROR: Unknown vertical dimension, '", &
                          trim(error), "'"
                  end if
               end if
            end if
         end if
      else
         if (present(errflg)) then
            errflg = 1
         end if
         if (present(errmsg)) then
            errmsg = 'ERROR: Model contituents are locked'
         end if
      end if

   end subroutine ccp_model_const_add_metadata

   !########################################################################

   subroutine ccp_model_const_initialize(this, num_elements)
      ! Initialize hash table, <num_elements> is total number of elements

      ! Dummy arguments
      class(ccpp_model_constituents_t), intent(inout) :: this
      integer,                          intent(in)    :: num_elements
      ! Local variable
      integer :: tbl_size

      ! Clear any data
      this%num_layer_vars = 0
      this%num_interface_vars = 0
      this%num_2d_vars = 0
      if (allocated(this%vars_layer)) then
         deallocate(this%vars_layer)
      end if
      if (allocated(this%vars_interface)) then
         deallocate(this%vars_interface)
      end if
      if (allocated(this%vars_2d)) then
         deallocate(this%vars_2d)
      end if
      if (allocated(this%const_metadata)) then
         deallocate(this%const_metadata)
      end if
      ! Figure a log base 2 for initializing hash table
      tbl_size = num_elements * 10 ! Hash padding
      tbl_size = int((log(real(tbl_size, kind_phys)) / log(2.0_kind_phys)) +  &
           1.0_kind_phys)
      ! Initialize hash table
      call this%hash_table%initialize(tbl_size)
      this%table_locked = .false.

   end subroutine ccp_model_const_initialize

   !########################################################################

   function ccp_model_const_find_const(this, standard_name, errflg, errmsg)   &
        result(cprop)
      ! Return a constituent with key, <standard_name>, from the hash table
      ! <this> must be locked to execute this function
      ! Since this is a private function, error checking for locked status
      !    is *not* performed.

      ! Dummy arguments
      class(ccpp_model_constituents_t), intent(in)  :: this
      character(len=*),                 intent(in)  :: standard_name
      integer,          optional,       intent(out) :: errflg
      character(len=*), optional,       intent(out) :: errmsg
      type(ccpp_constituent_properties_t), pointer  :: cprop
      ! Local variables
      class(ccpp_hashable_t), pointer :: hval
      character(len=256)              :: error
      character(len=*), parameter     :: subname = 'ccp_model_const_find_const'

      nullify(cprop)
      hval => this%hash_table%table_value(standard_name, errmsg=error)
      if (len_trim(error) > 0) then
         if (present(errflg)) then
            errflg = 1
         end if
         if (present(errmsg)) then
            write(errmsg, *) subname, ': ', trim(error)
         end if
      else
         select type(hval)
         type is (ccpp_constituent_properties_t)
            cprop => hval
         class default
            if (present(errflg)) then
               errflg = 1
            end if
            if (present(errmsg)) then
               write(errmsg, *) subname, ' ERROR: Bad hash table value',      &
                    trim(standard_name)
            end if
         end select
      end if

   end function ccp_model_const_find_const

   !########################################################################

   subroutine ccp_model_const_lock(this, ncols, num_layers, num_interfaces,   &
        errflg, errmsg)
      ! Freeze hash table and initialize constituent field arrays

      ! Dummy arguments
      class(ccpp_model_constituents_t), intent(inout) :: this
      integer,                          intent(in)    :: ncols
      integer,                          intent(in)    :: num_layers
      integer,                          intent(in)    :: num_interfaces
      integer,                optional, intent(out)   :: errflg
      character(len=*),       optional, intent(out)   :: errmsg
      ! Local variables
      integer                                      :: index_layer
      integer                                      :: index_interface
      integer                                      :: index_2d
      integer                                      :: index_const
      integer                                      :: astat
      type(ccpp_hash_iterator_t)                   :: hiter
      class(ccpp_hashable_t),              pointer :: hval
      type(ccpp_constituent_properties_t), pointer :: cprop
      character(len=32)                            :: dimname
      character(len=*), parameter :: subname = 'ccp_model_const_lock'

      if (this%locked(errflg=errflg, errmsg=errmsg, warn_func=subname)) then
         if (present(errflg)) then
            errflg = 1
         end if
         if (present(errmsg)) then
            if (len_trim(errmsg) == 0) then
               write(errmsg, *) subname,                                      &
                    ' WARNING: Model constituents already locked, ignoring'
            end if
         end if
      else
         index_layer = 0
         index_interface = 0
         index_2d = 0
         index_const = 0
         ! Make sure everything is really initialized
         if (allocated(this%vars_layer)) then
            deallocate(this%vars_layer)
         end if
         if (allocated(this%vars_interface)) then
            deallocate(this%vars_interface)
         end if
         if (allocated(this%vars_2d)) then
            deallocate(this%vars_2d)
         end if
         if (allocated(this%const_metadata)) then
            deallocate(this%const_metadata)
         end if
         ! Allocate the constituent array
         allocate(this%const_metadata(this%hash_table%num_values()), stat=astat)
         call handle_allocate_error(astat, 'const_metadata',                  &
              errflg=errflg, errmsg=errmsg)
         ! Iterate through the hash table to find entries
         if (astat == 0) then
            call hiter%initialize(this%hash_table)
            do
               if (hiter%valid()) then
                  index_const = index_const + 1
                  if (index_const > SIZE(this%const_metadata)) then
                     if (present(errflg)) then
                        errflg = 1
                     end if
                     if (present(errmsg)) then
                        write(errmsg, *) subname,                             &
                             " ERROR: const index out of bounds"
                     end if
                     exit
                  end if
                  hval => hiter%value()
                  select type(hval)
                  type is (ccpp_constituent_properties_t)
                     cprop => hval
                     call cprop%set_const_index(index_const,                  &
                          errflg=errflg, errmsg=errmsg)
                     ! Figure out which type of variable this is
                     if (cprop%is_layer_var()) then
                        index_layer = index_layer + 1
                        call cprop%set_field_index(index_layer,               &
                             errflg=errflg, errmsg=errmsg)
                     else if (cprop%is_interface_var()) then
                        index_interface = index_interface + 1
                        call cprop%set_field_index(index_interface,           &
                             errflg=errflg, errmsg=errmsg)
                     else if (cprop%is_2d_var()) then
                        index_2d = index_2d + 1
                        call cprop%set_field_index(index_2d,                  &
                             errflg=errflg, errmsg=errmsg)
                     else
                        if (present(errflg)) then
                           errflg = 1
                        end if
                        if (present(errmsg)) then
                           call cprop%vertical_dimension(dimname,             &
                                errflg=errflg, errmsg=errmsg)
                           if (len_trim(errmsg) == 0) then
                              write(errmsg, *) subname,                       &
                                   " ERROR: Bad vertical dimension, '",       &
                                   trim(dimname), "'"
                           end if
                        end if
                     end if
                     this%const_metadata(index_const) = cprop
                  class default
                     if (present(errflg)) then
                        errflg = 1
                     end if
                     if (present(errmsg)) then
                        write(errmsg, *) subname, 'ERROR: Bad hash table value'
                     end if
                     exit
                  end select
                  call hiter%next()
               else
                  exit
               end if
            end do
            ! Some size sanity checks
            if (index_const /= this%hash_table%num_values()) then
               if (present(errflg)) then
                  errflg = 1
               end if
               if (present(errmsg)) then
                  write(errmsg, *) subname,                                   &
                       " ERROR: Too few constituents found in hash table"
               end if
            else if (index_layer /= this%num_layer_vars) then
               if (present(errflg)) then
                  errflg = 1
               end if
               if (present(errmsg)) then
                  write(errmsg, '(2a,i0,a,i0)') subname,                      &
                       " ERROR: Wrong number of layer variables found (",     &
                       index_layer, ") should be ", this%num_layer_vars
               end if
            else if (index_interface /= this%num_interface_vars) then
               if (present(errflg)) then
                  errflg = 1
               end if
               if (present(errmsg)) then
                  write(errmsg, '(2a,i0,a,i0)') subname,                      &
                       " ERROR: Wrong number of interface variables found (", &
                       index_interface, ") should be ", this%num_interface_vars
               end if
            else if (index_2d /= this%num_2d_vars) then
               if (present(errflg)) then
                  errflg = 1
               end if
               if (present(errmsg)) then
                  write(errmsg, '(2a,i0,a,i0)') subname,                      &
                       " ERROR: Wrong number of 2D variables found (",        &
                       index_2d, ") should be ", this%num_2d_vars
               end if
            end if
            ! Everything looks okay, allocate field arrays
            allocate(this%vars_layer(ncols, num_layers, index_layer),         &
                 stat=astat)
            call handle_allocate_error(astat, 'vars_layer',                   &
                 errflg=errflg, errmsg=errmsg)
            if (astat == 0) then
               this%num_layers = num_layers
               this%vars_layer = kphys_unassigned
               allocate(this%vars_interface(ncols, num_interfaces,            &
                    index_layer), stat=astat)
               call handle_allocate_error(astat, 'vars_interface',            &
                    errflg=errflg, errmsg=errmsg)
            end if
            if (astat == 0) then
               this%num_interfaces = num_interfaces
               this%vars_interface = kphys_unassigned
               allocate(this%vars_2d(ncols, index_2d), stat=astat)
               call handle_allocate_error(astat, 'vars_2d',                   &
                    errflg=errflg, errmsg=errmsg)
            end if
            if (astat == 0) then
               this%vars_2d = kphys_unassigned
            end if
            if (present(errflg)) then
               if (errflg /= 0) then
                  astat = 1
               end if
            end if
            if (astat == 0) then
               this%table_locked = .true.
            end if
         end if
      end if

   end subroutine ccp_model_const_lock

   !########################################################################

   subroutine ccp_model_const_reset(this)
      ! Empty (reset) the entire object

      ! Dummy argument
      class(ccpp_model_constituents_t), intent(inout) :: this

      if (allocated(this%vars_layer)) then
         deallocate(this%vars_layer)
      end if
      if (allocated(this%vars_interface)) then
         deallocate(this%vars_interface)
      end if
      if (allocated(this%vars_2d)) then
         deallocate(this%vars_2d)
      end if
      if (allocated(this%const_metadata)) then
         deallocate(this%const_metadata)
      end if
      call this%hash_table%clear()

   end subroutine ccp_model_const_reset

   !########################################################################

   logical function ccp_model_const_is_match(this, index, advected)           &
        result(is_match)
      ! Return .true. iff the constituent at <index> matches a pattern
      ! Each (optional) property which is present represents something
      !    which is required as part of a match.
      ! Since this is a private function, error checking for locked status
      !    is *not* performed.

      ! Dummy arguments
      class(ccpp_model_constituents_t), intent(in) :: this
      integer,                          intent(in) :: index
      logical, optional,                intent(in) :: advected

      ! By default, every constituent is a match
      is_match = .true.
      if (present(advected)) then
         if (advected .neqv. this%const_metadata(index)%is_advected()) then
            is_match = .false.
         end if
      end if

   end function ccp_model_const_is_match

   !########################################################################

   integer function ccp_model_const_num_match(this, advected,                 &
        errflg, errmsg) result(nmatch)
      ! Query number of constituents matching pattern
      ! Each (optional) property which is present represents something
      !    which is required as part of a match.
      ! <this> must be locked to execute this function

      ! Dummy arguments
      class(ccpp_model_constituents_t), intent(in)  :: this
      logical,          optional,       intent(in)  :: advected
      integer,          optional,       intent(out) :: errflg
      character(len=*), optional,       intent(out) :: errmsg
      ! Local variables
      integer                     :: index
      character(len=*), parameter :: subname = "ccp_model_const_num_match"

      nmatch = 0
      if (this%locked(errflg=errflg, errmsg=errmsg, warn_func=subname)) then
         do index = 1, SIZE(this%const_metadata)
            if (this%is_match(index, advected=advected)) then
               nmatch = nmatch + 1
            end if
         end do
      end if

   end function ccp_model_const_num_match

   !########################################################################

   subroutine ccp_model_const_copy_in_3d(this, const_array, advected,         &
        errflg, errmsg)
      ! Gather constituent fields matching pattern
      ! Each (optional) property which is present represents something
      !    which is required as part of a match.
      ! <this> must be locked to execute this function

      ! Dummy arguments
      class(ccpp_model_constituents_t),    intent(in)  :: this
      real(kind_phys),                     intent(out) :: const_array(:,:,:)
      logical,          optional,          intent(in)  :: advected
      integer,          optional,          intent(out) :: errflg
      character(len=*), optional,          intent(out) :: errmsg
      ! Local variables
      integer                     :: index      ! <this> const_metadata index
      integer                     :: cindex     ! const_array index
      integer                     :: fld_ind    ! const field index
      integer                     :: max_cind   ! Size of const_array
      integer                     :: num_levels ! Levels of const_array
      character(len=64)           :: std_name
      character(len=*), parameter :: subname = "ccp_model_const_copy_in_3d"

      if (this%locked(errflg=errflg, errmsg=errmsg, warn_func=subname)) then
         cindex = 0
         max_cind = SIZE(const_array, 3)
         num_levels = SIZE(const_array, 2)
         do index = 1, SIZE(this%const_metadata)
            if (this%is_match(index, advected=advected)) then
               ! See if we have room for another constituent
               cindex = cindex + 1
               if (cindex > max_cind) then
                  if (present(errflg)) then
                     errflg = 1
                  end if
                  if (present(errmsg)) then
                     write(errmsg, *) subname,                                &
                          ": Too many constituents for <const_array>"
                  end if
                  exit
               end if
               ! Copy this constituent's field data to <const_array>
               fld_ind = this%const_metadata(index)%field_index()
               if (fld_ind < 1) then
                  if (present(errflg)) then
                     errflg = 1
                  end if
                  if (present(errmsg)) then
                     call this%const_metadata(index)%standard_name(std_name)
                     write(errmsg, '(4a,i0,a,i0)') subname,                   &
                          ": No field index for '", trim(std_name), "'"
                  end if
               else if (this%const_metadata(index)%is_layer_var()) then
                  if (this%num_layers == num_levels) then
                     const_array(:,:,cindex) = this%vars_layer(:,:,fld_ind)
                  else
                     if (present(errflg)) then
                        errflg = 1
                     end if
                     if (present(errmsg)) then
                        call this%const_metadata(index)%standard_name(std_name)
                        write(errmsg, '(4a,i0,a,i0)') subname,                &
                             ": Wrong number of vertical levels for ",        &
                             trim(std_name), ', ', num_levels,                &
                             ', expected ', this%num_layers
                     end if
                     exit
                  end if
               else if (this%const_metadata(index)%is_interface_var()) then
                  if (this%num_interfaces == num_levels) then
                     const_array(:,:,cindex) = this%vars_interface(:,:,fld_ind)
                  else
                     if (present(errflg)) then
                        errflg = 1
                     end if
                     if (present(errmsg)) then
                        call this%const_metadata(index)%standard_name(std_name)
                        write(errmsg, '(4a,i0,a,i0)') subname,                &
                             ": Wrong number of vertical levels for ",        &
                             std_name, ', ', num_levels, ', expected ',       &
                             this%num_interfaces
                     end if
                     exit
                  end if
               end if
            end if
         end do
      end if

   end subroutine ccp_model_const_copy_in_3d

   !########################################################################

   subroutine ccp_model_const_copy_out_3d(this, const_array, advected,        &
        errflg, errmsg)
      ! Update constituent fields matching pattern
      ! Each (optional) property which is present represents something
      !    which is required as part of a match.
      ! <this> must be locked to execute this function

      ! Dummy argument
      class(ccpp_model_constituents_t), intent(inout) :: this
      real(kind_phys),                  intent(in)    :: const_array(:,:,:)
      logical,          optional,       intent(in)    :: advected
      integer,          optional,       intent(out)   :: errflg
      character(len=*), optional,       intent(out)   :: errmsg
      ! Local variables
      integer                     :: index      ! <this> const_metadata index
      integer                     :: cindex     ! const_array index
      integer                     :: fld_ind    ! const field index
      integer                     :: max_cind   ! Size of const_array
      integer                     :: num_levels ! Levels of const_array
      character(len=64)           :: std_name
      character(len=*), parameter :: subname = "ccp_model_const_copy_out_3d"

      if (this%locked(errflg=errflg, errmsg=errmsg, warn_func=subname)) then
         cindex = 0
         max_cind = SIZE(const_array, 3)
         num_levels = SIZE(const_array, 2)
         do index = 1, SIZE(this%const_metadata)
            if (this%is_match(index, advected=advected)) then
               ! See if we have room for another constituent
               cindex = cindex + 1
               if (cindex > max_cind) then
                  if (present(errflg)) then
                     errflg = 1
                  end if
                  if (present(errmsg)) then
                     write(errmsg, *) subname,                                &
                          ": Too many constituents for <const_array>"
                  end if
                  exit
               end if
               ! Copy this field of to <const_array> to constituent's field data
               fld_ind = this%const_metadata(index)%field_index()
               if (fld_ind < 1) then
                  if (present(errflg)) then
                     errflg = 1
                  end if
                  if (present(errmsg)) then
                     call this%const_metadata(index)%standard_name(std_name)
                     write(errmsg, '(4a,i0,a,i0)') subname,                   &
                          ": No field index for '", trim(std_name), "'"
                  end if
               else if (this%const_metadata(index)%is_layer_var()) then
                  if (this%num_layers == num_levels) then
                     this%vars_layer(:,:,fld_ind) = const_array(:,:,cindex)
                  else
                     if (present(errflg)) then
                        errflg = 1
                     end if
                     if (present(errmsg)) then
                        call this%const_metadata(index)%standard_name(std_name)
                        write(errmsg, '(4a,i0,a,i0)') subname,                &
                             ": Wrong number of vertical levels for ",        &
                             trim(std_name), ', ', num_levels,                &
                             ', expected ', this%num_layers
                     end if
                     exit
                  end if
               else if (this%const_metadata(index)%is_interface_var()) then
                  if (this%num_interfaces == num_levels) then
                     this%vars_interface(:,:,fld_ind) = const_array(:,:,cindex)
                  else
                     if (present(errflg)) then
                        errflg = 1
                     end if
                     if (present(errmsg)) then
                        call this%const_metadata(index)%standard_name(std_name)
                        write(errmsg, '(4a,i0,a,i0)') subname,                &
                             ": Wrong number of vertical levels for ",        &
                             std_name, ', ', num_levels, ', expected ',       &
                             this%num_interfaces
                     end if
                     exit
                  end if
               end if
            end if
         end do
      end if

   end subroutine ccp_model_const_copy_out_3d

   !########################################################################

   integer function ccp_model_const_index(this, standard_name, errflg, errmsg)
      ! Return index of metadata matching <standard_name>.
      ! <this> must be locked to execute this function

      ! Dummy arguments
      class(ccpp_model_constituents_t), intent(in)  :: this
      character(len=*),                 intent(in)  :: standard_name
      integer,          optional,       intent(out) :: errflg
      character(len=*), optional,       intent(out) :: errmsg
      ! Local variables
      type(ccpp_constituent_properties_t), pointer  :: cprop
      character(len=*), parameter :: subname = "ccp_model_const_index"

      if (this%locked(errflg=errflg, errmsg=errmsg, warn_func=subname)) then
         cprop => this%find_const(standard_name, errflg=errflg, errmsg=errmsg)
         if (associated(cprop)) then
            ccp_model_const_index = cprop%const_index()
         else
            ccp_model_const_index = int_unassigned
         end if
      else
         ccp_model_const_index = int_unassigned
      end if

   end function ccp_model_const_index

   !########################################################################

   integer function ccp_model_const_field_index(this, standard_name,          &
        errflg, errmsg)
      ! Return index of field matching <standard_name>.
      ! <this> must be locked to execute this function

      ! Dummy arguments
      class(ccpp_model_constituents_t), intent(in)  :: this
      character(len=*),                 intent(in)  :: standard_name
      integer,          optional,       intent(out) :: errflg
      character(len=*), optional,       intent(out) :: errmsg
      ! Local variables
      type(ccpp_constituent_properties_t), pointer  :: cprop
      character(len=*), parameter :: subname = "ccp_model_field_index"

      if (this%locked(errflg=errflg, errmsg=errmsg, warn_func=subname)) then
         cprop => this%find_const(standard_name, errflg=errflg, errmsg=errmsg)
         if (associated(cprop)) then
            ccp_model_const_field_index = cprop%field_index()
         else
            ccp_model_const_field_index = int_unassigned
         end if
      else
         ccp_model_const_field_index = int_unassigned
      end if

   end function ccp_model_const_field_index

   !########################################################################

   subroutine ccp_model_const_metadata(this, standard_name, const_data,       &
        errflg, errmsg)
      ! Return metadata matching standard name
      ! <this> must be locked to execute this function

      ! Dummy arguments
      class(ccpp_model_constituents_t),    intent(in)  :: this
      character(len=*),                    intent(in)  :: standard_name
      type(ccpp_constituent_properties_t), intent(out) :: const_data
      integer,          optional,          intent(out) :: errflg
      character(len=*), optional,          intent(out) :: errmsg
      ! Local variables
      type(ccpp_constituent_properties_t), pointer  :: cprop
      character(len=*), parameter :: subname = "ccp_model_const_metadata"

      if (this%locked(errflg=errflg, errmsg=errmsg, warn_func=subname)) then
         cprop => this%find_const(standard_name, errflg=errflg, errmsg=errmsg)
         if (associated(cprop)) then
            const_data = cprop
         end if
      end if

   end subroutine ccp_model_const_metadata

end module ccpp_constituent_prop_mod
