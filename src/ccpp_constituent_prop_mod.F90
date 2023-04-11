module ccpp_constituent_prop_mod
      use vert_coord,                only: pver

   ! ccpp_contituent_prop_mod contains types and procedures for storing
   ! and retrieving constituent properties

   use ccpp_hashable,   only: ccpp_hashable_t, ccpp_hashable_char_t
   use ccpp_hash_table, only: ccpp_hash_table_t, ccpp_hash_iterator_t
   use ccpp_kinds,      only: kind_phys

   implicit none
   private

   !!XXgoldyXX: Implement "last_error" method so that functions do not
   !!           need to have output variables.

   ! Private module data
   integer,         parameter :: stdname_len = 256
   integer,         parameter :: dimname_len = 32
   integer,         parameter :: errmsg_len = 256
   integer,         parameter :: dry_mixing_ratio = -2
   integer,         parameter :: moist_mixing_ratio = -3
   integer,         parameter :: wet_mixing_ratio = -4
   integer,         parameter :: mass_mixing_ratio = -5
   integer,         parameter :: volume_mixing_ratio = -6
   integer,         parameter :: number_concentration = -7
   integer,         parameter :: int_unassigned = -HUGE(1)
   real(kind_phys), parameter :: kphys_unassigned = HUGE(1.0_kind_phys)

   type, public, extends(ccpp_hashable_char_t) :: ccpp_constituent_properties_t
      ! A ccpp_constituent_properties_t object holds relevant metadata
      !   for a constituent species and provides interfaces to access that data.
      character(len=:), private, allocatable :: var_std_name
      character(len=:), private, allocatable :: var_long_name
      character(len=:), private, allocatable :: var_units
      character(len=:), private, allocatable :: vert_dim
      integer,          private              :: const_ind = int_unassigned
      logical,          private              :: advected = .false.
      logical,          private              :: initialized_in_physics = .false.
      ! While the quantities below can be derived from the standard name,
      !    this implementation avoids string searching in parameterizations
      ! const_type distinguishes mass, volume, and number conc. mixing ratios
      integer,          private              :: const_type = int_unassigned
      ! const_water distinguishes dry, moist, and "wet" mixing ratios
      integer,          private              :: const_water = int_unassigned
      ! minimum_mr is the minimum allowed value (default zero)
      real(kind_phys),  private              :: min_val = 0.0_kind_phys
      ! molar_mass is the molecular weight of the constituent (g mol-1)
      real(kind_phys),  private              :: molar_mass = kphys_unassigned
   contains
      ! Required hashable method
      procedure :: key => ccp_properties_get_key
      ! Informational methods
      procedure :: is_instantiated           => ccp_is_instantiated
      procedure :: is_initialized_in_physics => ccp_is_initialized_in_physics
      procedure :: standard_name             => ccp_get_standard_name
      procedure :: long_name                 => ccp_get_long_name
      procedure :: is_layer_var              => ccp_is_layer_var
      procedure :: is_interface_var          => ccp_is_interface_var
      procedure :: is_2d_var                 => ccp_is_2d_var
      procedure :: vertical_dimension        => ccp_get_vertical_dimension
      procedure :: const_index               => ccp_const_index
      procedure :: is_advected               => ccp_is_advected
      procedure :: equivalent                => ccp_is_equivalent
      procedure :: is_mass_mixing_ratio      => ccp_is_mass_mixing_ratio
      procedure :: is_volume_mixing_ratio    => ccp_is_volume_mixing_ratio
      procedure :: is_number_concentration   => ccp_is_number_concentration
      procedure :: is_dry                    => ccp_is_dry
      procedure :: is_moist                  => ccp_is_moist
      procedure :: is_wet                    => ccp_is_wet
      procedure :: minimum                   => ccp_min_val
      procedure :: molec_weight              => ccp_molec_weight
      ! Copy method (be sure to update this anytime fields are added)
      procedure :: copyConstituent
      generic :: assignment(=) => copyConstituent
      ! Methods that change state (XXgoldyXX: make private?)
      procedure :: instantiate     => ccp_instantiate
      procedure :: deallocate      => ccp_deallocate
      procedure :: set_const_index => ccp_set_const_index
   end type ccpp_constituent_properties_t

!! \section arg_table_ccpp_constituent_prop_ptr_t
!! \htmlinclude ccpp_constituent_prop_ptr_t.html
!!
   type, public :: ccpp_constituent_prop_ptr_t
      type(ccpp_constituent_properties_t), private, pointer :: prop => NULL()
   contains
      ! Informational methods
      procedure :: standard_name             => ccpt_get_standard_name
      procedure :: long_name                 => ccpt_get_long_name
      procedure :: is_layer_var              => ccpt_is_layer_var
      procedure :: is_interface_var          => ccpt_is_interface_var
      procedure :: is_2d_var                 => ccpt_is_2d_var
      procedure :: vertical_dimension        => ccpt_get_vertical_dimension
      procedure :: const_index               => ccpt_const_index
      procedure :: is_advected               => ccpt_is_advected
      procedure :: is_mass_mixing_ratio      => ccpt_is_mass_mixing_ratio
      procedure :: is_volume_mixing_ratio    => ccpt_is_volume_mixing_ratio
      procedure :: is_number_concentration   => ccpt_is_number_concentration
      procedure :: is_dry                    => ccpt_is_dry
      procedure :: is_moist                  => ccpt_is_moist
      procedure :: is_wet                    => ccpt_is_wet
      procedure :: is_initialized_in_physics => ccpt_is_initialized_in_physics
      procedure :: minimum                   => ccpt_min_val
      procedure :: molec_weight              => ccpt_molec_weight
      ! ccpt_set: Set the internal pointer
      procedure :: set                     => ccpt_set
      ! Methods that change state (XXgoldyXX: make private?)
      procedure :: deallocate      => ccpt_deallocate
      procedure :: set_const_index => ccpt_set_const_index
   end type ccpp_constituent_prop_ptr_t

!! \section arg_table_ccpp_model_constituents_t
!! \htmlinclude ccpp_model_constituents_t.html
!!
   type, public :: ccpp_model_constituents_t
      ! A ccpp_model_constituents_t object holds all the metadata and field
      !   data for a model run's constituents along with data and methods
      !   to initialize and access the data.
      !!XXgoldyXX: To do: allow accessor functions as CCPP local variable
      !!                  names so that members can be private.
      integer                          :: num_layer_vars = 0
      integer                          :: num_advected_vars = 0
      integer,                 private :: num_layers = 0
      type(ccpp_hash_table_t), private :: hash_table
      logical,                 private :: table_locked = .false.
      logical,                 private :: data_locked = .false.
      ! These fields are public to allow for efficient (i.e., no copying)
      !   usage even though it breaks object independence
      real(kind_phys), allocatable     :: vars_layer(:,:,:)
      real(kind_phys), allocatable     :: vars_minvalue(:)
      ! An array containing all the constituent metadata
      ! Each element contains a pointer to a constituent from the hash table
      type(ccpp_constituent_prop_ptr_t), allocatable :: const_metadata(:)
   contains
      ! Return .true. if a constituent matches pattern
      procedure, private :: is_match => ccp_model_const_is_match
      ! Return a constituent from the hash table
      procedure, private :: find_const => ccp_model_const_find_const
      ! Are both the properties table and data array locked (i.e., ready to be used)?
      procedure :: locked => ccp_model_const_locked
      ! Is the properties table locked (i.e., ready to be used)?
      procedure :: const_props_locked => ccp_model_const_props_locked
      ! Is the data array locked (i.e., ready to be used)?
      procedure :: const_data_locked => ccp_model_const_data_locked
      ! Is it okay to add new metadata fields?
      procedure :: okay_to_add => ccp_model_const_okay_to_add
      ! Add a constituent's metadata to the master hash table
      procedure :: new_field => ccp_model_const_add_metadata
      ! Initialize hash table
      procedure :: initialize_table => ccp_model_const_initialize
      ! Freeze hash table and set constituents properties
      procedure :: lock_table => ccp_model_const_table_lock
      ! Freeze and initialize constituent field arrays
      procedure :: lock_data => ccp_model_const_data_lock
      ! Empty (reset) the entire object
      procedure :: reset => ccp_model_const_reset
      ! Query number of constituents matching pattern
      procedure :: num_constituents => ccp_model_const_num_match
      ! Return index of constituent matching standard name
      procedure :: const_index => ccp_model_const_index
      ! Return metadata matching standard name
      procedure :: field_metadata => ccp_model_const_metadata
      ! Gather constituent fields matching pattern
      procedure :: copy_in => ccp_model_const_copy_in_3d
      ! Update constituent fields matching pattern
      procedure :: copy_out => ccp_model_const_copy_out_3d
      ! Return pointer to constituent array (for use by host model)
      procedure :: field_data_ptr => ccp_field_data_ptr
      ! Return pointer to advected constituent array (for use by host model)
      procedure :: advected_constituents_ptr => ccp_advected_data_ptr
      ! Return pointer to constituent properties array (for use by host model)
      procedure :: constituent_props_ptr => ccp_constituent_props_ptr
   end type ccpp_model_constituents_t

   ! Private interfaces
   private to_str
   private initialize_errvars
   private set_errvars
   private handle_allocate_error
   private check_var_bounds

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

      outConst%var_std_name = inConst%var_std_name
      outConst%var_long_name = inConst%var_long_name
      outConst%vert_dim = inConst%vert_dim
      outConst%const_ind = inConst%const_ind
      outConst%advected = inConst%advected
      outConst%initialized_in_physics = inConst%initialized_in_physics
      outConst%const_type = inConst%const_type
      outConst%const_water = inConst%const_water
      outConst%min_val = inConst%min_val
   end subroutine copyConstituent

   !#######################################################################

   character(len=10) function to_str(val)
      ! return default integer as a left justified string

      ! Dummy argument
      integer, intent(in) :: val

      write(to_str,'(i0)') val

   end function to_str

   !#######################################################################

   subroutine initialize_errvars(errcode, errmsg)
      ! Initialize error variables, if present

      ! Dummy arguments
      integer,          optional, intent(out) :: errcode
      character(len=*), optional, intent(out) :: errmsg

      if (present(errcode)) then
         errcode = 0
      end if
      if (present(errmsg)) then
         errmsg = ''
      end if
   end subroutine initialize_errvars

   !#######################################################################

   subroutine set_errvars(errcode_val, errmsg_val, errcode, errmsg,           &
        errmsg2, errmsg3, errmsg4, errmsg5)
      ! Set error variables, if present

      ! Dummy arguments
      integer,          optional, intent(in)    :: errcode_val
      character(len=*), optional, intent(in)    :: errmsg_val
      integer,          optional, intent(inout) :: errcode
      character(len=*), optional, intent(inout) :: errmsg
      character(len=*), optional, intent(in)    :: errmsg2
      character(len=*), optional, intent(in)    :: errmsg3
      character(len=*), optional, intent(in)    :: errmsg4
      character(len=*), optional, intent(in)    :: errmsg5
      ! Local variable
      integer :: emsg_len

      if (present(errcode)) then
         errcode = errcode + errcode_val
      end if
      if (present(errmsg)) then
         emsg_len = len_trim(errmsg)
         if (emsg_len > 0) then
            errmsg(emsg_len+1:) = '; '
         end if
         emsg_len = len_trim(errmsg)
         errmsg(emsg_len+1:) = trim(errmsg_val)
         if (present(errmsg2)) then
            emsg_len = len_trim(errmsg)
            errmsg(emsg_len+1:) = trim(errmsg2)
         end if
         if (present(errmsg3)) then
            emsg_len = len_trim(errmsg)
            errmsg(emsg_len+1:) = trim(errmsg3)
         end if
         if (present(errmsg4)) then
            emsg_len = len_trim(errmsg)
            errmsg(emsg_len+1:) = trim(errmsg4)
         end if
         if (present(errmsg5)) then
            emsg_len = len_trim(errmsg)
            errmsg(emsg_len+1:) = trim(errmsg5)
         end if
      end if
   end subroutine set_errvars

   !#######################################################################

   subroutine handle_allocate_error(astat, fieldname, errcode, errmsg)
      ! Generate an error message if <astat> indicates an allocation failure

      ! Dummy arguments
      integer,                    intent(in)  :: astat
      character(len=*),           intent(in)  :: fieldname
      integer,          optional, intent(out) :: errcode
      character(len=*), optional, intent(out) :: errmsg

      call initialize_errvars(errcode, errmsg)
      if (astat /= 0) then
         call set_errvars(astat, "Error allocating ", errcode=errcode,        &
              errmsg=errmsg, errmsg2="ccpp_constituent_properties_t",         &
              errmsg3="object component, "//trim(fieldname),                  &
              errmsg4=", error code = ", errmsg5=to_str(astat))
      end if

   end subroutine handle_allocate_error

   !#######################################################################

   subroutine check_var_bounds(var, var_bound, varname, errcode, errmsg)
      ! Generate an error message if <astat> indicates an allocation failure

      ! Dummy arguments
      integer,                    intent(in)  :: var
      integer,                    intent(in)  :: var_bound
      character(len=*),           intent(in)  :: varname
      integer,          optional, intent(out) :: errcode
      character(len=*), optional, intent(out) :: errmsg

      call initialize_errvars(errcode, errmsg)
      if (var > var_bound) then
         call set_errvars(1, trim(varname)//" exceeds its upper bound, ",     &
              errcode=errcode, errmsg=errmsg, errmsg2=to_str(var_bound))
      end if
   end subroutine check_var_bounds

   !#######################################################################

   function ccp_properties_get_key(hashable)
      ! Return the constituent properties class key (var_std_name)

      ! Dummy arguments
      class(ccpp_constituent_properties_t), intent(in) :: hashable
      character(len=:), allocatable                    :: ccp_properties_get_key

      ccp_properties_get_key = hashable%var_std_name

   end function ccp_properties_get_key

   !#######################################################################

   logical function ccp_is_instantiated(this, errcode, errmsg)
      ! Return .true. iff <this> is instantiated
      ! If <this> is *not* instantiated and <errcode> and/or <errmsg> is present,
      !    fill these fields with an error status
      ! If <this> *is* instantiated and <errcode> and/or <errmsg> is present,
      !    clear these fields.

      ! Dummy arguments
      class(ccpp_constituent_properties_t), intent(in)  :: this
      integer,          optional,           intent(out) :: errcode
      character(len=*), optional,           intent(out) :: errmsg

      ccp_is_instantiated = allocated(this%var_std_name)
      call initialize_errvars(errcode, errmsg)
      if (.not. ccp_is_instantiated) then
         call set_errvars(1, "ccpp_constituent_properties_t object ",         &
              errcode=errcode, errmsg=errmsg, errmsg2="is not initialized")
      end if

   end function ccp_is_instantiated

   !#######################################################################

   subroutine ccp_instantiate(this, std_name, long_name, units, vertical_dim,  &
        advected, initialized_in_physics, errcode, errmsg)
      ! Initialize all fields in <this>

      ! Dummy arguments
      class(ccpp_constituent_properties_t), intent(inout) :: this
      character(len=*),                     intent(in)    :: std_name
      character(len=*),                     intent(in)    :: long_name
      character(len=*),                     intent(in)    :: units
      character(len=*),                     intent(in)    :: vertical_dim
      logical, optional,                    intent(in)    :: advected
      logical, optional,                    intent(in)    :: initialized_in_physics
      integer,                              intent(out)   :: errcode
      character(len=*),                     intent(out)   :: errmsg
      ! Local variable
      integer :: astat

      if (this%is_instantiated()) then
         errcode = 1
         write(errmsg, *) 'ccpp_constituent_properties_t object, ',        &
              trim(std_name), ', is already initialized as ', this%var_std_name
      else
         errcode = 0
         errmsg = ''
         this%var_std_name = trim(std_name)
      end if
      if (errcode == 0) then
         this%var_long_name = trim(long_name)
         this%var_units = trim(units)
         this%vert_dim = trim(vertical_dim)
         if (present(advected)) then
            this%advected = advected
         else
            this%advected = .false.
         end if
         if (present(initialized_in_physics)) then
            this%initialized_in_physics = initialized_in_physics
         else
            this%initialized_in_physics = .false.
         end if
      end if
      if (errcode == 0) then
         if (index(this%var_std_name, "volume_mixing_ratio") > 0) then
            this%const_type = volume_mixing_ratio
         else if (index(this%var_std_name, "number_concentration") > 0) then
            this%const_type = number_concentration
         else
            this%const_type = mass_mixing_ratio
         end if
         ! Determine if this is a (moist) mixing ratio or volume mixing ratio
      end if
      if (errcode == 0) then
         ! Determine if this mixing ratio is dry, moist, or "wet".
         if (index(this%var_std_name, "wrt_moist_air") > 0) then
            this%const_water = moist_mixing_ratio
         else if (this%var_std_name == "specific_humidity") then
            this%const_water = moist_mixing_ratio
         else if (this%var_std_name == "wrt_total_mass") then
            this%const_water = wet_mixing_ratio
         else
            this%const_water = dry_mixing_ratio
         end if
      end if
      if (errcode /= 0) then
         call this%deallocate()
      end if
   end subroutine ccp_instantiate

   !#######################################################################

   subroutine ccp_deallocate(this)
      ! Deallocate memory associated with this constituent property object

      ! Dummy argument
      class(ccpp_constituent_properties_t), intent(inout) :: this

      if (allocated(this%var_std_name)) then
         deallocate(this%var_std_name)
      end if
      if (allocated(this%var_long_name)) then
         deallocate(this%var_long_name)
      end if
      if (allocated(this%vert_dim)) then
         deallocate(this%vert_dim)
      end if
      this%const_ind = int_unassigned
      this%advected = .false.
      this%const_type = int_unassigned
      this%const_water = int_unassigned

   end subroutine ccp_deallocate

   !#######################################################################

   subroutine ccp_get_standard_name(this, std_name, errcode, errmsg)
      ! Return this constituent's standard name

      ! Dummy arguments
      class(ccpp_constituent_properties_t), intent(in)  :: this
      character(len=*),                     intent(out) :: std_name
      integer,          optional,           intent(out) :: errcode
      character(len=*), optional,           intent(out) :: errmsg

      if (this%is_instantiated(errcode, errmsg)) then
         std_name = this%var_std_name
      else
         std_name = ''
      end if

   end subroutine ccp_get_standard_name

   !#######################################################################

   subroutine ccp_get_long_name(this, long_name, errcode, errmsg)
      ! Return this constituent's long name (description)

      ! Dummy arguments
      class(ccpp_constituent_properties_t), intent(in)  :: this
      character(len=*),                     intent(out) :: long_name
      integer,          optional,           intent(out) :: errcode
      character(len=*), optional,           intent(out) :: errmsg

      if (this%is_instantiated(errcode, errmsg)) then
         long_name = this%var_long_name
      else
         long_name = ''
      end if

   end subroutine ccp_get_long_name

   !#######################################################################

   subroutine ccp_get_vertical_dimension(this, vert_dim, errcode, errmsg)
      ! Return the standard name of this constituent's vertical dimension

      ! Dummy arguments
      class(ccpp_constituent_properties_t), intent(in)  :: this
      character(len=*),                     intent(out) :: vert_dim
      integer,          optional,           intent(out) :: errcode
      character(len=*), optional,           intent(out) :: errmsg

      if (this%is_instantiated(errcode, errmsg)) then
         vert_dim = this%vert_dim
      else
         vert_dim = ''
      end if

   end subroutine ccp_get_vertical_dimension

   !#######################################################################

   logical function ccp_is_layer_var(this) result(is_layer)
      ! Return .true. iff this constituent has a layer vertical dimension

      ! Dummy arguments
      class(ccpp_constituent_properties_t), intent(in)  :: this
      ! Local variable
      character(len=dimname_len) :: dimname

      call this%vertical_dimension(dimname)
      is_layer = trim(dimname) == 'vertical_layer_dimension'

   end function ccp_is_layer_var

   !#######################################################################

   logical function ccp_is_interface_var(this) result(is_interface)
      ! Return .true. iff this constituent has a interface vertical dimension

      ! Dummy arguments
      class(ccpp_constituent_properties_t), intent(in)  :: this
      ! Local variable
      character(len=dimname_len) :: dimname

      call this%vertical_dimension(dimname)
      is_interface = trim(dimname) == 'vertical_interface_dimension'

   end function ccp_is_interface_var

   !#######################################################################

   logical function ccp_is_2d_var(this) result(is_2d)
      ! Return .true. iff this constituent has a 2d vertical dimension

      ! Dummy arguments
      class(ccpp_constituent_properties_t), intent(in)  :: this
      ! Local variable
      character(len=dimname_len) :: dimname

      call this%vertical_dimension(dimname)
      is_2d = len_trim(dimname) == 0

   end function ccp_is_2d_var

   !#######################################################################

   integer function ccp_const_index(this, errcode, errmsg)
      ! Return this constituent's array index (or -1 of not assigned)

      ! Dummy arguments
      class(ccpp_constituent_properties_t), intent(in)  :: this
      integer,          optional,           intent(out) :: errcode
      character(len=*), optional,           intent(out) :: errmsg

      if (this%is_instantiated(errcode, errmsg)) then
         ccp_const_index = this%const_ind
      else
         ccp_const_index = int_unassigned
      end if

   end function ccp_const_index

   !#######################################################################

   subroutine ccp_set_const_index(this, index, errcode, errmsg)
      ! Set this constituent's index in the master constituent array
      ! It is an error to try to set an index if it is already set

      ! Dummy arguments
      class(ccpp_constituent_properties_t), intent(inout) :: this
      integer,                              intent(in)    :: index
      integer,          optional,           intent(out)   :: errcode
      character(len=*), optional,           intent(out)   :: errmsg

      if (this%is_instantiated(errcode, errmsg)) then
         if (this%const_ind == int_unassigned) then
            this%const_ind = index
         else
            call set_errvars(1, "ccpp_constituent_properties_t ",             &
                 errcode=errcode, errmsg=errmsg,                              &
                 errmsg2="const index is already set")
         end if
      end if

   end subroutine ccp_set_const_index

   !#######################################################################

   subroutine ccp_is_advected(this, val_out, errcode, errmsg)

      ! Dummy arguments
      class(ccpp_constituent_properties_t), intent(in)  :: this
      logical,                              intent(out) :: val_out
      integer,          optional,           intent(out) :: errcode
      character(len=*), optional,           intent(out) :: errmsg

      if (this%is_instantiated(errcode, errmsg)) then
         val_out = this%advected
      else
         val_out = .false.
      end if
   end subroutine ccp_is_advected

   !#######################################################################

   subroutine ccp_is_initialized_in_physics(this, val_out, errcode, errmsg)

      ! Dummy arguments
      class(ccpp_constituent_properties_t), intent(in)  :: this
      logical,                              intent(out) :: val_out
      integer,          optional,           intent(out) :: errcode
      character(len=*), optional,           intent(out) :: errmsg

      if (this%is_instantiated(errcode, errmsg)) then
         val_out = this%initialized_in_physics
      else
         val_out = .false.
      end if
   end subroutine ccp_is_initialized_in_physics

   !#######################################################################

   subroutine ccp_is_equivalent(this, oconst, equiv, errcode, errmsg)

      ! Dummy arguments
      class(ccpp_constituent_properties_t), intent(in)  :: this
      type(ccpp_constituent_properties_t),  intent(in)  :: oconst
      logical,                              intent(out) :: equiv
      integer,          optional,           intent(out) :: errcode
      character(len=*), optional,           intent(out) :: errmsg

      if (this%is_instantiated(errcode, errmsg) .and.                          &
           oconst%is_instantiated(errcode, errmsg)) then
         equiv = (trim(this%var_std_name) == trim(oconst%var_std_name)) .and. &
              (trim(this%var_long_name) == trim(oconst%var_long_name))  .and. &
              (trim(this%vert_dim) == trim(oconst%vert_dim))            .and. &
              (this%advected .eqv. oconst%advected)
      else
         equiv = .false.
      end if

   end subroutine ccp_is_equivalent

   !########################################################################

   subroutine ccp_is_mass_mixing_ratio(this, val_out, errcode, errmsg)

      ! Dummy arguments
      class(ccpp_constituent_properties_t), intent(in)  :: this
      logical,                              intent(out) :: val_out
      integer,                              intent(out) :: errcode
      character(len=*),                     intent(out) :: errmsg

      if (this%is_instantiated(errcode, errmsg)) then
         val_out = this%const_type == mass_mixing_ratio
      else
         val_out = .false.
      end if
   end subroutine ccp_is_mass_mixing_ratio

   !########################################################################

   subroutine ccp_is_volume_mixing_ratio(this, val_out, errcode, errmsg)

      ! Dummy arguments
      class(ccpp_constituent_properties_t), intent(in)  :: this
      logical,                              intent(out) :: val_out
      integer,                              intent(out) :: errcode
      character(len=*),                     intent(out) :: errmsg

      if (this%is_instantiated(errcode, errmsg)) then
         val_out = this%const_type == volume_mixing_ratio
      else
         val_out = .false.
      end if
   end subroutine ccp_is_volume_mixing_ratio

   !########################################################################

   subroutine ccp_is_number_concentration(this, val_out, errcode, errmsg)

      ! Dummy arguments
      class(ccpp_constituent_properties_t), intent(in)  :: this
      logical,                              intent(out) :: val_out
      integer,                              intent(out) :: errcode
      character(len=*),                     intent(out) :: errmsg

      if (this%is_instantiated(errcode, errmsg)) then
         val_out = this%const_type == number_concentration
      else
         val_out = .false.
      end if
   end subroutine ccp_is_number_concentration

   !########################################################################

   subroutine ccp_is_dry(this, val_out, errcode, errmsg)

      ! Dummy arguments
      class(ccpp_constituent_properties_t), intent(in)  :: this
      logical,                              intent(out) :: val_out
      integer,                              intent(out) :: errcode
      character(len=*),                     intent(out) :: errmsg

      if (this%is_instantiated(errcode, errmsg)) then
         val_out = this%const_water == dry_mixing_ratio
      else
         val_out = .false.
      end if

   end subroutine ccp_is_dry

   !########################################################################

   subroutine ccp_is_moist(this, val_out, errcode, errmsg)

      ! Dummy arguments
      class(ccpp_constituent_properties_t), intent(in)  :: this
      logical,                              intent(out) :: val_out
      integer,                              intent(out) :: errcode
      character(len=*),                     intent(out) :: errmsg

      if (this%is_instantiated(errcode, errmsg)) then
         val_out = this%const_water == moist_mixing_ratio
      else
         val_out = .false.
      end if

   end subroutine ccp_is_moist

   !########################################################################

   subroutine ccp_is_wet(this, val_out, errcode, errmsg)

      ! Dummy arguments
      class(ccpp_constituent_properties_t), intent(in)  :: this
      logical,                              intent(out) :: val_out
      integer,                              intent(out) :: errcode
      character(len=*),                     intent(out) :: errmsg

      if (this%is_instantiated(errcode, errmsg)) then
         val_out = this%const_water == wet_mixing_ratio
      else
         val_out = .false.
      end if

   end subroutine ccp_is_wet

   !########################################################################

   subroutine ccp_min_val(this, val_out, errcode, errmsg)

      ! Dummy arguments
      class(ccpp_constituent_properties_t), intent(in)  :: this
      real(kind_phys),                      intent(out) :: val_out
      integer,                              intent(out) :: errcode
      character(len=*),                     intent(out) :: errmsg

      if (this%is_instantiated(errcode, errmsg)) then
         val_out = this%min_val
      else
         val_out = kphys_unassigned
      end if

   end subroutine ccp_min_val

   !########################################################################

   subroutine ccp_molec_weight(this, val_out, errcode, errmsg)

      ! Dummy arguments
      class(ccpp_constituent_properties_t), intent(in)  :: this
      real(kind_phys),                      intent(out) :: val_out
      integer,                              intent(out) :: errcode
      character(len=*),                     intent(out) :: errmsg

      if (this%is_instantiated(errcode, errmsg)) then
         val_out = this%molar_mass
      else
         val_out = kphys_unassigned
      end if

   end subroutine ccp_molec_weight

   !########################################################################
   !
   ! CCPP_MODEL_CONSTITUENTS_T (constituent field data) methods
   !
   !########################################################################

   logical function ccp_model_const_locked(this, errcode, errmsg, warn_func)
      ! Return .true. iff <this> is locked (i.e., ready to use)
      ! Optionally fill out <errcode> and <errmsg> if object not initialized

      ! Dummy arguments
      class(ccpp_model_constituents_t), intent(in)  :: this
      integer,          optional,       intent(out) :: errcode
      character(len=*), optional,       intent(out) :: errmsg
      character(len=*), optional,       intent(in)  :: warn_func
      ! Local variable
      character(len=*), parameter :: subname = 'ccp_model_const_locked'

      call initialize_errvars(errcode, errmsg)
      ccp_model_const_locked = .false.
      ! Use an initialized hash table as double check
      if (this%hash_table%is_initialized()) then
         ccp_model_const_locked = this%table_locked .and. this%data_locked
         if ( (.not. (this%table_locked .and. this%data_locked)) .and.                                 &
              present(errmsg) .and. present(warn_func)) then
            ! Write a warning as a courtesy to calling function but do not set
            !   errcode (let caller decide).
            write(errmsg, *) trim(warn_func),                                 &
                 ' WARNING: Model constituents not ready to use'
         end if
      else
         if (present(warn_func)) then
            call set_errvars(1, trim(warn_func),                              &
                 errcode=errcode, errmsg=errmsg,                              &
                 errmsg2=" WARNING: Model constituents not initialized")
         else
            call set_errvars(1, subname,                                      &
                 errcode=errcode, errmsg=errmsg,                              &
                 errmsg2=" WARNING: Model constituents not initialized")
         end if
      end if

   end function ccp_model_const_locked

   !########################################################################

   logical function ccp_model_const_props_locked(this, errcode, errmsg, warn_func)
      ! Return .true. iff <this>'s constituent properties are ready to use
      ! Optionally fill out <errcode> and <errmsg> if object not initialized
      ! Dummy arguments
      class(ccpp_model_constituents_t), intent(in)  :: this
      integer,          optional,       intent(out) :: errcode
      character(len=*), optional,       intent(out) :: errmsg
      character(len=*), optional,       intent(in)  :: warn_func
      ! Local variable
      character(len=*), parameter :: subname = 'ccp_model_const_table_locked'

      call initialize_errvars(errcode, errmsg)
      ccp_model_const_props_locked = .false.
      ! Use an initialized hash table as double check
      if (this%hash_table%is_initialized()) then
         ccp_model_const_props_locked = this%table_locked
         if ( .not. this%table_locked .and.                                 &
              present(errmsg) .and. present(warn_func)) then
            ! Write a warning as a courtesy to calling function but do not set
            !   errcode (let caller decide).
            write(errmsg, *) trim(warn_func),                                 &
                 ' WARNING: Model constituent properties not ready to use'
         end if
      else
         if (present(warn_func)) then
            call set_errvars(1, trim(warn_func),                              &
                 errcode=errcode, errmsg=errmsg,                              &
                 errmsg2=" WARNING: Model constituent properties not initialized")
         else
            call set_errvars(1, subname,                                      &
                 errcode=errcode, errmsg=errmsg,                              &
                 errmsg2=" WARNING: Model constituent properties not initialized")
         end if
      end if

   end function ccp_model_const_props_locked

   !########################################################################

   logical function ccp_model_const_data_locked(this, errcode, errmsg, warn_func)
      ! Return .true. iff <this>'s data are ready to use
      ! Optionally fill out <errcode> and <errmsg> if object not initialized
      ! Dummy arguments
      class(ccpp_model_constituents_t), intent(in)  :: this
      integer,          optional,       intent(out) :: errcode
      character(len=*), optional,       intent(out) :: errmsg
      character(len=*), optional,       intent(in)  :: warn_func
      ! Local variable
      character(len=*), parameter :: subname = 'ccp_model_const_data_locked'

      call initialize_errvars(errcode, errmsg)
      ccp_model_const_data_locked = .false.
      ! Use an initialized hash table as double check
      if (this%hash_table%is_initialized()) then
         ccp_model_const_data_locked = this%data_locked
         if ( .not. this%data_locked .and.                                 &
              present(errmsg) .and. present(warn_func)) then
            ! Write a warning as a courtesy to calling function but do not set
            !   errcode (let caller decide).
            write(errmsg, *) trim(warn_func),                                 &
                 ' WARNING: Model constituent data not ready to use'
         end if
      else
         if (present(warn_func)) then
            call set_errvars(1, trim(warn_func),                              &
                 errcode=errcode, errmsg=errmsg,                              &
                 errmsg2=" WARNING: Model constituent data not initialized")
         else
            call set_errvars(1, subname,                                      &
                 errcode=errcode, errmsg=errmsg,                              &
                 errmsg2=" WARNING: Model constituent data not initialized")
         end if
      end if

   end function ccp_model_const_data_locked

   !########################################################################

   logical function ccp_model_const_okay_to_add(this, errcode, errmsg,        &
        warn_func)
      ! Return .true. iff <this> is initialized and not locked
      ! Optionally fill out <errcode> and <errmsg> if the conditions
      !    are not met.

      ! Dummy arguments
      class(ccpp_model_constituents_t),    intent(inout) :: this
      integer,          optional,          intent(out)   :: errcode
      character(len=*), optional,          intent(out)   :: errmsg
      character(len=*), optional,          intent(in)    :: warn_func
      ! Local variable
      character(len=*), parameter :: subname = 'ccp_model_const_okay_to_add'

      ccp_model_const_okay_to_add = this%hash_table%is_initialized()
      if (ccp_model_const_okay_to_add) then
         ccp_model_const_okay_to_add = .not. (this%const_props_locked(errcode=errcode,     &
              errmsg=errmsg, warn_func=subname) .or. this%const_data_locked(errcode=errcode, &
              errmsg=errmsg, warn_func=subname))
         if (.not. ccp_model_const_okay_to_add) then
            if (present(warn_func)) then
               call set_errvars(1, trim(warn_func),                           &
                    errcode=errcode, errmsg=errmsg,                           &
                    errmsg2=" WARNING: Model constituents are locked")
            else
               call set_errvars(1, subname,                                   &
                    errcode=errcode, errmsg=errmsg,                           &
                    errmsg2=" WARNING: Model constituents are locked")
            end if
         end if
      else
         if (present(warn_func)) then
            call set_errvars(1, trim(warn_func),                              &
                 errcode=errcode, errmsg=errmsg,                              &
                 errmsg2=" WARNING: Model constituents not initialized")
         else
            call set_errvars(1, subname,                                      &
                 errcode=errcode, errmsg=errmsg,                              &
                 errmsg2=" WARNING: Model constituents not initialized")
         end if
      end if

   end function ccp_model_const_okay_to_add

   !########################################################################

   subroutine ccp_model_const_add_metadata(this, field_data, errcode, errmsg)
      ! Add a constituent's metadata to the master hash table

      ! Dummy arguments
      class(ccpp_model_constituents_t),              intent(inout) :: this
      type(ccpp_constituent_properties_t), target,   intent(in)    :: field_data
      integer,                             optional, intent(out)   :: errcode
      character(len=*),                    optional, intent(out)   :: errmsg
      ! Local variables
      character(len=errmsg_len)   :: error
      character(len=*), parameter :: subname = 'ccp_model_const_add_metadata'

      if (this%okay_to_add(errcode=errcode, errmsg=errmsg,                    &
           warn_func=subname)) then
         error = ''
!!XXgoldyXX: Add check on key to see if incompatible item already there.
         call this%hash_table%add_hash_key(field_data, error)
         if (len_trim(error) > 0) then
            call set_errvars(1, trim(error), errcode=errcode, errmsg=errmsg)
         else
            ! If we get here we are successful, add to variable count
            if (field_data%is_layer_var()) then
               this%num_layer_vars = this%num_layer_vars + 1
            else
               if (present(errmsg)) then
                  call field_data%vertical_dimension(error,                   &
                       errcode=errcode, errmsg=errmsg)
                  if (len_trim(errmsg) == 0) then
                     call set_errvars(1,                                      &
                          "ERROR: Unknown vertical dimension, '",             &
                          errcode=errcode, errmsg=errmsg,                     &
                          errmsg2=trim(error), errmsg3="'")
                  end if
               end if
            end if
         end if
      else
         call set_errvars(1, subname,                                         &
              errcode=errcode, errmsg=errmsg,                                 &
              errmsg2=" WARNING: Model constituents are locked")
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
      call this%reset()
      ! Figure a log base 2 for initializing hash table
      tbl_size = num_elements * 10 ! Hash padding
      tbl_size = int((log(real(tbl_size, kind_phys)) / log(2.0_kind_phys)) +  &
           1.0_kind_phys)
      ! Initialize hash table
      call this%hash_table%initialize(tbl_size)
      this%table_locked = .false.

   end subroutine ccp_model_const_initialize

   !########################################################################

   function ccp_model_const_find_const(this, standard_name, errcode, errmsg)  &
        result(cprop)
      ! Return a constituent with key, <standard_name>, from the hash table
      ! <this> must be locked to execute this function
      ! Since this is a private function, error checking for locked status
      !    is *not* performed.

      ! Dummy arguments
      class(ccpp_model_constituents_t), intent(in)  :: this
      character(len=*),                 intent(in)  :: standard_name
      integer,          optional,       intent(out) :: errcode
      character(len=*), optional,       intent(out) :: errmsg
      type(ccpp_constituent_properties_t), pointer  :: cprop
      ! Local variables
      class(ccpp_hashable_t), pointer :: hval
      character(len=errmsg_len)       :: error
      character(len=*), parameter     :: subname = 'ccp_model_const_find_const'

      nullify(cprop)
      hval => this%hash_table%table_value(standard_name, errmsg=error)
      if (len_trim(error) > 0) then
         call set_errvars(1, subname, errcode=errcode, errmsg=errmsg,         &
              errmsg2=": "//trim(error))
      else
         select type(hval)
         type is (ccpp_constituent_properties_t)
            cprop => hval
         class default
            call set_errvars(1, subname, errcode=errcode, errmsg=errmsg,      &
                 errmsg2=" ERROR: Bad hash table value",                      &
                 errmsg3=trim(standard_name))
         end select
      end if

   end function ccp_model_const_find_const

   !########################################################################

   subroutine ccp_model_const_table_lock(this, errcode, errmsg)
      ! Freeze hash table and initialize constituent properties

      ! Dummy arguments
      class(ccpp_model_constituents_t), intent(inout) :: this
      integer,                optional, intent(out)   :: errcode
      character(len=*),       optional, intent(out)   :: errmsg
      ! Local variables
      integer                                      :: index_const
      integer                                      :: index_advect
      integer                                      :: num_vars
      integer                                      :: astat
      logical                                      :: check
      type(ccpp_hash_iterator_t)                   :: hiter
      class(ccpp_hashable_t),              pointer :: hval
      type(ccpp_constituent_properties_t), pointer :: cprop
      character(len=dimname_len)                   :: dimname
      character(len=*), parameter :: subname = 'ccp_model_const_table_lock'

      astat = 0
      if (this%const_props_locked(errcode=errcode, errmsg=errmsg, warn_func=subname)) then
         call set_errvars(1, subname, errcode=errcode, errmsg=errmsg,         &
              errmsg2=" WARNING: Model constituent properites already locked, ignoring")
         astat = astat + 1
      else
         ! Make sure everything is really initialized
         call this%reset(clear_hash_table=.false.)
         this%num_advected_vars = 0
         ! Allocate the constituent array
         num_vars = this%hash_table%num_values()
         allocate(this%const_metadata(num_vars), stat=astat)
         call handle_allocate_error(astat, 'const_metadata',                  &
              errcode=errcode, errmsg=errmsg)
         ! We want to pack the advected constituents at the beginning of
         !   the field array so we need to know how many there are
         if (astat == 0) then
            call hiter%initialize(this%hash_table)
            do
               if (hiter%valid()) then
                  hval => hiter%value()
                  select type(hval)
                  type is (ccpp_constituent_properties_t)
                     cprop => hval
                     call cprop%is_advected(check)
                     if (check) then
                        this%num_advected_vars = this%num_advected_vars + 1
                     end if
                  end select
                  call hiter%next()
               else
                  exit
               end if
            end do
            call hiter%finalize()
            ! Sanity check on num_advect
            if (this%num_advected_vars > num_vars) then
               astat = 1
               call set_errvars(astat, subname,                               &
                    errcode=errcode, errmsg=errmsg,                           &
                    errmsg2=" ERROR: num_advected_vars index out of bounds")
               astat = astat + 1
            end if
         end if
         index_advect = 0
         index_const = this%num_advected_vars
         ! Iterate through the hash table to find entries
         if (astat == 0) then
            call hiter%initialize(this%hash_table)
            do
               if (hiter%valid()) then
                  hval => hiter%value()
                  select type(hval)
                  type is (ccpp_constituent_properties_t)
                     cprop => hval
                     call cprop%is_advected(check)
                     if (check) then
                        index_advect = index_advect + 1
                        if (index_advect > this%num_advected_vars) then
                           call set_errvars(1, subname,                       &
                                errcode=errcode, errmsg=errmsg,               &
                                errmsg2=" ERROR: const a index out of bounds")
                           astat = astat + 1
                           exit
                        end if
                        call cprop%set_const_index(index_advect,              &
                             errcode=errcode, errmsg=errmsg)
                        call this%const_metadata(index_advect)%set(cprop)
                     else
                        index_const = index_const + 1
                        if (index_const > num_vars) then
                           call set_errvars(1, subname,                       &
                                errcode=errcode, errmsg=errmsg,               &
                                errmsg2=" ERROR: const v index out of bounds")
                           astat = astat + 1
                           exit
                        end if
                        call cprop%set_const_index(index_const,               &
                             errcode=errcode, errmsg=errmsg)
                        call this%const_metadata(index_const)%set(cprop)
                     end if
                     ! Make sure this is a layer variable
                     if (.not. cprop%is_layer_var()) then
                        call cprop%vertical_dimension(dimname,                &
                             errcode=errcode, errmsg=errmsg)
                        call set_errvars(1, subname,                          &
                             errcode=errcode, errmsg=errmsg,                  &
                             errmsg2=" ERROR: Bad vertical dimension, '",     &
                             errmsg3=trim(dimname))
                        astat = astat + 1
                        exit
                     end if
                  class default
                     call set_errvars(1, subname,                             &
                          errcode=errcode, errmsg=errmsg,                     &
                          errmsg2="ERROR: Bad hash table value")
                     astat = astat + 1
                     exit
                  end select
                  call hiter%next()
               else
                  exit
               end if
            end do
            call hiter%finalize()
            ! Some size sanity checks
            if (index_const /= this%hash_table%num_values()) then
               call set_errvars(errcode + 1, subname,                         &
                    errcode=errcode, errmsg=errmsg,                           &
                    errmsg2=" ERROR: Too few constituents found in hash table")
               astat = astat + 1
            end if
            if (index_advect /= this%num_advected_vars) then
               call set_errvars(1, subname,                                   &
                    errcode=errcode, errmsg=errmsg,                           &
                    errmsg2=" ERROR: Too few advected constituents found ",   &
                    errmsg3="in hash table")
               astat = astat + 1
            end if
            if (present(errcode)) then
               if (errcode /= 0) then
                  astat = 1
               end if
            end if
            if (astat == 0) then
               this%table_locked = .true.
            end if
         end if
      end if

   end subroutine ccp_model_const_table_lock

   !########################################################################

   subroutine ccp_model_const_data_lock(this, ncols, num_layers, errcode, errmsg)
      ! Freeze hash table and initialize constituent arrays

      ! Dummy arguments
      class(ccpp_model_constituents_t), intent(inout) :: this
      integer,                          intent(in)    :: ncols
      integer,                          intent(in)    :: num_layers
      integer,                optional, intent(out)   :: errcode
      character(len=*),       optional, intent(out)   :: errmsg
      ! Local variables
      integer                                      :: astat
      character(len=*), parameter :: subname = 'ccp_model_const_data_lock'

      if (this%const_data_locked(errcode=errcode, errmsg=errmsg, warn_func=subname)) then
         call set_errvars(1, subname, errcode=errcode, errmsg=errmsg, &
              errmsg2=" WARNING: Model constituent data already locked, ignoring")
         astat = astat + 1
      else if (.not. this%const_props_locked(errcode=errcode, errmsg=errmsg, &
                warn_func=subname)) then
         call set_errvars(1, subname, errcode=errcode, errmsg=errmsg, &
              errmsg2=" WARNING: Model constituent properties not yet locked, ignoring")
         astat = astat + 1
      else
         allocate(this%vars_layer(ncols, num_layers, this%hash_table%num_values()),         &
              stat=astat)
         call handle_allocate_error(astat, 'vars_layer',                   &
              errcode=errcode, errmsg=errmsg)
         if (astat == 0) then
            allocate(this%vars_minvalue(this%hash_table%num_values()), stat=astat)
            call handle_allocate_error(astat, 'vars_minvalue',             &
                 errcode=errcode, errmsg=errmsg)
         end if
         if (astat == 0) then
            this%num_layers = num_layers
            this%vars_layer = kphys_unassigned
            this%vars_minvalue = 0.0_kind_phys
         end if
         if (present(errcode)) then
            if (errcode /= 0) then
               astat = 1
            end if
         end if
         if (astat == 0) then
            this%data_locked = .true.
         end if
      end if

   end subroutine ccp_model_const_data_lock

   !########################################################################

   subroutine ccp_model_const_reset(this, clear_hash_table)
      ! Empty (reset) the entire object
      ! Optionally do not clear the hash table (and its data)

      ! Dummy arguments
      class(ccpp_model_constituents_t), intent(inout) :: this
      logical, optional,                intent(in)    :: clear_hash_table
      ! Local variables
      logical :: clear_table
      integer :: index

      if (present(clear_hash_table)) then
         clear_table = clear_hash_table
      else
         clear_table = .true.
      end if
      if (allocated(this%vars_layer)) then
         deallocate(this%vars_layer)
      end if
      if (allocated(this%vars_minvalue)) then
         deallocate(this%vars_minvalue)
      end if
      if (allocated(this%const_metadata)) then
         if (clear_table) then
            do index = 1, size(this%const_metadata, 1)
               call this%const_metadata(index)%deallocate()
            end do
         end if
         deallocate(this%const_metadata)
      end if
      if (clear_table) then
         this%num_layer_vars = 0
         this%num_advected_vars = 0
         this%num_layers = 0
         call this%hash_table%clear()
      end if

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
      ! Local variable
      logical :: check

      ! By default, every constituent is a match
      is_match = .true.
      if (present(advected)) then
         call this%const_metadata(index)%is_advected(check)
         if (advected .neqv. check) then
            is_match = .false.
         end if
      end if

   end function ccp_model_const_is_match

   !########################################################################

   subroutine ccp_model_const_num_match(this, nmatch, advected, errcode, errmsg)
      ! Query number of constituents matching pattern
      ! Each (optional) property which is present represents something
      !    which is required as part of a match.
      ! <this> must be locked to execute this function

      ! Dummy arguments
      class(ccpp_model_constituents_t), intent(in)  :: this
      integer,                          intent(out) :: nmatch
      logical,          optional,       intent(in)  :: advected
      integer,          optional,       intent(out) :: errcode
      character(len=*), optional,       intent(out) :: errmsg
      ! Local variables
      integer                     :: index
      character(len=*), parameter :: subname = "ccp_model_const_num_match"

      nmatch = 0
      if (this%const_props_locked(errcode=errcode, errmsg=errmsg, warn_func=subname)) then
         do index = 1, SIZE(this%const_metadata)
            if (this%is_match(index, advected=advected)) then
               nmatch = nmatch + 1
            end if
         end do
      end if

   end subroutine ccp_model_const_num_match

   !########################################################################

   subroutine ccp_model_const_index(this, index, standard_name, errcode, errmsg)
      ! Return index of metadata matching <standard_name>.
      ! <this> must be locked to execute this function

      ! Dummy arguments
      class(ccpp_model_constituents_t), intent(in)  :: this
      character(len=*),                 intent(in)  :: standard_name
      integer,                          intent(out) :: index
      integer,          optional,       intent(out) :: errcode
      character(len=*), optional,       intent(out) :: errmsg
      ! Local variables
      type(ccpp_constituent_properties_t), pointer  :: cprop
      character(len=*), parameter :: subname = "ccp_model_const_index"

      if (this%const_props_locked(errcode=errcode, errmsg=errmsg, warn_func=subname)) then
         cprop => this%find_const(standard_name, errcode=errcode, errmsg=errmsg)
         if (associated(cprop)) then
            index = cprop%const_index()
         else
            index = int_unassigned
         end if
      else
         index = int_unassigned
      end if

   end subroutine ccp_model_const_index

   !########################################################################

   subroutine ccp_model_const_metadata(this, standard_name, const_data,       &
        errcode, errmsg)
      ! Return metadata matching standard name
      ! <this> must be locked to execute this function

      ! Dummy arguments
      class(ccpp_model_constituents_t),    intent(in)  :: this
      character(len=*),                    intent(in)  :: standard_name
      type(ccpp_constituent_properties_t), intent(out) :: const_data
      integer,                   optional, intent(out) :: errcode
      character(len=*),          optional, intent(out) :: errmsg
      ! Local variables
      type(ccpp_constituent_properties_t), pointer     :: cprop
      character(len=*), parameter :: subname = "ccp_model_const_metadata"

      if (this%const_props_locked(errcode=errcode, errmsg=errmsg, warn_func=subname)) then
         cprop => this%find_const(standard_name, errcode=errcode, errmsg=errmsg)
         if (associated(cprop)) then
            const_data = cprop
         end if
      end if

   end subroutine ccp_model_const_metadata

   !########################################################################

   subroutine ccp_model_const_copy_in_3d(this, const_array, advected,         &
        errcode, errmsg)
      ! Gather constituent fields matching pattern
      ! Each (optional) property which is present represents something
      !    which is required as part of a match.
      ! <this> must be locked to execute this function

      ! Dummy arguments
      class(ccpp_model_constituents_t),    intent(in)  :: this
      real(kind_phys),                     intent(out) :: const_array(:,:,:)
      logical,          optional,          intent(in)  :: advected
      integer,          optional,          intent(out) :: errcode
      character(len=*), optional,          intent(out) :: errmsg
      ! Local variables
      integer                     :: index      ! <this> const_metadata index
      integer                     :: cindex     ! const_array index
      integer                     :: fld_ind    ! const field index
      integer                     :: max_cind   ! Size of const_array
      integer                     :: num_levels ! Levels of const_array
      character(len=stdname_len)  :: std_name
      character(len=*), parameter :: subname = "ccp_model_const_copy_in_3d"

      if (this%locked(errcode=errcode, errmsg=errmsg, warn_func=subname)) then
         cindex = 0
         max_cind = SIZE(const_array, 3)
         num_levels = SIZE(const_array, 2)
         do index = 1, SIZE(this%const_metadata)
            if (this%is_match(index, advected=advected)) then
               ! See if we have room for another constituent
               cindex = cindex + 1
               if (cindex > max_cind) then
                  call set_errvars(1, subname,                                &
                       errcode=errcode, errmsg=errmsg,                        &
                       errmsg2=": Too many constituents for <const_array>")
                  exit
               end if
               ! Copy this constituent's field data to <const_array>
               call this%const_metadata(index)%const_index(fld_ind)
               if (fld_ind /= index) then
                  call this%const_metadata(index)%standard_name(std_name)
                  call set_errvars(1, subname//": ERROR: ",                   &
                       errcode=errcode, errmsg=errmsg,                        &
                       errmsg2="bad field index, "//to_str(fld_ind),          &
                       errmsg3=" for '"//trim(std_name)//"', ",               &
                       errmsg4="should have been "//to_str(index))
                  exit
               else if (this%const_metadata(index)%is_layer_var()) then
                  if (this%num_layers == num_levels) then
                     const_array(:,:,cindex) = this%vars_layer(:,:,fld_ind)
                  else
                     call this%const_metadata(index)%standard_name(std_name)
                     call set_errvars(1, subname,                             &
                          errcode=errcode, errmsg=errmsg,                     &
                          errmsg2=": Wrong number of vertical levels for '",  &
                          errmsg3=trim(std_name)//"', "//to_str(num_levels),  &
                          errmsg4=", expected"//to_str(this%num_layers))
                     exit
                  end if
               else
                  call this%const_metadata(index)%standard_name(std_name)
                  call set_errvars(1, subname//": Unsupported var type, ",    &
                       errcode=errcode, errmsg=errmsg,                        &
                       errmsg2="wrong number of vertical levels for '",       &
                       errmsg3=trim(std_name)//"', "//to_str(num_levels),     &
                       errmsg4=", expected"//to_str(this%num_layers))
                  exit
               end if
            end if
         end do
      end if

   end subroutine ccp_model_const_copy_in_3d

   !########################################################################

   subroutine ccp_model_const_copy_out_3d(this, const_array, advected,        &
        errcode, errmsg)
      ! Update constituent fields matching pattern
      ! Each (optional) property which is present represents something
      !    which is required as part of a match.
      ! <this> must be locked to execute this function

      ! Dummy argument
      class(ccpp_model_constituents_t), intent(inout) :: this
      real(kind_phys),                  intent(in)    :: const_array(:,:,:)
      logical,          optional,       intent(in)    :: advected
      integer,          optional,       intent(out)   :: errcode
      character(len=*), optional,       intent(out)   :: errmsg
      ! Local variables
      integer                     :: index      ! <this> const_metadata index
      integer                     :: cindex     ! const_array index
      integer                     :: fld_ind    ! const field index
      integer                     :: max_cind   ! Size of const_array
      integer                     :: num_levels ! Levels of const_array
      character(len=stdname_len)  :: std_name
      character(len=*), parameter :: subname = "ccp_model_const_copy_out_3d"

      if (this%locked(errcode=errcode, errmsg=errmsg, warn_func=subname)) then
         cindex = 0
         max_cind = SIZE(const_array, 3)
         num_levels = SIZE(const_array, 2)
         do index = 1, SIZE(this%const_metadata)
            if (this%is_match(index, advected=advected)) then
               ! See if we have room for another constituent
               cindex = cindex + 1
               if (cindex > max_cind) then
                  call set_errvars(1, subname,                                &
                       errcode=errcode, errmsg=errmsg,                        &
                       errmsg2=": Too many constituents for <const_array>")
                  exit
               end if
               ! Copy this field of to <const_array> to constituent's field data
               call this%const_metadata(index)%const_index(fld_ind)
               if (fld_ind /= index) then
                  call this%const_metadata(index)%standard_name(std_name)
                  call set_errvars(1, subname//": ERROR: ",                   &
                       errcode=errcode, errmsg=errmsg,                        &
                       errmsg2="bad field index, "//to_str(fld_ind),          &
                       errmsg3=" for '"//trim(std_name)//"', ",               &
                       errmsg4="should have been "//to_str(index))
                  exit
               else if (this%const_metadata(index)%is_layer_var()) then
                  if (this%num_layers == num_levels) then
                     this%vars_layer(:,:,fld_ind) = const_array(:,:,cindex)
                  else
                     call this%const_metadata(index)%standard_name(std_name)
                     call set_errvars(1, subname,                             &
                          errcode=errcode, errmsg=errmsg,                     &
                          errmsg2=": Wrong number of vertical levels for '",  &
                          errmsg3=trim(std_name)//"', "//to_str(num_levels),  &
                          errmsg4=", expected"//to_str(this%num_layers))
                     exit
                  end if
               else
                  call this%const_metadata(index)%standard_name(std_name)
                  call set_errvars(1, subname//": Unsupported var type, ",    &
                       errcode=errcode, errmsg=errmsg,                        &
                       errmsg2="wrong number of vertical levels for '",       &
                       errmsg3=trim(std_name)//"', "//to_str(num_levels),     &
                       errmsg4=", expected"//to_str(this%num_layers))
                  exit
               end if
            end if
         end do
      end if

   end subroutine ccp_model_const_copy_out_3d

   !########################################################################

   function ccp_field_data_ptr(this) result(const_ptr)
      ! Return pointer to constituent array (for use by host model)

      ! Dummy arguments
      class(ccpp_model_constituents_t), target, intent(inout) :: this
      real(kind_phys),                  pointer              :: const_ptr(:,:,:)
      ! Local variables
      integer                     :: errcode
      character(len=errmsg_len)   :: errmsg
      character(len=*), parameter :: subname = 'ccp_field_data_ptr'

      if (this%locked(errcode=errcode, errmsg=errmsg, warn_func=subname)) then
         const_ptr => this%vars_layer
      else
         ! We don't want output variables in a function so just nullify
         ! See note above about creating a 'last_error' method
         nullify(const_ptr)
      end if

   end function ccp_field_data_ptr

   !########################################################################

   function ccp_advected_data_ptr(this) result(const_ptr)
      ! Return pointer to advected constituent array (for use by host model)

      ! Dummy arguments
      class(ccpp_model_constituents_t), target, intent(inout) :: this
      real(kind_phys), pointer                               :: const_ptr(:,:,:)
      ! Local variables
      integer                     :: errcode
      character(len=errmsg_len)   :: errmsg
      character(len=*), parameter :: subname = 'ccp_advected_data_ptr'

      if (this%locked(errcode=errcode, errmsg=errmsg, warn_func=subname)) then
         const_ptr => this%vars_layer(:,:,1:this%num_advected_vars)
      else
         ! We don't want output variables in a function so just nullify
         ! See note above about creating a 'last_error' method
         nullify(const_ptr)
      end if

   end function ccp_advected_data_ptr

   function ccp_constituent_props_ptr(this) result(const_ptr)
      ! Return pointer to constituent properties array (for use by host model)

      ! Dummy arguments
      class(ccpp_model_constituents_t),  target, intent(inout) :: this
      type(ccpp_constituent_prop_ptr_t), pointer               :: const_ptr(:)
      ! Local variables
      integer                     :: errcode
      character(len=errmsg_len)   :: errmsg
      character(len=*), parameter :: subname = 'ccp_constituent_props_ptr'

      if (this%const_props_locked(errcode=errcode, errmsg=errmsg, warn_func=subname)) then
         const_ptr => this%const_metadata
      else
         ! We don't want output variables in a function so just nullify
         ! See note above about creating a 'last_error' method
         nullify(const_ptr)
      end if

   end function ccp_constituent_props_ptr

   !########################################################################

   !#####################################
   ! ccpp_constituent_prop_ptr_t methods
   !#####################################

   !#######################################################################

   subroutine ccpt_get_standard_name(this, std_name, errcode, errmsg)
      ! Return this constituent's standard name

      ! Dummy arguments
      class(ccpp_constituent_prop_ptr_t), intent(in)  :: this
      character(len=*),                   intent(out) :: std_name
      integer,          optional,         intent(out) :: errcode
      character(len=*), optional,         intent(out) :: errmsg
      ! Local variable
      character(len=*), parameter :: subname = 'ccpt_get_standard_name'

      if (associated(this%prop)) then
         call this%prop%standard_name(std_name, errcode, errmsg)
      else
         std_name = ''
         call set_errvars(1, subname//": invalid constituent pointer",        &
              errcode=errcode, errmsg=errmsg)
      end if

   end subroutine ccpt_get_standard_name

   !#######################################################################

   subroutine ccpt_get_long_name(this, long_name, errcode, errmsg)
      ! Return this constituent's long name (description)

      ! Dummy arguments
      class(ccpp_constituent_prop_ptr_t), intent(in)  :: this
      character(len=*),                   intent(out) :: long_name
      integer,          optional,         intent(out) :: errcode
      character(len=*), optional,         intent(out) :: errmsg
      ! Local variable
      character(len=*), parameter :: subname = 'ccpt_get_long_name'

      if (associated(this%prop)) then
         call this%prop%long_name(long_name, errcode, errmsg)
      else
         long_name = ''
         call set_errvars(1, subname//": invalid constituent pointer",        &
              errcode=errcode, errmsg=errmsg)
      end if

   end subroutine ccpt_get_long_name

   !#######################################################################

   subroutine ccpt_get_vertical_dimension(this, vert_dim, errcode, errmsg)
      ! Return the standard name of this constituent's vertical dimension

      ! Dummy arguments
      class(ccpp_constituent_prop_ptr_t), intent(in)  :: this
      character(len=*),                   intent(out) :: vert_dim
      integer,          optional,         intent(out) :: errcode
      character(len=*), optional,         intent(out) :: errmsg
      ! Local variable
      character(len=*), parameter :: subname = 'ccpt_get_vertical_dimension'

      if (associated(this%prop)) then
         if (this%prop%is_instantiated(errcode, errmsg)) then
            vert_dim = this%prop%vert_dim
         end if
      else
         vert_dim = ''
         call set_errvars(1, subname//": invalid constituent pointer",        &
              errcode=errcode, errmsg=errmsg)
      end if

   end subroutine ccpt_get_vertical_dimension

   !#######################################################################

   logical function ccpt_is_layer_var(this) result(is_layer)
      ! Return .true. iff this constituent has a layer vertical dimension

      ! Dummy arguments
      class(ccpp_constituent_prop_ptr_t), intent(in)  :: this
      ! Local variables
      character(len=dimname_len)  :: dimname
      character(len=*), parameter :: subname = 'ccpt_is_layer_var'

      if (associated(this%prop)) then
         call this%prop%vertical_dimension(dimname)
         is_layer = trim(dimname) == 'vertical_layer_dimension'
      else
         is_layer = .false.
      end if

   end function ccpt_is_layer_var

   !#######################################################################

   logical function ccpt_is_interface_var(this) result(is_interface)
      ! Return .true. iff this constituent has a interface vertical dimension

      ! Dummy arguments
      class(ccpp_constituent_prop_ptr_t), intent(in)  :: this
      ! Local variables
      character(len=dimname_len)  :: dimname
      character(len=*), parameter :: subname = 'ccpt_is_interface_var'

      if (associated(this%prop)) then
         call this%prop%vertical_dimension(dimname)
         is_interface = trim(dimname) == 'vertical_interface_dimension'
      else
         is_interface = .false.
      end if

   end function ccpt_is_interface_var

   !#######################################################################

   logical function ccpt_is_2d_var(this) result(is_2d)
      ! Return .true. iff this constituent has a 2d vertical dimension

      ! Dummy arguments
      class(ccpp_constituent_prop_ptr_t), intent(in)  :: this
      ! Local variables
      character(len=dimname_len)  :: dimname
      character(len=*), parameter :: subname = 'ccpt_is_2d_var'

      if (associated(this%prop)) then
         call this%prop%vertical_dimension(dimname)
         is_2d = len_trim(dimname) == 0
      else
         is_2d = .false.
      end if

   end function ccpt_is_2d_var

   !#######################################################################

   subroutine ccpt_const_index(this, index, errcode, errmsg)
      ! Return this constituent's master index (or -1 of not assigned)

      ! Dummy arguments
      class(ccpp_constituent_prop_ptr_t), intent(in)  :: this
      integer,                            intent(out) :: index
      integer,          optional,         intent(out) :: errcode
      character(len=*), optional,         intent(out) :: errmsg
      ! Local variable
      character(len=*), parameter :: subname = 'ccpt_const_index'

      if (associated(this%prop)) then
         index = this%prop%const_index(errcode, errmsg)
      else
         index = int_unassigned
         call set_errvars(1, subname//": invalid constituent pointer",        &
              errcode=errcode, errmsg=errmsg)
      end if

   end subroutine ccpt_const_index

   !#######################################################################

   subroutine ccpt_is_advected(this, val_out, errcode, errmsg)

      ! Dummy arguments
      class(ccpp_constituent_prop_ptr_t), intent(in)  :: this
      logical,                            intent(out) :: val_out
      integer,          optional,         intent(out) :: errcode
      character(len=*), optional,         intent(out) :: errmsg
      ! Local variable
      character(len=*), parameter :: subname = 'ccpt_is_advected'

      if (associated(this%prop)) then
         call this%prop%is_advected(val_out, errcode, errmsg)
      else
         val_out = .false.
         call set_errvars(1, subname//": invalid constituent pointer",        &
              errcode=errcode, errmsg=errmsg)
      end if

   end subroutine ccpt_is_advected

   !########################################################################

   subroutine ccpt_is_mass_mixing_ratio(this, val_out, errcode, errmsg)

      ! Dummy arguments
      class(ccpp_constituent_prop_ptr_t), intent(in)  :: this
      logical,                            intent(out) :: val_out
      integer,                            intent(out) :: errcode
      character(len=*),                   intent(out) :: errmsg
      ! Local variable
      character(len=*), parameter :: subname = 'ccpt_is_mass_mixing_ratio'

      if (associated(this%prop)) then
         call this%prop%is_mass_mixing_ratio(val_out, errcode, errmsg)
      else
         val_out = .false.
         call set_errvars(1, subname//": invalid constituent pointer",        &
              errcode=errcode, errmsg=errmsg)
      end if

   end subroutine ccpt_is_mass_mixing_ratio

   !########################################################################

   subroutine ccpt_is_volume_mixing_ratio(this, val_out, errcode, errmsg)

      ! Dummy arguments
      class(ccpp_constituent_prop_ptr_t), intent(in)  :: this
      logical,                            intent(out) :: val_out
      integer,                            intent(out) :: errcode
      character(len=*),                   intent(out) :: errmsg
      ! Local variable
      character(len=*), parameter :: subname = 'ccpt_is_volume_mixing_ratio'

      if (associated(this%prop)) then
         call this%prop%is_volume_mixing_ratio(val_out, errcode, errmsg)
      else
         val_out = .false.
         call set_errvars(1, subname//": invalid constituent pointer",        &
              errcode=errcode, errmsg=errmsg)
      end if

   end subroutine ccpt_is_volume_mixing_ratio

   !########################################################################

   subroutine ccpt_is_number_concentration(this, val_out, errcode, errmsg)

      ! Dummy arguments
      class(ccpp_constituent_prop_ptr_t), intent(in)  :: this
      logical,                            intent(out) :: val_out
      integer,                            intent(out) :: errcode
      character(len=*),                   intent(out) :: errmsg
      ! Local variable
      character(len=*), parameter :: subname = 'ccpt_is_number_concentration'

      if (associated(this%prop)) then
         call this%prop%is_number_concentration(val_out, errcode, errmsg)
      else
         val_out = .false.
         call set_errvars(1, subname//": invalid constituent pointer",        &
              errcode=errcode, errmsg=errmsg)
      end if

   end subroutine ccpt_is_number_concentration

   !########################################################################

   subroutine ccpt_is_dry(this, val_out, errcode, errmsg)

      ! Dummy arguments
      class(ccpp_constituent_prop_ptr_t), intent(in)  :: this
      logical,                            intent(out) :: val_out
      integer,                            intent(out) :: errcode
      character(len=*),                   intent(out) :: errmsg
      ! Local variable
      character(len=*), parameter :: subname = 'ccpt_is_dry'

      if (associated(this%prop)) then
         call this%prop%is_dry(val_out, errcode, errmsg)
      else
         val_out = .false.
         call set_errvars(1, subname//": invalid constituent pointer",        &
              errcode=errcode, errmsg=errmsg)
      end if

   end subroutine ccpt_is_dry

   !########################################################################

   subroutine ccpt_is_moist(this, val_out, errcode, errmsg)

      ! Dummy arguments
      class(ccpp_constituent_prop_ptr_t), intent(in)  :: this
      logical,                            intent(out) :: val_out
      integer,                            intent(out) :: errcode
      character(len=*),                   intent(out) :: errmsg
      ! Local variable
      character(len=*), parameter :: subname = 'ccpt_is_moist'

      if (associated(this%prop)) then
         call this%prop%is_moist(val_out, errcode, errmsg)
      else
         val_out = .false.
         call set_errvars(1, subname//": invalid constituent pointer",        &
              errcode=errcode, errmsg=errmsg)
      end if

   end subroutine ccpt_is_moist

   !########################################################################

   subroutine ccpt_is_wet(this, val_out, errcode, errmsg)

      ! Dummy arguments
      class(ccpp_constituent_prop_ptr_t), intent(in)  :: this
      logical,                            intent(out) :: val_out
      integer,                            intent(out) :: errcode
      character(len=*),                   intent(out) :: errmsg
      ! Local variable
      character(len=*), parameter :: subname = 'ccpt_is_wet'

      if (associated(this%prop)) then
         call this%prop%is_wet(val_out, errcode, errmsg)
      else
         val_out = .false.
         call set_errvars(1, subname//": invalid constituent pointer",        &
              errcode=errcode, errmsg=errmsg)
      end if

   end subroutine ccpt_is_wet

   !########################################################################

   subroutine ccpt_is_initialized_in_physics(this, val_out, errcode, errmsg)

      ! Dummy arguments
      class(ccpp_constituent_prop_ptr_t), intent(in)  :: this
      logical,                            intent(out) :: val_out
      integer,                            intent(out) :: errcode
      character(len=*),                   intent(out) :: errmsg
      ! Local variable
      character(len=*), parameter :: subname = 'ccpt_is_initialized_in_physics'

      if (associated(this%prop)) then
         call this%prop%is_initialized_in_physics(val_out, errcode, errmsg)
      else
         val_out = .false.
         call set_errvars(1, subname//": invalid constituent pointer",        &
              errcode=errcode, errmsg=errmsg)
      end if

   end subroutine ccpt_is_initialized_in_physics

   !########################################################################

   subroutine ccpt_min_val(this, val_out, errcode, errmsg)

      ! Dummy arguments
      class(ccpp_constituent_prop_ptr_t), intent(in)  :: this
      real(kind_phys),                    intent(out) :: val_out
      integer,                            intent(out) :: errcode
      character(len=*),                   intent(out) :: errmsg
      ! Local variable
      character(len=*), parameter :: subname = 'ccpt_min_val'

      if (associated(this%prop)) then
         call this%prop%minimum(val_out, errcode, errmsg)
      else
         val_out = kphys_unassigned
         call set_errvars(1, subname//": invalid constituent pointer",        &
              errcode=errcode, errmsg=errmsg)
      end if

   end subroutine ccpt_min_val

   !########################################################################

   subroutine ccpt_molec_weight(this, val_out, errcode, errmsg)

      ! Dummy arguments
      class(ccpp_constituent_prop_ptr_t), intent(in)  :: this
      real(kind_phys),                    intent(out) :: val_out
      integer,                            intent(out) :: errcode
      character(len=*),                   intent(out) :: errmsg
      ! Local variable
      character(len=*), parameter :: subname = 'ccpt_molec_weight'

      if (associated(this%prop)) then
         call this%prop%molec_weight(val_out, errcode, errmsg)
      else
         val_out = kphys_unassigned
         call set_errvars(1, subname//": invalid constituent pointer",        &
              errcode=errcode, errmsg=errmsg)
      end if

   end subroutine ccpt_molec_weight

   !########################################################################

   subroutine ccpt_set(this, const_ptr, errcode, errmsg)
      ! Set the pointer to <const_ptr>, however, an error is recorded if
      !    the pointer is already set.

      ! Dummy arguments
      class(ccpp_constituent_prop_ptr_t),            intent(inout) :: this
      type(ccpp_constituent_properties_t), pointer                 :: const_ptr
      integer,                             optional, intent(out)   :: errcode
      character(len=*),                    optional, intent(out)   :: errmsg
      ! Local variables
      character(len=stdname_len) :: stdname
      character(len=errmsg_len)  :: errmsg2

      call initialize_errvars(errcode, errmsg)
      if (associated(this%prop)) then
         call this%standard_name(stdname, errcode=errcode, errmsg=errmsg2)
         if (errcode == 0) then
            write(errmsg2, *) "Pointer already allocated as '",               &
                 trim(stdname), "'"
         end if
         errcode = errcode + 1
         call set_errvars(1, "ccpt_set: ", errcode=errcode, errmsg=errmsg,    &
              errmsg2=trim(errmsg2))
      else
         this%prop => const_ptr
      end if

   end subroutine ccpt_set

   !########################################################################

   subroutine ccpt_deallocate(this)
      ! Deallocate the constituent object pointer if it is allocated.

      ! Dummy argument
      class(ccpp_constituent_prop_ptr_t), intent(inout) :: this

      if (associated(this%prop)) then
         call this%prop%deallocate()
         deallocate(this%prop)
      end if
      nullify(this%prop)

   end subroutine ccpt_deallocate

   !#######################################################################

   subroutine ccpt_set_const_index(this, index, errcode, errmsg)
      ! Set this constituent's index in the master constituent array
      ! It is an error to try to set an index if it is already set

      ! Dummy arguments
      class(ccpp_constituent_prop_ptr_t), intent(inout) :: this
      integer,                              intent(in)    :: index
      integer,          optional,           intent(out)   :: errcode
      character(len=*), optional,           intent(out)   :: errmsg
      ! Local variable
      character(len=*), parameter :: subname = 'ccpt_set_const_index'

      if (associated(this%prop)) then
         if (this%prop%is_instantiated(errcode, errmsg)) then
            if (this%prop%const_ind == int_unassigned) then
               this%prop%const_ind = index
            else
               call set_errvars(1, "ccpp_constituent_prop_ptr_t ",            &
                    errcode=errcode, errmsg=errmsg,                           &
                    errmsg2="const index is already set")
            end if
         end if
      else
         call set_errvars(1, subname//": invalid constituent pointer",        &
              errcode=errcode, errmsg=errmsg)
      end if

   end subroutine ccpt_set_const_index

end module ccpp_constituent_prop_mod
