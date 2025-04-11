module ccpp_scheme_utils

   ! Module of utilities available to CCPP schemes

   use ccpp_constituent_prop_mod, only: ccpp_model_constituents_t, int_unassigned

   implicit none
   private

   !! Public interfaces
   public :: ccpp_initialize_constituent_ptr ! Used by framework to initialize
   public :: ccpp_constituent_index          ! Lookup index constituent by name
   public :: ccpp_constituent_indices        ! Lookup indices of consitutents by name

   !! Private module variables & interfaces

   ! initialized set to .true. once hash table pointer is initialized
   logical                                  :: initialized = .false.
   type(ccpp_model_constituents_t), pointer :: constituent_obj => NULL()

   private :: check_initialization
   private :: status_ok

contains

   subroutine check_initialization(caller, errcode, errmsg)
      ! Dummy arguments
      character(len=*),           intent(in)  :: caller
      integer,          optional, intent(out) :: errcode
      character(len=*), optional, intent(out) :: errmsg

      if (initialized) then
         if (present(errcode)) then
            errcode = 0
         end if
         if (present(errmsg)) then
            errmsg = ''
         end if
      else
         if (present(errcode)) then
            errcode = 1
         end if
         if (present(errmsg)) then
            errmsg = trim(caller)//' FAILED, module not initialized'
         end if
      end if
   end subroutine check_initialization

   logical function status_ok(errcode)
      ! Dummy argument
      integer, optional, intent(in) :: errcode

      if (present(errcode)) then
         status_ok = (errcode == 0) .and. initialized
      else
         status_ok = initialized
      end if

   end function status_ok

   subroutine ccpp_initialize_constituent_ptr(const_obj)
      ! Dummy arguments
      type(ccpp_model_constituents_t), pointer, intent(in) :: const_obj

      if (.not. initialized) then
         constituent_obj => const_obj
         initialized = .true.
      end if
   end subroutine ccpp_initialize_constituent_ptr

   subroutine ccpp_constituent_index(standard_name, const_index, errcode, errmsg)
      ! Dummy arguments
      character(len=*),           intent(in)  :: standard_name
      integer,                    intent(out) :: const_index
      integer,          optional, intent(out) :: errcode
      character(len=*), optional, intent(out) :: errmsg

      ! Local variable
      character(len=*), parameter :: subname = 'ccpp_constituent_index'

      call check_initialization(caller=subname, errcode=errcode, errmsg=errmsg)
      if (status_ok(errcode)) then
         call constituent_obj%const_index(const_index, standard_name,         &
              errcode, errmsg)
      else
         const_index = int_unassigned
      end if
   end subroutine ccpp_constituent_index

   subroutine ccpp_constituent_indices(standard_names, const_inds, errcode, errmsg)
      ! Dummy arguments
      character(len=*),           intent(in)  :: standard_names(:)
      integer,                    intent(out) :: const_inds(:)
      integer,          optional, intent(out) :: errcode
      character(len=*), optional, intent(out) :: errmsg

      ! Local variables
      integer                     :: indx
      character(len=*), parameter :: subname = 'ccpp_constituent_indices'

      const_inds = int_unassigned
      call check_initialization(caller=subname, errcode=errcode, errmsg=errmsg)
      if (status_ok(errcode)) then
         if (size(const_inds) < size(standard_names)) then
            errcode = 1
            write(errmsg, '(3a)') subname, ": const_inds array too small. ", &
                 "Must be greater than or equal to the size of standard_names"
         else
            do indx = 1, size(standard_names)
               ! For each std name in <standard_names>, find the const. index
               call constituent_obj%const_index(const_inds(indx),             &
                    standard_names(indx), errcode, errmsg)
               if (errcode /= 0) then
                  exit
               end if
            end do
         end if
      end if
   end subroutine ccpp_constituent_indices

end module ccpp_scheme_utils
