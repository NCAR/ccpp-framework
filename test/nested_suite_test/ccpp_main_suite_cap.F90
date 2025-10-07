!
! This work (Common Community Physics Package Framework), identified by
! NOAA, NCAR, CU/CIRES, is free of known copyright restrictions and is
! placed in the public domain.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
! THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
! IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
! CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.


!>
!! @brief Auto-generated CCPP Suite Cap for main_suite
!!
!
module ccpp_main_suite_cap

   use ccpp_kinds
   use ccpp_constituent_prop_mod, only: ccpp_constituent_properties_t

   implicit none
   private

   ! Suite interfaces

   character(len=16) :: ccpp_suite_state = 'uninitialized'

   public :: main_suite_register
   public :: main_suite_initialize
   public :: main_suite_timestep_initial
   public :: main_suite_radiation1
   public :: main_suite_rad_lw_group
   public :: main_suite_rad_sw_group
   public :: main_suite_timestep_final
   public :: main_suite_finalize
   ! Public interfaces for handling constituents
   ! Return the number of constituents for this suite
   public :: main_suite_constituents_num_consts
   ! Return the name of a constituent
   public :: main_suite_constituents_const_name
   ! Copy the data for a constituent
   public :: main_suite_constituents_copy_const
   ! Private constituent module data
   logical, private :: ccpp_constituents_initialized = .false.
   ! Private interface for constituents
   private :: ccpp_create_constituent_array

   ! Private suite variables

CONTAINS

   subroutine main_suite_register(errflg, errmsg)


      ! Dummy arguments
      integer,            intent(out)   :: errflg
      character(len=512), intent(out)   :: errmsg

      ! Initialize ccpp error handling
      errflg = 0
      errmsg = ''

      ! Output threaded region check
#ifdef _OPENMP
      if (omp_get_thread_num() > 1) then
         errflg = 1
         errmsg = "Cannot call register routine from a threaded region"
         return
      end if
#endif
      ! Check state machine
      if (trim(ccpp_suite_state) /= 'uninitialized') then
         errflg = 1
         write(errmsg, '(3a)') "Invalid initial CCPP state, '", trim(ccpp_suite_state),           &
              "' in main_suite_register"
         return
      end if
      ! Set horizontal loop extent

      ! Allocate local arrays
      ! Suite state does not change

   end subroutine main_suite_register

   ! ========================================================================


   subroutine main_suite_initialize(errflg, errmsg, scheme_order)

      use effr_calc,    only: effr_calc_init
      use effr_diag,    only: effr_diag_init
      use effr_post,    only: effr_post_init
      use mod_effr_pre, only: effr_pre_init

      ! Dummy arguments
      integer,            intent(out)   :: errflg
      character(len=512), intent(out)   :: errmsg
      integer,            intent(inout) :: scheme_order

      ! Local Variables
      type(integer)                            :: internal_var_integer

      ! Initialize ccpp error handling
      errflg = 0
      errmsg = ''

      ! Output threaded region check
#ifdef _OPENMP
      if (omp_get_thread_num() > 1) then
         errflg = 1
         errmsg = "Cannot call initialize routine from a threaded region"
         return
      end if
#endif
      ! Check state machine
      if (trim(ccpp_suite_state) /= 'uninitialized') then
         errflg = 1
         write(errmsg, '(3a)') "Invalid initial CCPP state, '", trim(ccpp_suite_state),           &
              "' in main_suite_initialize"
         return
      end if
      ! Set horizontal loop extent

      ! Allocate local arrays

      ! Allocate suite_vars

      if (errflg == 0) then
         ! ##################################################################
         ! Begin debug tests
         ! ##################################################################

         ! Assign value of scheme_order to internal_var_integer
         internal_var_integer = scheme_order

         ! ##################################################################
         ! End debug tests
         ! ##################################################################



         ! Call scheme
         call effr_pre_init(scheme_order=scheme_order, errmsg=errmsg, errflg=errflg)



      end if

      if (errflg == 0) then
         ! ##################################################################
         ! Begin debug tests
         ! ##################################################################

         ! Assign value of scheme_order to internal_var_integer
         internal_var_integer = scheme_order

         ! ##################################################################
         ! End debug tests
         ! ##################################################################



         ! Call scheme
         call effr_calc_init(scheme_order=scheme_order, errmsg=errmsg, errflg=errflg)



      end if

      if (errflg == 0) then
         ! ##################################################################
         ! Begin debug tests
         ! ##################################################################

         ! Assign value of scheme_order to internal_var_integer
         internal_var_integer = scheme_order

         ! ##################################################################
         ! End debug tests
         ! ##################################################################



         ! Call scheme
         call effr_post_init(scheme_order=scheme_order, errmsg=errmsg, errflg=errflg)



      end if

      if (errflg == 0) then
         ! ##################################################################
         ! Begin debug tests
         ! ##################################################################

         ! Assign value of scheme_order to internal_var_integer
         internal_var_integer = scheme_order

         ! ##################################################################
         ! End debug tests
         ! ##################################################################



         ! Call scheme
         call effr_diag_init(scheme_order=scheme_order, errmsg=errmsg, errflg=errflg)



      end if
      ccpp_suite_state = 'initialized'

   end subroutine main_suite_initialize

   ! ========================================================================


   subroutine main_suite_timestep_initial(errflg, errmsg)


      ! Dummy arguments
      integer,            intent(out)   :: errflg
      character(len=512), intent(out)   :: errmsg

      ! Initialize ccpp error handling
      errflg = 0
      errmsg = ''

      ! Output threaded region check
#ifdef _OPENMP
      if (omp_get_thread_num() > 1) then
         errflg = 1
         errmsg = "Cannot call timestep_initial routine from a threaded region"
         return
      end if
#endif
      ! Check state machine
      if (trim(ccpp_suite_state) /= 'initialized') then
         errflg = 1
         write(errmsg, '(3a)') "Invalid initial CCPP state, '", trim(ccpp_suite_state),           &
              "' in main_suite_timestep_initial"
         return
      end if
      ! Set horizontal loop extent

      ! Allocate local arrays

      ! Allocate suite_vars
      ccpp_suite_state = 'in_time_step'

   end subroutine main_suite_timestep_initial

   ! ========================================================================


   subroutine main_suite_radiation1(num_subcycles, errflg, errmsg, ncols, pver, effrr_inout,      &
        scalar_var, has_graupel, effrg_in, ncg_in, has_ice, nci_out, effrl_inout, effri_out,      &
        effrs_inout, col_start, col_end, scalar_var1, tke_inout, tke2_inout, scalar_var2,         &
        scalar_var3)

      use effr_calc,    only: effr_calc_run
      use effr_diag,    only: effr_diag_run
      use effr_post,    only: effr_post_run
      use effrs_calc,   only: effrs_calc_run
      use mod_effr_pre, only: effr_pre_run

      ! Dummy arguments
      integer,            intent(in)    :: num_subcycles
      integer,            intent(out)   :: errflg
      character(len=512), intent(out)   :: errmsg
      integer,            intent(in)    :: ncols
      integer,            intent(in)    :: pver
      real(kind_phys),    intent(inout) :: effrr_inout(:,:)
      real(kind_phys),    intent(in)    :: scalar_var
      logical,            intent(in)    :: has_graupel
      real(kind_phys),    intent(in),   target, optional  :: effrg_in(:,:)
      real(kind_phys),    intent(in),   target, optional  :: ncg_in(:,:)
      logical,            intent(in)    :: has_ice
      real(kind_phys),    intent(out),  target, optional  :: nci_out(:,:)
      real(kind_phys),    intent(inout) :: effrl_inout(:,:)
      real(kind_phys),    intent(out),  target, optional  :: effri_out(:,:)
      real(8),            intent(inout) :: effrs_inout(:,:)
      integer,            intent(in)    :: col_start
      integer,            intent(in)    :: col_end
      real(kind_phys),    intent(inout) :: scalar_var1
      real(kind_phys),    intent(inout) :: tke_inout
      real(kind_phys),    intent(inout) :: tke2_inout
      real(kind_phys),    intent(in)    :: scalar_var2
      integer,            intent(in)    :: scalar_var3

      ! Local Variables
      integer                                  :: loop0_num_subcycles_for_effr
      type(real)                               :: internal_var_real
      integer                                  :: loop1
      integer                                  :: loop2
      integer                                  :: ncol
      integer                                  :: internal_var_integer
      real(kind_phys)                          :: internal_var_real_kind_phys
      logical                                  :: internal_var_logical
      real(kind_phys)                          :: scalar_var_local
      real(kind_phys),    allocatable          :: effrr_in_local(:,:)
      real(kind_phys),    allocatable,  target :: effrg_in_local(:,:)
      real(kind_phys),    allocatable          :: effrl_inout_local(:,:)
      real(kind_phys),    allocatable,  target :: effri_out_local(:,:)
      real(8),            allocatable          :: effrs_inout_local(:,:)
      real(kind_phys),        pointer          :: effrg_in_ptr(:,:)    => null()
      real(kind_phys),        pointer          :: ncg_in_ptr(:,:)      => null()
      real(kind_phys),        pointer          :: nci_out_ptr(:,:)     => null()
      real(kind_phys),        pointer          :: effri_out_ptr(:,:)   => null()
      real(kind_phys),        pointer          :: ncl_out_ptr(:,:)     => null()

      ! Initialize ccpp error handling
      errflg = 0
      errmsg = ''

      ! Check state machine
      if (trim(ccpp_suite_state) /= 'in_time_step') then
         errflg = 1
         write(errmsg, '(3a)') "Invalid initial CCPP state, '", trim(ccpp_suite_state),           &
              "' in main_suite_radiation1"
         return
      end if
      ! Set horizontal loop extent
      ncol = col_end - col_start + 1

      ! Allocate local arrays
      allocate(effrg_in_local(1:ncol, 1:pver))
      allocate(effri_out_local(1:ncol, 1:pver))
      allocate(effrl_inout_local(1:ncol, 1:pver))
      allocate(effrr_in_local(1:ncol, 1:pver))
      allocate(effrs_inout_local(1:ncol, 1:pver))
      do loop0_num_subcycles_for_effr = 1, num_subcycles

         if (errflg == 0) then
            ! ##################################################################
            ! Begin debug tests
            ! ##################################################################

            ! Check size of array effrr_inout
            if (size(effrr_inout(:,:)) /= 1*(ncol-1+1)*(pver-1+1)) then
               write(errmsg, '(2(a,i8))')                                                         &
                    'In group main_suite_radiation1 before  effr_pre_run: for array effrr_inout, expected size ',&
                    1*(ncol-1+1)*(pver-1+1), ' but got ', size(effrr_inout)
               errflg = 1
               return
            end if

            ! Check length of effrr_inout(:,1)
            if (size(effrr_inout(:,1)) /= ncol-1+1) then
               write(errmsg, '(2(a,i8))')                                                         &
                    'In group main_suite_radiation1 before effr_pre_run: for array effrr_inout(:,1), expected size ', ncol-1+1,&
                    ' but got ', size(effrr_inout(:,1))
               errflg = 1
               return
            end if
            ! Check length of effrr_inout(1,:)
            if (size(effrr_inout(1,:)) /= pver-1+1) then
               write(errmsg, '(2(a,i8))')                                                         &
                    'In group main_suite_radiation1 before effr_pre_run: for array effrr_inout(1,:), expected size ', pver-1+1,&
                    ' but got ', size(effrr_inout(1,:))
               errflg = 1
               return
            end if

            ! Assign value of scalar_var to internal_var_real
            internal_var_real = scalar_var

            ! ##################################################################
            ! End debug tests
            ! ##################################################################



            ! Call scheme
            call effr_pre_run(effrr_inout=effrr_inout, scalar_var=scalar_var, errmsg=errmsg,      &
                 errflg=errflg)



         end if
         do loop1 = 1, 2
            do loop2 = 1, 2

               if (errflg == 0) then
                  ! ##################################################################
                  ! Begin debug tests
                  ! ##################################################################

                  ! Assign value of pver to internal_var_integer
                  internal_var_integer = pver

                  ! Check size of array effrr_inout
                  if (size(effrr_inout(:,:)) /= 1*(ncol-1+1)*(pver-1+1)) then
                     write(errmsg, '(2(a,i8))')                                                   &
                          'In group main_suite_radiation1 before  effr_calc_run: for array effrr_inout, expected size ',&
                          1*(ncol-1+1)*(pver-1+1), ' but got ', size(effrr_inout)
                     errflg = 1
                     return
                  end if

                  ! Check length of effrr_inout(:,1)
                  if (size(effrr_inout(:,1)) /= ncol-1+1) then
                     write(errmsg, '(2(a,i8))')                                                   &
                          'In group main_suite_radiation1 before effr_calc_run: for array effrr_inout(:,1), expected size ',&
                          ncol-1+1, ' but got ', size(effrr_inout(:,1))
                     errflg = 1
                     return
                  end if
                  ! Check length of effrr_inout(1,:)
                  if (size(effrr_inout(1,:)) /= pver-1+1) then
                     write(errmsg, '(2(a,i8))')                                                   &
                          'In group main_suite_radiation1 before effr_calc_run: for array effrr_inout(1,:), expected size ',&
                          pver-1+1, ' but got ', size(effrr_inout(1,:))
                     errflg = 1
                     return
                  end if

                  if (has_graupel) then
                     ! Check size of array effrg_in
                     if (size(effrg_in(:,:)) /= 1*(ncol-1+1)*(pver-1+1)) then
                        write(errmsg, '(2(a,i8))')                                                &
                             'In group main_suite_radiation1 before  effr_calc_run: for array effrg_in, expected size ',&
                             1*(ncol-1+1)*(pver-1+1), ' but got ', size(effrg_in)
                        errflg = 1
                        return
                     end if
                  end if

                  if (has_graupel) then
                     ! Check length of effrg_in(:,1)
                     if (size(effrg_in(:,1)) /= ncol-1+1) then
                        write(errmsg, '(2(a,i8))')                                                &
                             'In group main_suite_radiation1 before effr_calc_run: for array effrg_in(:,1), expected size ',&
                             ncol-1+1, ' but got ', size(effrg_in(:,1))
                        errflg = 1
                        return
                     end if
                     ! Check length of effrg_in(1,:)
                     if (size(effrg_in(1,:)) /= pver-1+1) then
                        write(errmsg, '(2(a,i8))')                                                &
                             'In group main_suite_radiation1 before effr_calc_run: for array effrg_in(1,:), expected size ',&
                             pver-1+1, ' but got ', size(effrg_in(1,:))
                        errflg = 1
                        return
                     end if
                  end if

                  if (has_graupel) then
                     ! Check size of array ncg_in
                     if (size(ncg_in(:,:)) /= 1*(ncol-1+1)*(pver-1+1)) then
                        write(errmsg, '(2(a,i8))')                                                &
                             'In group main_suite_radiation1 before  effr_calc_run: for array ncg_in, expected size ',&
                             1*(ncol-1+1)*(pver-1+1), ' but got ', size(ncg_in)
                        errflg = 1
                        return
                     end if
                  end if

                  if (has_graupel) then
                     ! Check length of ncg_in(:,1)
                     if (size(ncg_in(:,1)) /= ncol-1+1) then
                        write(errmsg, '(2(a,i8))')                                                &
                             'In group main_suite_radiation1 before effr_calc_run: for array ncg_in(:,1), expected size ',&
                             ncol-1+1, ' but got ', size(ncg_in(:,1))
                        errflg = 1
                        return
                     end if
                     ! Check length of ncg_in(1,:)
                     if (size(ncg_in(1,:)) /= pver-1+1) then
                        write(errmsg, '(2(a,i8))')                                                &
                             'In group main_suite_radiation1 before effr_calc_run: for array ncg_in(1,:), expected size ',&
                             pver-1+1, ' but got ', size(ncg_in(1,:))
                        errflg = 1
                        return
                     end if
                  end if

                  if (has_ice) then
                     ! Check size of array nci_out
                     if (size(nci_out(:,:)) /= 1*(ncol-1+1)*(pver-1+1)) then
                        write(errmsg, '(2(a,i8))')                                                &
                             'In group main_suite_radiation1 before  effr_calc_run: for array nci_out, expected size ',&
                             1*(ncol-1+1)*(pver-1+1), ' but got ', size(nci_out)
                        errflg = 1
                        return
                     end if
                  end if

                  ! Check size of array effrl_inout
                  if (size(effrl_inout(:,:)) /= 1*(ncol-1+1)*(pver-1+1)) then
                     write(errmsg, '(2(a,i8))')                                                   &
                          'In group main_suite_radiation1 before  effr_calc_run: for array effrl_inout, expected size ',&
                          1*(ncol-1+1)*(pver-1+1), ' but got ', size(effrl_inout)
                     errflg = 1
                     return
                  end if

                  ! Check length of effrl_inout(:,1)
                  if (size(effrl_inout(:,1)) /= ncol-1+1) then
                     write(errmsg, '(2(a,i8))')                                                   &
                          'In group main_suite_radiation1 before effr_calc_run: for array effrl_inout(:,1), expected size ',&
                          ncol-1+1, ' but got ', size(effrl_inout(:,1))
                     errflg = 1
                     return
                  end if
                  ! Check length of effrl_inout(1,:)
                  if (size(effrl_inout(1,:)) /= pver-1+1) then
                     write(errmsg, '(2(a,i8))')                                                   &
                          'In group main_suite_radiation1 before effr_calc_run: for array effrl_inout(1,:), expected size ',&
                          pver-1+1, ' but got ', size(effrl_inout(1,:))
                     errflg = 1
                     return
                  end if

                  if (has_ice) then
                     ! Check size of array effri_out
                     if (size(effri_out(:,:)) /= 1*(ncol-1+1)*(pver-1+1)) then
                        write(errmsg, '(2(a,i8))')                                                &
                             'In group main_suite_radiation1 before  effr_calc_run: for array effri_out, expected size ',&
                             1*(ncol-1+1)*(pver-1+1), ' but got ', size(effri_out)
                        errflg = 1
                        return
                     end if
                  end if

                  ! Check size of array effrs_inout
                  if (size(effrs_inout(:,:)) /= 1*(ncol-1+1)*(pver-1+1)) then
                     write(errmsg, '(2(a,i8))')                                                   &
                          'In group main_suite_radiation1 before  effr_calc_run: for array effrs_inout, expected size ',&
                          1*(ncol-1+1)*(pver-1+1), ' but got ', size(effrs_inout)
                     errflg = 1
                     return
                  end if

                  ! Check length of effrs_inout(:,1)
                  if (size(effrs_inout(:,1)) /= ncol-1+1) then
                     write(errmsg, '(2(a,i8))')                                                   &
                          'In group main_suite_radiation1 before effr_calc_run: for array effrs_inout(:,1), expected size ',&
                          ncol-1+1, ' but got ', size(effrs_inout(:,1))
                     errflg = 1
                     return
                  end if
                  ! Check length of effrs_inout(1,:)
                  if (size(effrs_inout(1,:)) /= pver-1+1) then
                     write(errmsg, '(2(a,i8))')                                                   &
                          'In group main_suite_radiation1 before effr_calc_run: for array effrs_inout(1,:), expected size ',&
                          pver-1+1, ' but got ', size(effrs_inout(1,:))
                     errflg = 1
                     return
                  end if

                  ! Assign value of has_graupel to internal_var_logical
                  internal_var_logical = has_graupel

                  ! Assign value of scalar_var1 to internal_var_real
                  internal_var_real = scalar_var1

                  ! Assign value of tke_inout to internal_var_real
                  internal_var_real = tke_inout

                  ! Assign value of tke2_inout to internal_var_real
                  internal_var_real = tke2_inout

                  ! ##################################################################
                  ! End debug tests
                  ! ##################################################################

                  ! Compute reverse (pre-scheme) transforms
                  effrr_in_local(:,1:pver) = 1.0E+6_kind_phys*effrr_inout(:,pver:1:-1)
                  effrg_in_local(:,1:pver) = 1.0E+6_kind_phys*effrg_in(:,1:pver)
                  effrl_inout_local(:,1:pver) = 1.0E+6_kind_phys*effrl_inout(:,1:pver)
                  effrs_inout_local(:,1:pver) = 1.0E+6_8*real(effrs_inout(:,pver:1:-1), 8)
                  scalar_var_local = 1.0E-3_kind_phys*scalar_var1

                  ! Associate conditional variables
                  if (has_graupel) then
                     effrg_in_ptr => effrg_in_local
                  end if
                  if (has_graupel) then
                     ncg_in_ptr => ncg_in
                  end if
                  if (has_ice) then
                     nci_out_ptr => nci_out
                  end if
                  if (has_ice) then
                     effri_out_ptr => effri_out_local
                  end if

                  ! Call scheme
                  call effr_calc_run(ncol=ncol, nlev=pver, effrr_in=effrr_in_local,               &
                       effrg_in=effrg_in_local, ncg_in=ncg_in_ptr, nci_out=nci_out_ptr,           &
                       effrl_inout=effrl_inout_local, effri_out=effri_out_ptr,                    &
                       effrs_inout=effrs_inout_local, ncl_out=ncl_out_ptr,                        &
                       has_graupel=has_graupel, scalar_var=scalar_var_local,                      &
                       tke_inout=tke_inout, tke2_inout=tke2_inout, errmsg=errmsg, errflg=errflg)

                  ! Copy any local pointers to dummy/local variables
                  if (has_ice) then
                     nci_out = nci_out_ptr
                  end if
                  if (has_ice) then
                     effri_out_local = effri_out_ptr
                  end if

                  ! Compute forward (post-scheme) transforms
                  effrl_inout(:,1:pver) = 1.0E-6_kind_phys*effrl_inout_local(:,1:pver)
                  effri_out(:,1:pver) = 1.0E-6_kind_phys*effri_out_local(:,1:pver)
                  effrs_inout(:,pver:1:-1) = 1.0E-6_kind_phys*real(effrs_inout_local(:,1:pver),   &
                       kind_phys)
                  scalar_var1 = 1.0E+3_kind_phys*scalar_var_local

               end if
      end do
      end do

         if (errflg == 0) then
            ! ##################################################################
            ! Begin debug tests
            ! ##################################################################

            ! Check size of array effrr_inout
            if (size(effrr_inout(:,:)) /= 1*(ncol-1+1)*(pver-1+1)) then
               write(errmsg, '(2(a,i8))')                                                         &
                    'In group main_suite_radiation1 before  effr_post_run: for array effrr_inout, expected size ',&
                    1*(ncol-1+1)*(pver-1+1), ' but got ', size(effrr_inout)
               errflg = 1
               return
            end if

            ! Check length of effrr_inout(:,1)
            if (size(effrr_inout(:,1)) /= ncol-1+1) then
               write(errmsg, '(2(a,i8))')                                                         &
                    'In group main_suite_radiation1 before effr_post_run: for array effrr_inout(:,1), expected size ', ncol-1+1,&
                    ' but got ', size(effrr_inout(:,1))
               errflg = 1
               return
            end if
            ! Check length of effrr_inout(1,:)
            if (size(effrr_inout(1,:)) /= pver-1+1) then
               write(errmsg, '(2(a,i8))')                                                         &
                    'In group main_suite_radiation1 before effr_post_run: for array effrr_inout(1,:), expected size ', pver-1+1,&
                    ' but got ', size(effrr_inout(1,:))
               errflg = 1
               return
            end if

            ! Assign value of scalar_var2 to internal_var_real
            internal_var_real = scalar_var2

            ! ##################################################################
            ! End debug tests
            ! ##################################################################



            ! Call scheme
            call effr_post_run(effrr_inout=effrr_inout, scalar_var=scalar_var2, errmsg=errmsg,    &
                 errflg=errflg)



         end if
      end do
      do loop0_num_subcycles_for_effr = 1, num_subcycles

         if (errflg == 0) then
            ! ##################################################################
            ! Begin debug tests
            ! ##################################################################

            ! Check size of array effrs_inout
            if (size(effrs_inout(:,:)) /= 1*(ncol-1+1)*(pver-1+1)) then
               write(errmsg, '(2(a,i8))')                                                         &
                    'In group main_suite_radiation1 before  effrs_calc_run: for array effrs_inout, expected size ',&
                    1*(ncol-1+1)*(pver-1+1), ' but got ', size(effrs_inout)
               errflg = 1
               return
            end if

            ! Check length of effrs_inout(:,1)
            if (size(effrs_inout(:,1)) /= ncol-1+1) then
               write(errmsg, '(2(a,i8))')                                                         &
                    'In group main_suite_radiation1 before effrs_calc_run: for array effrs_inout(:,1), expected size ', ncol-1+1,&
                    ' but got ', size(effrs_inout(:,1))
               errflg = 1
               return
            end if
            ! Check length of effrs_inout(1,:)
            if (size(effrs_inout(1,:)) /= pver-1+1) then
               write(errmsg, '(2(a,i8))')                                                         &
                    'In group main_suite_radiation1 before effrs_calc_run: for array effrs_inout(1,:), expected size ', pver-1+1,&
                    ' but got ', size(effrs_inout(1,:))
               errflg = 1
               return
            end if

            ! ##################################################################
            ! End debug tests
            ! ##################################################################



            ! Call scheme
            call effrs_calc_run(effrs_inout=effrs_inout, errmsg=errmsg, errflg=errflg)



         end if
      end do

      if (errflg == 0) then
         ! ##################################################################
         ! Begin debug tests
         ! ##################################################################

         ! Check size of array effrr_inout
         if (size(effrr_inout(:,:)) /= 1*(ncol-1+1)*(pver-1+1)) then
            write(errmsg, '(2(a,i8))')                                                            &
                 'In group main_suite_radiation1 before  effr_diag_run: for array effrr_inout, expected size ',&
                 1*(ncol-1+1)*(pver-1+1), ' but got ', size(effrr_inout)
            errflg = 1
            return
         end if

         ! Check length of effrr_inout(:,1)
         if (size(effrr_inout(:,1)) /= ncol-1+1) then
            write(errmsg, '(2(a,i8))')                                                            &
                 'In group main_suite_radiation1 before effr_diag_run: for array effrr_inout(:,1), expected size ', ncol-1+1,&
                 ' but got ', size(effrr_inout(:,1))
            errflg = 1
            return
         end if
         ! Check length of effrr_inout(1,:)
         if (size(effrr_inout(1,:)) /= pver-1+1) then
            write(errmsg, '(2(a,i8))')                                                            &
                 'In group main_suite_radiation1 before effr_diag_run: for array effrr_inout(1,:), expected size ', pver-1+1,&
                 ' but got ', size(effrr_inout(1,:))
            errflg = 1
            return
         end if

         ! Assign value of scalar_var3 to internal_var_integer
         internal_var_integer = scalar_var3

         ! ##################################################################
         ! End debug tests
         ! ##################################################################

         ! Compute reverse (pre-scheme) transforms
         effrr_in_local(:,1:pver) = 1.0E+6_kind_phys*effrr_inout(:,pver:1:-1)


         ! Call scheme
         call effr_diag_run(effrr_in=effrr_in_local, scalar_var=scalar_var3, errmsg=errmsg,       &
              errflg=errflg)



      end if

      ! Deallocate local arrays
      if (allocated(effrg_in_local))        deallocate(effrg_in_local)
      if (allocated(effri_out_local))       deallocate(effri_out_local)
      if (allocated(effrl_inout_local))     deallocate(effrl_inout_local)
      if (allocated(effrr_in_local))        deallocate(effrr_in_local)
      if (allocated(effrs_inout_local))     deallocate(effrs_inout_local)

      ! Nullify local pointers
      if (associated(effrg_in_ptr))     nullify(effrg_in_ptr)
      if (associated(ncg_in_ptr))       nullify(ncg_in_ptr)
      if (associated(nci_out_ptr))      nullify(nci_out_ptr)
      if (associated(effri_out_ptr))    nullify(effri_out_ptr)
      if (associated(ncl_out_ptr))      nullify(ncl_out_ptr)
      ! Suite state does not change

   end subroutine main_suite_radiation1

   ! ========================================================================


   subroutine main_suite_rad_lw_group(errflg, errmsg, col_start, col_end, fluxlw, ncols)

      use rad_lw, only: rad_lw_run
      use mod_rad_ddt,               only: ty_rad_lw

      ! Dummy arguments
      integer,            intent(out)   :: errflg
      character(len=512), intent(out)   :: errmsg
      integer,            intent(in)    :: col_start
      integer,            intent(in)    :: col_end
      type(ty_rad_lw),    intent(inout) :: fluxlw(:)
      integer,            intent(in)    :: ncols

      ! Local Variables
      integer                                  :: ncol
      type(ty_rad_lw)                          :: internal_var_ty_rad_lw

      ! Initialize ccpp error handling
      errflg = 0
      errmsg = ''

      ! Check state machine
      if (trim(ccpp_suite_state) /= 'in_time_step') then
         errflg = 1
         write(errmsg, '(3a)') "Invalid initial CCPP state, '", trim(ccpp_suite_state),           &
              "' in main_suite_rad_lw_group"
         return
      end if
      ! Set horizontal loop extent
      ncol = col_end - col_start + 1

      ! Allocate local arrays

      if (errflg == 0) then
         ! ##################################################################
         ! Begin debug tests
         ! ##################################################################

         ! ##################################################################
         ! End debug tests
         ! ##################################################################



         ! Call scheme
         call rad_lw_run(ncol=ncol, fluxlw=fluxlw, errmsg=errmsg, errflg=errflg)



      end if
      ! Suite state does not change

   end subroutine main_suite_rad_lw_group

   ! ========================================================================


   subroutine main_suite_rad_sw_group(errflg, errmsg, col_start, col_end, sfc_up_sw, ncols,       &
        sfc_down_sw)

      use rad_sw, only: rad_sw_run

      ! Dummy arguments
      integer,            intent(out)   :: errflg
      character(len=512), intent(out)   :: errmsg
      integer,            intent(in)    :: col_start
      integer,            intent(in)    :: col_end
      real(kind_phys),    intent(inout) :: sfc_up_sw(:)
      integer,            intent(in)    :: ncols
      real(kind_phys),    intent(inout) :: sfc_down_sw(:)

      ! Local Variables
      integer                                  :: ncol
      type(real)                               :: internal_var_real

      ! Initialize ccpp error handling
      errflg = 0
      errmsg = ''

      ! Check state machine
      if (trim(ccpp_suite_state) /= 'in_time_step') then
         errflg = 1
         write(errmsg, '(3a)') "Invalid initial CCPP state, '", trim(ccpp_suite_state),           &
              "' in main_suite_rad_sw_group"
         return
      end if
      ! Set horizontal loop extent
      ncol = col_end - col_start + 1

      ! Allocate local arrays

      if (errflg == 0) then
         ! ##################################################################
         ! Begin debug tests
         ! ##################################################################

         ! Check size of array sfc_up_sw
         if (size(sfc_up_sw(:)) /= 1*(ncol-1+1)) then
            write(errmsg, '(2(a,i8))')                                                            &
                 'In group main_suite_rad_sw_group before  rad_sw_run: for array sfc_up_sw, expected size ', 1*(ncol-1+1),&
                 ' but got ', size(sfc_up_sw)
            errflg = 1
            return
         end if


         ! Check size of array sfc_down_sw
         if (size(sfc_down_sw(:)) /= 1*(ncol-1+1)) then
            write(errmsg, '(2(a,i8))')                                                            &
                 'In group main_suite_rad_sw_group before  rad_sw_run: for array sfc_down_sw, expected size ', 1*(ncol-1+1),&
                 ' but got ', size(sfc_down_sw)
            errflg = 1
            return
         end if


         ! ##################################################################
         ! End debug tests
         ! ##################################################################



         ! Call scheme
         call rad_sw_run(ncol=ncol, sfc_up_sw=sfc_up_sw, sfc_down_sw=sfc_down_sw, errmsg=errmsg,  &
              errflg=errflg)



      end if
      ! Suite state does not change

   end subroutine main_suite_rad_sw_group

   ! ========================================================================


   subroutine main_suite_timestep_final(errflg, errmsg)


      ! Dummy arguments
      integer,            intent(out)   :: errflg
      character(len=512), intent(out)   :: errmsg

      ! Initialize ccpp error handling
      errflg = 0
      errmsg = ''

      ! Output threaded region check
#ifdef _OPENMP
      if (omp_get_thread_num() > 1) then
         errflg = 1
         errmsg = "Cannot call timestep_final routine from a threaded region"
         return
      end if
#endif
      ! Check state machine
      if (trim(ccpp_suite_state) /= 'in_time_step') then
         errflg = 1
         write(errmsg, '(3a)') "Invalid initial CCPP state, '", trim(ccpp_suite_state),           &
              "' in main_suite_timestep_final"
         return
      end if
      ! Set horizontal loop extent

      ! Allocate local arrays
      ccpp_suite_state = 'initialized'

   end subroutine main_suite_timestep_final

   ! ========================================================================


   subroutine main_suite_finalize(errflg, errmsg)


      ! Dummy arguments
      integer,            intent(out)   :: errflg
      character(len=512), intent(out)   :: errmsg

      ! Initialize ccpp error handling
      errflg = 0
      errmsg = ''

      ! Output threaded region check
#ifdef _OPENMP
      if (omp_get_thread_num() > 1) then
         errflg = 1
         errmsg = "Cannot call finalize routine from a threaded region"
         return
      end if
#endif
      ! Check state machine
      if (trim(ccpp_suite_state) /= 'initialized') then
         errflg = 1
         write(errmsg, '(3a)') "Invalid initial CCPP state, '", trim(ccpp_suite_state),           &
              "' in main_suite_finalize"
         return
      end if
      ! Set horizontal loop extent

      ! Allocate local arrays
      ccpp_suite_state = 'uninitialized'

   end subroutine main_suite_finalize

   ! ========================================================================

   subroutine ccpp_create_constituent_array(errmsg, errflg)
      ! Allocate and fill the constituent property array
      !    for this suite
      ! Dummy arguments
      character(len=512), intent(out)   :: errmsg
      integer,            intent(out)   :: errflg
      errmsg = ''
      errflg = 0
      ccpp_constituents_initialized = .true.
   end subroutine ccpp_create_constituent_array


   ! ========================================================================

   integer function main_suite_constituents_num_consts(errmsg, errflg)
      ! Return the number of constituents for this suite
      ! Dummy arguments
      character(len=512), intent(out)   :: errmsg
      integer,            intent(out)   :: errflg
      errmsg = ''
      errflg = 0
      ! Make sure that our constituent array is initialized
      if (.not. ccpp_constituents_initialized) then
         call ccpp_create_constituent_array(errflg=errflg, errmsg=errmsg)
      end if
      main_suite_constituents_num_consts = 0
   end function main_suite_constituents_num_consts

   ! ========================================================================

   subroutine main_suite_constituents_const_name(index, name_out, errmsg, errflg)
      ! Return the name of constituent, <index>
      ! Dummy arguments
      integer,            intent(in)    :: index
      character(len=*),   intent(out)   :: name_out
      character(len=512), intent(out)   :: errmsg
      integer,            intent(out)   :: errflg

      errflg = 0
      errmsg = ''
      ! Make sure that our constituent array is initialized
      if (.not. ccpp_constituents_initialized) then
         errflg = 1
         errmsg = "constituent properties not initialized for suite, main_suite"
      end if
      errflg = 1
      write(errmsg, '(a,i0,a)') 'ERROR: main_suite_constituents, has no constituents'
   end subroutine main_suite_constituents_const_name

   ! ========================================================================

   subroutine main_suite_constituents_copy_const(index, cnst_out, errmsg, errflg)
      ! Copy the data for a constituent
      ! Dummy arguments
      integer,            intent(in)    :: index
      type(ccpp_constituent_properties_t), intent(out)     :: cnst_out
      character(len=512), intent(out)   :: errmsg
      integer,            intent(out)   :: errflg

      errflg = 0
      errmsg = ''
      ! Make sure that our constituent array is initialized
      if (.not. ccpp_constituents_initialized) then
         errflg = 1
         errmsg = "constituent properties not initialized for suite, main_suite"
      end if
      errflg = 1
      write(errmsg, '(a,i0,a)') 'ERROR: main_suite_constituents, has no constituents'
   end subroutine main_suite_constituents_copy_const

end module ccpp_main_suite_cap
