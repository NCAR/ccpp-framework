module test_host_mod

   use ccpp_kinds,     only: kind_phys
   use test_host_data, only: physics_state, allocate_physics_state

   implicit none
   public

   integer,         parameter   :: num_time_steps = 2
   real(kind_phys), parameter   :: tolerance = 1.0e-13_kind_phys

   !> \section arg_table_test_host_mod  Argument Table
   !! \htmlinclude arg_table_test_host_host.html
   !!
   integer,         parameter   :: ncols = 10
   integer,         parameter   :: pver = 5
   integer,         parameter   :: pverP = pver + 1
   integer,         protected   :: ncnst = -1
   integer,         protected   :: index_qv = -1
   real(kind_phys)              :: dt
   real(kind_phys), parameter   :: tfreeze = 273.15_kind_phys
   type(physics_state)          :: phys_state
   integer                      :: num_model_times = -1
   integer,         allocatable :: model_times(:)

   public :: init_data
   public :: compare_data
   public :: twist_array

   real(kind_phys), private, allocatable :: check_vals(:,:,:)
   real(kind_phys), private              :: check_temp(ncols, pver)
   integer,         private              :: ind_liq = -1
   integer,         private              :: ind_ice = -1

contains

   subroutine init_data(constituent_array, index_qv_use, index_liq, index_ice)

      ! Dummy arguments
      real(kind_phys), pointer  :: constituent_array(:,:,:) ! From host & suites
      integer,       intent(in) :: index_qv_use
      integer,       intent(in) :: index_liq
      integer,       intent(in) :: index_ice

      ! Local variables
      integer                    :: col
      integer                    :: lev
      integer                    :: cind
      integer                    :: itime
      real(kind_phys)            :: qmax
      real(kind_phys), parameter :: inc = 0.1_kind_phys

      ! Allocate and initialize state
      ! Temperature starts above freezing and decreases to -30C
      ! water vapor is initialized in odd columns to different amounts
      ncnst = SIZE(constituent_array, 3)
      call allocate_physics_state(ncols, pver, constituent_array, phys_state)
      index_qv = index_qv_use
      ind_liq = index_liq
      ind_ice = index_ice
      allocate(check_vals(ncols, pver, ncnst))
      check_vals(:,:,:) = 0.0_kind_phys
      do lev = 1, pver
         phys_state%temp(:, lev) = tfreeze + (10.0_kind_phys * (lev - 3))
         qmax = real(lev, kind_phys)
         do col = 1, ncols
            if (mod(col, 2) == 1) then
               phys_state%q(col, lev, index_qv) = qmax
            else
               phys_state%q(col, lev, index_qv) = 0.0_kind_phys
            end if
         end do
      end do
      check_vals(:,:,index_qv) = phys_state%q(:,:,index_qv)
      check_temp(:,:) = phys_state%temp(:,:)
      ! Do timestep 1
      do col = 1, ncols, 2
         check_temp(col, 1) = check_temp(col, 1) + 0.5_kind_phys
         check_vals(col, 1, index_qv) = check_vals(col, 1, index_qv) - inc
         check_vals(col, 1, ind_liq) = check_vals(col, 1, ind_liq) + inc
      end do
      do itime = 1, num_time_steps
         do cind = 1, ncnst
            call twist_array(check_vals(:,:,cind))
         end do
      end do

   end subroutine init_data

   subroutine twist_array(array)
      ! Dummy argument
      real(kind_phys), intent(inout) :: array(:,:)

       ! Local variables
       integer         :: icol, ilev ! Field coordinates
       integer         :: idir       ! 'w' sign
       integer         :: levb, leve ! Starting and ending level indices
       real(kind_phys) :: last_val, next_val

       idir = 1
       leve = (pver * mod(ncols, 2)) + mod(ncols-1, 2)
       last_val = array(ncols, leve)
       do icol = 1, ncols
          levb = ((pver * (1 - idir)) + (1 + idir)) / 2
          leve = ((pver * (1 + idir)) + (1 - idir)) / 2
          do ilev = levb, leve, idir
             next_val = array(icol, ilev)
             array(icol, ilev) = last_val
             last_val = next_val
          end do
          idir = -1 * idir
       end do

   end subroutine twist_array

   logical function compare_data(ncnst)

      integer, intent(in) :: ncnst

      integer            :: col
      integer            :: lev
      integer            :: cind
      logical            :: need_header
      real(kind_phys)    :: check
      real(kind_phys)    :: denom

      compare_data = .true.

      need_header = .true.
      do lev = 1, pver
         do col = 1, ncols
            check = check_temp(col, lev)
            if (abs((phys_state%temp(col, lev) - check) / check) >            &
                 tolerance) then
               if (need_header) then
                  write(6, '("  COL  LEV      T MIDPOINTS        EXPECTED")')
                  need_header = .false.
               end if
               write(6, '(2i5,2(3x,es15.7))') col, lev,                       &
                    phys_state%temp(col, lev), check
               compare_data = .false.
            end if
         end do
      end do
      ! Check constituents
      need_header = .true.
      do cind = 1, ncnst
         do lev = 1, pver
            do col = 1, ncols
               check = check_vals(col, lev, cind)
               if (check < tolerance) then
                  denom = 1.0_kind_phys
               else
                  denom = check
               end if
               if (abs((phys_state%q(col, lev, cind) - check) / denom) >      &
                    tolerance) then
                  if (need_header) then
                     write(6, '(2(2x,a),3x,a,10x,a,14x,a)')                   &
                          'COL', 'LEV', 'C#', 'Q', 'EXPECTED'
                     need_header = .false.
                  end if
                  write(6, '(3i5,2(3x,es15.7))') col, lev, cind,              &
                       phys_state%q(col, lev, cind), check
                  compare_data = .false.
               end if
            end do
         end do
      end do

   end function compare_data

end module test_host_mod
