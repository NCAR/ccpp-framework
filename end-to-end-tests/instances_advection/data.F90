module data

  use ccpp_kinds, only: kind_phys

  implicit none
  public

  ! Sizing parameters (shared across instances)
  integer, parameter :: ncols      = 4
  integer, parameter :: pver       = 3
  integer, parameter :: ninstances = 3

  ! Time-step + freezing-point constants (shared across instances)
  real(kind=kind_phys), parameter :: dt      = 1.0_kind_phys
  real(kind=kind_phys), parameter :: tfreeze = 273.15_kind_phys
  integer, parameter :: num_time_steps = 2

  ! qv index in the constituent state array; filled at runtime after
  ! ccpp_register_constituents.  Identical across instances because all
  ! instances register the same constituents in the same order.
  integer, protected :: index_qv = -1

  ! Per-instance mutable physics state
  !
  ! \section arg_table_physics_state Argument Table
  !! \htmlinclude arg_table_physics_state.html
  type physics_state
    real(kind=kind_phys), allocatable :: ps(:)         ! surface pressure
    real(kind=kind_phys), allocatable :: temp(:, :)    ! temperature
    real(kind=kind_phys), pointer     :: q(:, :, :) => null() ! constituent array
  end type physics_state

  type(physics_state), target :: phys_state(ninstances)

  ! Per-instance distinct initial qv values (used to drive distinct results)
  real(kind=kind_phys), parameter, dimension(ninstances) :: &
      qv_init = (/ 1.0_kind_phys, 2.0_kind_phys, 3.0_kind_phys /)

  ! Tolerance for the final verification check
  real(kind=kind_phys), parameter :: tolerance = 1.0e-12_kind_phys

contains

  subroutine set_index_qv(idx)
    integer, intent(in) :: idx
    index_qv = idx
  end subroutine set_index_qv

  subroutine allocate_physics_state(ins, constituents_ptr)
    ! Wire phys_state(ins) to its per-instance constituent array and allocate
    ! the per-instance temp/ps storage.
    integer, intent(in) :: ins
    real(kind=kind_phys), pointer :: constituents_ptr(:, :, :)

    if (allocated(phys_state(ins)%ps)) then
      deallocate(phys_state(ins)%ps)
    end if
    allocate(phys_state(ins)%ps(ncols))
    phys_state(ins)%ps = 1000.0_kind_phys

    if (allocated(phys_state(ins)%temp)) then
      deallocate(phys_state(ins)%temp)
    end if
    allocate(phys_state(ins)%temp(ncols, pver))
    ! Start cold so cld_liq_run will produce a tendency on the first call.
    phys_state(ins)%temp = tfreeze - 30.0_kind_phys

    if (associated(phys_state(ins)%q)) nullify(phys_state(ins)%q)
    phys_state(ins)%q => constituents_ptr

  end subroutine allocate_physics_state

  subroutine init_qv(ins)
    ! Seed the per-instance constituent array with a distinct qv value.
    integer, intent(in) :: ins
    phys_state(ins)%q(:, :, :)        = 0.0_kind_phys
    phys_state(ins)%q(:, :, index_qv) = qv_init(ins)
  end subroutine init_qv

  logical function verify_results(num_consts)
    ! Check per-instance mass conservation and cross-instance distinctness.
    integer, intent(in) :: num_consts

    real(kind=kind_phys) :: q_sum(ninstances)
    real(kind=kind_phys) :: cld_liq_max(ninstances)
    integer :: ins, ins2, k
    logical :: ok

    verify_results = .true.

    ! Mass conservation per instance: total constituent mass per instance
    ! should equal ncols * pver * qv_init(ins) (because all qv either
    ! stays as qv or is moved into cld_liq by cld_liq_run; nothing else
    ! produces or consumes mass in this minimal scheme set).
    do ins = 1, ninstances
      q_sum(ins) = 0.0_kind_phys
      do k = 1, num_consts
        q_sum(ins) = q_sum(ins) + sum(phys_state(ins)%q(:, :, k))
      end do
      cld_liq_max(ins) = maxval(phys_state(ins)%q(:, :, :))
      ok = abs(q_sum(ins) - real(ncols, kind_phys) * real(pver, kind_phys) &
              * qv_init(ins)) < tolerance * real(ncols * pver, kind_phys) &
              * qv_init(ins)
      if (.not. ok) then
        write(6, '(a,i0,a,es15.7,a,es15.7)') &
            'FAIL mass conservation for instance ', ins, &
            ': q_sum=', q_sum(ins), '   expected~', &
            real(ncols, kind_phys) * real(pver, kind_phys) * qv_init(ins)
        verify_results = .false.
      end if
    end do

    ! Cross-instance distinctness: instance i must end up with a different
    ! state than instance j, since they started from different qv.
    do ins = 1, ninstances
      do ins2 = ins + 1, ninstances
        if (abs(q_sum(ins) - q_sum(ins2)) < tolerance) then
          write(6, '(a,i0,a,i0,a)') &
              'FAIL distinctness: instance ', ins, ' and instance ', ins2, &
              ' have identical totals (state leaked between instances?)'
          verify_results = .false.
        end if
      end do
    end do

  end function verify_results

end module data
