!>\file const_dim_producer.F90
!! Exercises three ways a variable can be dimensioned by the framework
!! constituent count number_of_ccpp_constituents:
!!   Case 1  - consume a HOST array dimensioned by the count (passed as ':').
!!   Case 2a - fill a non-allocatable SUITE workspace the framework allocated
!!             in init_fields (sized to ccpp_model_constituents_obj(i)%num_layer_vars).
!!   Case 2b - allocate an allocatable SUITE workspace here in _run, sized by the
!!             count received as a scalar argument.

module const_dim_producer

  use ccpp_kinds, only: kind_phys

  implicit none

  private
  public :: const_dim_producer_run

contains

  !! \section arg_table_const_dim_producer_run Argument Table
  !! \htmlinclude const_dim_producer_run.html
  !!
  subroutine const_dim_producer_run(coupler_flux, cwork, n_const, awork, &
      qbase, qtend, errmsg, errcode)
    real(kind=kind_phys),              intent(in)    :: coupler_flux(:, :)
    real(kind=kind_phys),              intent(out)   :: cwork(:)
    integer,                           intent(in)    :: n_const
    real(kind=kind_phys), allocatable, intent(out)   :: awork(:)
    real(kind=kind_phys),              intent(inout) :: qbase(:, :)
    real(kind=kind_phys),              intent(out)   :: qtend(:, :)
    character(len=*),                  intent(out)   :: errmsg
    integer,                           intent(out)   :: errcode

    integer :: m, i

    errmsg = ''
    errcode = 0

    ! Case 1: the host filled coupler_flux(i, m) = m; the constituent axis was
    ! passed whole (':'), so size(coupler_flux, 2) is the constituent count.
    do m = 1, size(coupler_flux, 2)
      do i = 1, size(coupler_flux, 1)
        if (coupler_flux(i, m) /= real(m, kind_phys)) then
          errcode = 1
          errmsg = 'Case 1: coupler_flux dimensioned by ' // &
              'number_of_ccpp_constituents has the wrong value'
          return
        end if
      end do
    end do

    ! Case 2a: fill the framework-allocated (init_fields) suite workspace.
    do m = 1, size(cwork)
      cwork(m) = real(10 * m, kind_phys)
    end do

    ! Case 2b: this scheme owns the allocation, sized by the scalar count which
    ! the framework resolves to ccpp_model_constituents_obj(inst)%num_layer_vars.
    allocate(awork(n_const))
    do m = 1, n_const
      awork(m) = real(100 * m, kind_phys)
    end do

    ! Rule (b), producer side: flag a base constituent (advected) and a
    ! constituent tendency (constituent=true) and write known values into their
    ! framework columns.  const_dim_consumer reads both with NO flag (inference).
    qbase = 42.0_kind_phys
    qtend = 7.0_kind_phys
  end subroutine const_dim_producer_run

end module const_dim_producer
