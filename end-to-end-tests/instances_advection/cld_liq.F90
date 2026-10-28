! Test parameterization with advected species
!

module cld_liq

  use ccpp_kinds, only: kind_phys
  use ccpp_constituent_prop_mod, only: ccpp_constituent_properties_t

  implicit none
  private

  public :: cld_liq_register
  public :: cld_liq_init
  public :: cld_liq_run

contains

  !> \section arg_table_cld_liq_register  Argument Table
  !! \htmlinclude arg_table_cld_liq_register.html
  !!
  subroutine cld_liq_register(dyn_const, errmsg, errcode)
    type(ccpp_constituent_properties_t), allocatable, intent(out) :: dyn_const(:)
    character(len=512), intent(out) :: errmsg
    integer, intent(out) :: errcode

    errmsg = ''
    errcode = 0
    allocate(dyn_const(1), stat=errcode)
    if (errcode /= 0) then
      errmsg = 'Error allocating dyn_const in cld_liq_register'
      return
    end if
    call dyn_const(1)%instantiate(std_name="cloud_liquid_dry_mixing_ratio", long_name='Cloud liquid dry mixing ratio', &
        diag_name='CLDLIQ', units='kg kg-1', default_value=0._kind_phys, &
        vertical_dim='vertical_layer_dimension', advected=.true., &
        mixing_ratio_type='dry', &
        errcode=errcode, errmsg=errmsg)

  end subroutine cld_liq_register

  !> \section arg_table_cld_liq_run  Argument Table
  !! \htmlinclude arg_table_cld_liq_run.html
  !!
  subroutine cld_liq_run(ncol, timestep, tcld, temp, qv, ps, &
      cld_liq_array, cld_liq_tend, errmsg, errcode)

    integer, intent(in) :: ncol
    real(kind=kind_phys), intent(in) :: timestep
    real(kind=kind_phys), intent(in) :: tcld
    real(kind=kind_phys), intent(inout) :: temp(:, :)
    real(kind=kind_phys), intent(inout) :: qv(:, :)
    real(kind=kind_phys), intent(in) :: ps(:)
    real(kind=kind_phys), intent(inout) :: cld_liq_array(:, :)
    real(kind=kind_phys), intent(out) :: cld_liq_tend(:, :)
    character(len=512), intent(out) :: errmsg
    integer, intent(out) :: errcode
    !----------------------------------------------------------------

    integer :: icol
    integer :: ilev
    real(kind=kind_phys) :: cond

    errmsg = ''
    errcode = 0

    do icol = 1, ncol
      do ilev = 1, size(temp, 2)
        cld_liq_array(icol, ilev) = max(0.0_kind_phys, cld_liq_array(icol, ilev))
        if ((qv(icol, ilev) > 0.0_kind_phys) .and. &
            (temp(icol, ilev) <= tcld)) then
          cond = min(qv(icol, ilev), 0.1_kind_phys)
          cld_liq_tend(icol, ilev) = cond
          qv(icol, ilev) = qv(icol, ilev) - cond
          if (cond > 0.0_kind_phys) then
            temp(icol, ilev) = temp(icol, ilev) + (cond * 5.0_kind_phys)
          end if
        end if
      end do
    end do

  end subroutine cld_liq_run

  !> \section arg_table_cld_liq_init  Argument Table
  !! \htmlinclude arg_table_cld_liq_init.html
  !!
  subroutine cld_liq_init(tfreeze, tcld, errmsg, errcode)

    real(kind=kind_phys), intent(in) :: tfreeze
    real(kind=kind_phys), intent(out) :: tcld
    character(len=512), intent(out) :: errmsg
    integer, intent(out) :: errcode

    errmsg = ''
    errcode = 0
    tcld = tfreeze - 20.0_kind_phys

  end subroutine cld_liq_init

end module cld_liq
