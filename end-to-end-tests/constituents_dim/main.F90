program test_constituents_dim

  use, intrinsic :: iso_fortran_env, only: output_unit, &
      error_unit

  use ccpp_kinds, only: kind_phys
  use ccpp_constituent_prop_mod, only: ccpp_constituent_properties_t

  use host_data, only: ncols, &
      pver, &
      coupler_flux

  use test_host_ccpp_cap, only: ccpp_register, &
      ccpp_register_constituents, &
      ccpp_number_constituents, &
      ccpp_initialize_constituents, &
      ccpp_init, &
      ccpp_physics_init, &
      ccpp_physics_timestep_init, &
      ccpp_physics_run, &
      ccpp_physics_timestep_final, &
      ccpp_physics_final, &
      ccpp_final, &
      ccpp_deallocate_dynamic_constituents

  implicit none

  character(len=*), parameter :: ccpp_suite = 'constituents_dim_suite'
  character(len=512) :: errmsg
  integer :: errcode
  integer :: num_const
  integer :: m, i
  type(ccpp_constituent_properties_t), allocatable, target :: host_constituents(:)

  errcode = 0
  errmsg = ''

  ! Register phase: register_consts_register registers 3 dynamic constituents,
  ! which is what gives the suite a non-trivial number_of_ccpp_constituents.
  call ccpp_register(suite_name=trim(ccpp_suite), errmsg=errmsg, errcode=errcode)
  call check('ccpp_register')

  ! This test registers all constituents on the scheme side; the host adds none.
  allocate(host_constituents(0))
  call ccpp_register_constituents(host_constituents, errmsg=errmsg, errcode=errcode)
  call check('ccpp_register_constituents')

  call ccpp_number_constituents(num_const, errmsg=errmsg, errcode=errcode)
  call check('ccpp_number_constituents')
  if (num_const < 1) then
    write(error_unit, '(a,i0)') &
        'Error: expected at least one constituent, got ', num_const
    stop 1
  end if

  call ccpp_initialize_constituents(ncols=ncols, num_layers=pver, &
      errcode=errcode, errmsg=errmsg)
  call check('ccpp_initialize_constituents')

  ! Case 1: the host owns coupler_flux and sizes it to the constituent count.
  ! capgen passes the whole constituent axis (':') to const_dim_producer_run.
  allocate(coupler_flux(ncols, num_const))
  do m = 1, num_const
    do i = 1, ncols
      coupler_flux(i, m) = real(m, kind_phys)
    end do
  end do

  ! ccpp_init -> suite_data_init_fields allocates the non-allocatable suite
  ! workspace (Case 2a) using ccpp_model_constituents_obj(i)%num_layer_vars.
  call ccpp_init(suite_name=trim(ccpp_suite), errmsg=errmsg, errcode=errcode)
  call check('ccpp_init')

  call ccpp_physics_init(suite_name=trim(ccpp_suite), group_name='all', &
      col_start=1, col_end=ncols, thread_num=1, nthreads=1, nphys_threads=1, &
      errmsg=errmsg, errcode=errcode)
  call check('ccpp_physics_init')

  call ccpp_physics_timestep_init(suite_name=trim(ccpp_suite), group_name='all', &
      col_start=1, col_end=ncols, thread_num=1, nthreads=1, nphys_threads=1, &
      errmsg=errmsg, errcode=errcode)
  call check('ccpp_physics_timestep_init')

  ! Producer fills/allocates the workspaces and checks Case 1; consumer verifies
  ! Cases 2a/2b.  Any mismatch sets errcode inside the schemes.
  call ccpp_physics_run(suite_name=trim(ccpp_suite), group_name='all', &
      col_start=1, col_end=ncols, thread_num=1, nthreads=1, nphys_threads=1, &
      errmsg=errmsg, errcode=errcode)
  call check('ccpp_physics_run')

  call ccpp_physics_timestep_final(suite_name=trim(ccpp_suite), group_name='all', &
      col_start=1, col_end=ncols, thread_num=1, nthreads=1, nphys_threads=1, &
      errmsg=errmsg, errcode=errcode)
  call check('ccpp_physics_timestep_final')

  call ccpp_physics_final(suite_name=trim(ccpp_suite), group_name='all', &
      col_start=1, col_end=ncols, thread_num=1, nthreads=1, nphys_threads=1, &
      errmsg=errmsg, errcode=errcode)
  call check('ccpp_physics_final')

  ! ccpp_final -> suite_data_final_fields frees both suite workspaces (guarded).
  call ccpp_final(suite_name=trim(ccpp_suite), errmsg=errmsg, errcode=errcode)
  call check('ccpp_final')

  call ccpp_deallocate_dynamic_constituents()
  deallocate(host_constituents)
  if (allocated(coupler_flux)) deallocate(coupler_flux)

  write(output_unit, '(a,i0,a)') &
      'PASS: constituents_dim (number_of_ccpp_constituents = ', num_const, ')'

contains

  subroutine check(phase)
    character(len=*), intent(in) :: phase
    if (errcode /= 0) then
      write(error_unit, '(a)') 'An error occurred in ' // trim(phase) // ':'
      write(error_unit, '(a)') trim(errmsg)
      stop 1
    end if
  end subroutine check

end program test_constituents_dim
