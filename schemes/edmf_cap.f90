
!
! This work (Common Community Physics Package), identified by NOAA, NCAR,
! CU/CIRES, is free of known copyright restrictions and is placed in the
! public domain.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
! THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
! IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
! CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
!

!>
!! @brief Auto-generated cap module for the edmf scheme
!!
!
module edmf_cap

    use, intrinsic :: iso_c_binding,                                   &
                      only: c_f_pointer, c_ptr
    use            :: ccpp_types,                                      &
                      only: ccpp_t
    use            :: ccpp_fields,                                     &
                      only: ccpp_fields_get
    use            :: ccpp_errors,                                     &
                      only: ccpp_error
    use            :: edmf, &
                      only: edmf_tridin,edmf_tridi2,edmf_run,edmf_finalize,edmf_init
    implicit none

    private
    public :: edmf_tridin_cap,edmf_tridi2_cap,edmf_run_cap,edmf_finalize_cap,edmf_init_cap

    contains


    subroutine edmf_tridin_cap(ptr) bind(c)

        type(c_ptr), intent(inout) :: ptr

        type(ccpp_t), pointer      :: cdata
        integer                    :: ierr


        call c_f_pointer(ptr, cdata)

        

        call edmf_tridin()
    end subroutine edmf_tridin_cap

    subroutine edmf_tridi2_cap(ptr) bind(c)

        type(c_ptr), intent(inout) :: ptr

        type(ccpp_t), pointer      :: cdata
        integer                    :: ierr


        call c_f_pointer(ptr, cdata)

        

        call edmf_tridi2()
    end subroutine edmf_tridi2_cap

    subroutine edmf_run_cap(ptr) bind(c)

        type(c_ptr), intent(inout) :: ptr

        type(ccpp_t), pointer      :: cdata
        integer                    :: ierr
        integer, pointer     :: ix
        integer, pointer     :: im
        integer, pointer     :: km
        integer, pointer     :: ntrac
        integer, pointer     :: ntcw
        real, pointer     :: dv(:,:)
        real, pointer     :: du(:,:)
        real, pointer     :: tau(:,:)
        real, pointer     :: rtg(:,:,:)
        real, pointer     :: u1(:,:)
        real, pointer     :: v1(:,:)
        real, pointer     :: t1(:,:)
        real, pointer     :: q1(:,:,:)
        real, pointer     :: swh(:,:)
        real, pointer     :: hlw(:,:)
        real, pointer     :: xmu(:,:)
        real, pointer     :: psk(:)
        real, pointer     :: rbsoil(:)
        real, pointer     :: zorl(:)
        real, pointer     :: u10m(:)
        real, pointer     :: v10m(:)
        real, pointer     :: fm(:)
        real, pointer     :: fh(:)
        real, pointer     :: tsea(:)
        real, pointer     :: heat(:)
        real, pointer     :: evap(:)
        real, pointer     :: stress(:)
        real, pointer     :: spd1(:)
        integer, pointer     :: kpbl(:)
        real, pointer     :: prsi(:,:)
        real, pointer     :: del(:,:)
        real, pointer     :: prsl(:,:)
        real, pointer     :: prslk(:,:)
        real, pointer     :: phii(:,:)
        real, pointer     :: phil(:,:)
        real, pointer     :: delt
        logical, pointer     :: dspheat
        real, pointer     :: dusfc(:)
        real, pointer     :: dvsfc(:)
        real, pointer     :: dtsfc(:)
        real, pointer     :: dqsfc(:)
        real, pointer     :: hpbl(:)
        real, pointer     :: hgamt(:)
        real, pointer     :: hgamq(:)
        real, pointer     :: dkt(:)
        integer, pointer     :: kinver(:)
        real, pointer     :: xkzm_m
        real, pointer     :: xkzm_h
        real, pointer     :: xkzm_s
        logical, pointer     :: lprnt
        integer, pointer     :: ipr

        call c_f_pointer(ptr, cdata)

        
        call ccpp_fields_get(cdata, 'horizontal_dimension', ix, ierr)
        if (ierr /= 0) then
            call ccpp_error('Unable to retrieve horizontal_dimension')
            return
        end if

        call ccpp_fields_get(cdata, 'horizontal_loop_extent', im, ierr)
        if (ierr /= 0) then
            call ccpp_error('Unable to retrieve horizontal_loop_extent')
            return
        end if

        call ccpp_fields_get(cdata, 'vertical_dimension', km, ierr)
        if (ierr /= 0) then
            call ccpp_error('Unable to retrieve vertical_dimension')
            return
        end if

        call ccpp_fields_get(cdata, 'number_of_vertical_diffusion_tracers', ntrac, ierr)
        if (ierr /= 0) then
            call ccpp_error('Unable to retrieve number_of_vertical_diffusion_tracers')
            return
        end if

        call ccpp_fields_get(cdata, 'index_for_liquid_cloud_condensate', ntcw, ierr)
        if (ierr /= 0) then
            call ccpp_error('Unable to retrieve index_for_liquid_cloud_condensate')
            return
        end if

        call ccpp_fields_get(cdata, 'tendency_of_y_wind_due_to_model_physics', dv, ierr)
        if (ierr /= 0) then
            call ccpp_error('Unable to retrieve tendency_of_y_wind_due_to_model_physics')
            return
        end if

        call ccpp_fields_get(cdata, 'tendency_of_x_wind_due_to_model_physics', du, ierr)
        if (ierr /= 0) then
            call ccpp_error('Unable to retrieve tendency_of_x_wind_due_to_model_physics')
            return
        end if

        call ccpp_fields_get(cdata, 'tendency_of_air_temperature_due_to_model_physics', tau, ierr)
        if (ierr /= 0) then
            call ccpp_error('Unable to retrieve tendency_of_air_temperature_due_to_model_physics')
            return
        end if

        call ccpp_fields_get(cdata, 'tendency_of_tracers_due_to_model_physics', rtg, ierr)
        if (ierr /= 0) then
            call ccpp_error('Unable to retrieve tendency_of_tracers_due_to_model_physics')
            return
        end if

        call ccpp_fields_get(cdata, 'x_wind', u1, ierr)
        if (ierr /= 0) then
            call ccpp_error('Unable to retrieve x_wind')
            return
        end if

        call ccpp_fields_get(cdata, 'y_wind', v1, ierr)
        if (ierr /= 0) then
            call ccpp_error('Unable to retrieve y_wind')
            return
        end if

        call ccpp_fields_get(cdata, 'air_temperature', t1, ierr)
        if (ierr /= 0) then
            call ccpp_error('Unable to retrieve air_temperature')
            return
        end if

        call ccpp_fields_get(cdata, 'tracer_concentration', q1, ierr)
        if (ierr /= 0) then
            call ccpp_error('Unable to retrieve tracer_concentration')
            return
        end if

        call ccpp_fields_get(cdata, 'tendency_of_air_temperature_due_to_shortwave_heating', swh, ierr)
        if (ierr /= 0) then
            call ccpp_error('Unable to retrieve tendency_of_air_temperature_due_to_shortwave_heating')
            return
        end if

        call ccpp_fields_get(cdata, 'tendency_of_air_temperature_due_to_longwave_heating', hlw, ierr)
        if (ierr /= 0) then
            call ccpp_error('Unable to retrieve tendency_of_air_temperature_due_to_longwave_heating')
            return
        end if

        call ccpp_fields_get(cdata, 'time_step_zenith_angle_adjust_factor_for_sw', xmu, ierr)
        if (ierr /= 0) then
            call ccpp_error('Unable to retrieve time_step_zenith_angle_adjust_factor_for_sw')
            return
        end if

        call ccpp_fields_get(cdata, 'exner_function_at_lowest_model_interface', psk, ierr)
        if (ierr /= 0) then
            call ccpp_error('Unable to retrieve exner_function_at_lowest_model_interface')
            return
        end if

        call ccpp_fields_get(cdata, 'bulk_richardson_number_at_lowest_model_level', rbsoil, ierr)
        if (ierr /= 0) then
            call ccpp_error('Unable to retrieve bulk_richardson_number_at_lowest_model_level')
            return
        end if

        call ccpp_fields_get(cdata, 'surface_roughness_length', zorl, ierr)
        if (ierr /= 0) then
            call ccpp_error('Unable to retrieve surface_roughness_length')
            return
        end if

        call ccpp_fields_get(cdata, 'x_wind_at_10m', u10m, ierr)
        if (ierr /= 0) then
            call ccpp_error('Unable to retrieve x_wind_at_10m')
            return
        end if

        call ccpp_fields_get(cdata, 'y_wind_at_10m', v10m, ierr)
        if (ierr /= 0) then
            call ccpp_error('Unable to retrieve y_wind_at_10m')
            return
        end if

        call ccpp_fields_get(cdata, 'Monin-Obukhov_similarity_function_for_momentum', fm, ierr)
        if (ierr /= 0) then
            call ccpp_error('Unable to retrieve Monin-Obukhov_similarity_function_for_momentum')
            return
        end if

        call ccpp_fields_get(cdata, 'Monin-Obukhov_similarity_function_for_heat', fh, ierr)
        if (ierr /= 0) then
            call ccpp_error('Unable to retrieve Monin-Obukhov_similarity_function_for_heat')
            return
        end if

        call ccpp_fields_get(cdata, 'surface_skin_temperature', tsea, ierr)
        if (ierr /= 0) then
            call ccpp_error('Unable to retrieve surface_skin_temperature')
            return
        end if

        call ccpp_fields_get(cdata, 'kinematic_surface_upward_sensible_heat_flux', heat, ierr)
        if (ierr /= 0) then
            call ccpp_error('Unable to retrieve kinematic_surface_upward_sensible_heat_flux')
            return
        end if

        call ccpp_fields_get(cdata, 'kinematic_surface_upward_latent_heat_flux', evap, ierr)
        if (ierr /= 0) then
            call ccpp_error('Unable to retrieve kinematic_surface_upward_latent_heat_flux')
            return
        end if

        call ccpp_fields_get(cdata, 'surface_wind_stress', stress, ierr)
        if (ierr /= 0) then
            call ccpp_error('Unable to retrieve surface_wind_stress')
            return
        end if

        call ccpp_fields_get(cdata, 'wind_speed_at_lowest_model_layer', spd1, ierr)
        if (ierr /= 0) then
            call ccpp_error('Unable to retrieve wind_speed_at_lowest_model_layer')
            return
        end if

        call ccpp_fields_get(cdata, 'vertical_index_of_top_of_atmosphere_boundary_layer', kpbl, ierr)
        if (ierr /= 0) then
            call ccpp_error('Unable to retrieve vertical_index_of_top_of_atmosphere_boundary_layer')
            return
        end if

        call ccpp_fields_get(cdata, 'air_pressure_at_interface', prsi, ierr)
        if (ierr /= 0) then
            call ccpp_error('Unable to retrieve air_pressure_at_interface')
            return
        end if

        call ccpp_fields_get(cdata, 'air_pressure_difference_between_midlayers', del, ierr)
        if (ierr /= 0) then
            call ccpp_error('Unable to retrieve air_pressure_difference_between_midlayers')
            return
        end if

        call ccpp_fields_get(cdata, 'air_pressure', prsl, ierr)
        if (ierr /= 0) then
            call ccpp_error('Unable to retrieve air_pressure')
            return
        end if

        call ccpp_fields_get(cdata, 'dimensionless_exner_function', prslk, ierr)
        if (ierr /= 0) then
            call ccpp_error('Unable to retrieve dimensionless_exner_function')
            return
        end if

        call ccpp_fields_get(cdata, 'geopotential_at_interface', phii, ierr)
        if (ierr /= 0) then
            call ccpp_error('Unable to retrieve geopotential_at_interface')
            return
        end if

        call ccpp_fields_get(cdata, 'geopotential', phil, ierr)
        if (ierr /= 0) then
            call ccpp_error('Unable to retrieve geopotential')
            return
        end if

        call ccpp_fields_get(cdata, 'time_step_for_physics', delt, ierr)
        if (ierr /= 0) then
            call ccpp_error('Unable to retrieve time_step_for_physics')
            return
        end if

        call ccpp_fields_get(cdata, 'flag_TKE_dissipation_heating', dspheat, ierr)
        if (ierr /= 0) then
            call ccpp_error('Unable to retrieve flag_TKE_dissipation_heating')
            return
        end if

        call ccpp_fields_get(cdata, 'x_momentum_flux', dusfc, ierr)
        if (ierr /= 0) then
            call ccpp_error('Unable to retrieve x_momentum_flux')
            return
        end if

        call ccpp_fields_get(cdata, 'y_momentum_flux', dvsfc, ierr)
        if (ierr /= 0) then
            call ccpp_error('Unable to retrieve y_momentum_flux')
            return
        end if

        call ccpp_fields_get(cdata, 'surface_upward_sensible_heat_flux', dtsfc, ierr)
        if (ierr /= 0) then
            call ccpp_error('Unable to retrieve surface_upward_sensible_heat_flux')
            return
        end if

        call ccpp_fields_get(cdata, 'surface_upward_latent_heat_flux', dqsfc, ierr)
        if (ierr /= 0) then
            call ccpp_error('Unable to retrieve surface_upward_latent_heat_flux')
            return
        end if

        call ccpp_fields_get(cdata, 'atmosphere_boundary_layer_thickness', hpbl, ierr)
        if (ierr /= 0) then
            call ccpp_error('Unable to retrieve atmosphere_boundary_layer_thickness')
            return
        end if

        call ccpp_fields_get(cdata, 'countergradient_mixing_term_for_temperature', hgamt, ierr)
        if (ierr /= 0) then
            call ccpp_error('Unable to retrieve countergradient_mixing_term_for_temperature')
            return
        end if

        call ccpp_fields_get(cdata, 'countergradient_mixing_term_for_water_vapor', hgamq, ierr)
        if (ierr /= 0) then
            call ccpp_error('Unable to retrieve countergradient_mixing_term_for_water_vapor')
            return
        end if

        call ccpp_fields_get(cdata, 'atmosphere_heat_diffusivity', dkt, ierr)
        if (ierr /= 0) then
            call ccpp_error('Unable to retrieve atmosphere_heat_diffusivity')
            return
        end if

        call ccpp_fields_get(cdata, 'index_of_highest_temperature_inversion', kinver, ierr)
        if (ierr /= 0) then
            call ccpp_error('Unable to retrieve index_of_highest_temperature_inversion')
            return
        end if

        call ccpp_fields_get(cdata, 'atmosphere_momentum_diffusivity_background', xkzm_m, ierr)
        if (ierr /= 0) then
            call ccpp_error('Unable to retrieve atmosphere_momentum_diffusivity_background')
            return
        end if

        call ccpp_fields_get(cdata, 'atmosphere_heat_diffusivity_background', xkzm_h, ierr)
        if (ierr /= 0) then
            call ccpp_error('Unable to retrieve atmosphere_heat_diffusivity_background')
            return
        end if

        call ccpp_fields_get(cdata, 'diffusivity_background_sigma_level', xkzm_s, ierr)
        if (ierr /= 0) then
            call ccpp_error('Unable to retrieve diffusivity_background_sigma_level')
            return
        end if

        call ccpp_fields_get(cdata, 'flag_print', lprnt, ierr)
        if (ierr /= 0) then
            call ccpp_error('Unable to retrieve flag_print')
            return
        end if

        call ccpp_fields_get(cdata, 'horizontal_index_of_printed_column', ipr, ierr)
        if (ierr /= 0) then
            call ccpp_error('Unable to retrieve horizontal_index_of_printed_column')
            return
        end if

        call edmf_run(ix=ix,im=im,km=km,ntrac=ntrac,ntcw=ntcw,dv=dv,du=du,tau=tau,rtg=rtg,u1=u1,v1=v1,t1=t1,q1=q1,swh=swh,hlw=hlw,xmu=xmu,psk=psk,rbsoil=rbsoil,zorl=zorl,u10m=u10m,v10m=v10m,fm=fm,fh=fh,tsea=tsea,heat=heat,evap=evap,stress=stress,spd1=spd1,kpbl=kpbl,prsi=prsi,del=del,prsl=prsl,prslk=prslk,phii=phii,phil=phil,delt=delt,dspheat=dspheat,dusfc=dusfc,dvsfc=dvsfc,dtsfc=dtsfc,dqsfc=dqsfc,hpbl=hpbl,hgamt=hgamt,hgamq=hgamq,dkt=dkt,kinver=kinver,xkzm_m=xkzm_m,xkzm_h=xkzm_h,xkzm_s=xkzm_s,lprnt=lprnt,ipr=ipr)
    end subroutine edmf_run_cap

    subroutine edmf_finalize_cap(ptr) bind(c)

        type(c_ptr), intent(inout) :: ptr

        type(ccpp_t), pointer      :: cdata
        integer                    :: ierr


        call c_f_pointer(ptr, cdata)

        

        call edmf_finalize()
    end subroutine edmf_finalize_cap

    subroutine edmf_init_cap(ptr) bind(c)

        type(c_ptr), intent(inout) :: ptr

        type(ccpp_t), pointer      :: cdata
        integer                    :: ierr


        call c_f_pointer(ptr, cdata)

        

        call edmf_init()
    end subroutine edmf_init_cap
end module edmf_cap
