!Test unit conversions for intent in, inout, out variables
!

module effr_calc

   use ccpp_kinds, only: kind_phys

   implicit none
   private

   public :: effr_calc_run, effr_calc_init

 contains
   !> \section arg_table_effr_calc_init Argument Table
   !! \htmlinclude arg_table_effr_calc_init.html
   !!
   subroutine effr_calc_init(scheme_order, errmsg, errflg)
     character(len=512), intent(out)   :: errmsg
     integer,            intent(out)   :: errflg
     integer,            intent(inout) :: scheme_order

     errmsg = ''
     errflg = 0

     if (scheme_order .ne. 2) then
        errflg = 1
        errmsg = 'ERROR: effr_calc_init() needs to be called second'
        return
     else
        scheme_order = scheme_order + 1
     endif

   end subroutine effr_calc_init

   !> \section arg_table_effr_calc_run  Argument Table
   !! \htmlinclude arg_table_effr_calc_run.html
   !!
   subroutine effr_calc_run(ncol, nlev, effrr_in, effrg_in, ncg_in, nci_out, &
                            effrl_inout, effri_out, effrs_inout, ncl_out,    &
                            has_graupel, scalar_var, errmsg, errflg)

      integer,            intent(in)    :: ncol
      integer,            intent(in)    :: nlev
      real(kind_phys),    intent(in)    :: effrr_in(:,:)
      real(kind_phys),    intent(in),optional    :: effrg_in(:,:)
      real(kind_phys),    intent(in),optional    :: ncg_in(:,:)
      real(kind_phys),    intent(out),optional   :: nci_out(:,:)
      real(kind_phys),    intent(inout) :: effrl_inout(:,:)
      real(kind_phys),    intent(out),optional   :: effri_out(:,:)
      real(8),intent(inout) :: effrs_inout(:,:)
      logical,            intent(in)    :: has_graupel
      real(kind_phys),    intent(inout) :: scalar_var
      character(len=512), intent(out)   :: errmsg
      integer,            intent(out)   :: errflg
      real(kind_phys),    intent(out),optional   :: ncl_out(:,:)
      !----------------------------------------------------------------

      real(kind_phys), parameter :: re_qc_min = 2.5 ! microns
      real(kind_phys), parameter :: re_qc_max = 50. ! microns
      real(kind_phys), parameter :: re_qi_avg = 75. ! microns
      real(kind_phys) :: effrr_local(ncol,nlev)
      real(kind_phys) :: effrg_local(ncol,nlev)
      real(kind_phys) :: ncg_in_local(ncol,nlev)
      real(kind_phys) :: nci_out_local(ncol,nlev)

      errmsg = ''
      errflg = 0

      effrr_local = effrr_in
      if (present(effrg_in)) effrg_local = effrg_in
      if (present(ncg_in)) ncg_in_local = ncg_in
      if (present(nci_out)) nci_out_local = nci_out
      effrl_inout = min(max(effrl_inout,re_qc_min),re_qc_max)
      if (present(effri_out)) effri_out   = re_qi_avg
      effrs_inout = effrs_inout + (10.0 / 6.0) ! in micrometer
      scalar_var = 2.0 ! in km

   end subroutine effr_calc_run

end module effr_calc
