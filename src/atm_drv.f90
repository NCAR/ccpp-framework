!>
!! @brief Atmospheric Driver Program
!!
program atm_drv

    use, intrinsic :: iso_c_binding,                       &
                      only: c_loc, c_f_pointer
    use            :: kinds,                               &
                      only: i_sp, r_dp
    use            :: types,                               &
                      only: STR_LEN, aip_t
    use            :: ipd,                                 &
                      only: ipd_init, ipd_run
    use            :: phy_fields,                          &
                      only: phy_field_init, phy_field_add, &
                            phy_field_sort

    implicit none

    type(aip_t), target                                    :: ap_data
    character(len=STR_LEN)                                 :: filename
    integer                                                :: len
    integer                                                :: ierr
    integer                                                :: ipd_loop, phys_loop
    integer                                                :: asize

    real(kind=r_dp),                                target :: gravity
    real(kind=r_dp), allocatable, dimension(:),     target :: surf_t
    real(kind=r_dp), allocatable, dimension(:,:,:), target :: u
    real(kind=r_dp), allocatable, dimension(:,:,:), target :: v

    ierr = 0

    call get_command_argument(1, filename, len, ierr)
    if (ierr /= 0) then
            print *, 'Error: no suite XML file specified.'
            stop
    end if

    ! Allocate the data
    asize = 5
    allocate(surf_t(asize), stat=ierr)
    if (ierr /= 0) then
            print *, 'Unable to allocate surface temperature array'
    end if

    allocate(u(asize,asize,asize), stat=ierr)
    if (ierr /= 0) then
            print *, 'Unable to allocate U array'
    end if

    allocate(v(asize,asize,asize), stat=ierr)
    if (ierr /= 0) then
            print *, 'Unable to allocate U array'
    end if

    ! Generate data to pass into a physics driver
    gravity = 9.80665_r_dp
    surf_t = [290.0_r_dp, 291.0_r_dp, 292.0_r_dp, 293.0_r_dp, 294.0_r_dp]
    u = 0.0_r_dp
    v = 10.0_r_dp

    ! Fill in the ap_data
    call phy_field_init(ap_data, 4)

    call phy_field_add(ap_data, 'gravity', 'm s-1', c_loc(gravity))

    call phy_field_add(ap_data, 'surface_temperature', 'K',            &
                       c_loc(surf_t), size(surf_t), shape(surf_t))

    call phy_field_add(ap_data, 'eastward_wind', 'm s-1',              &
                       c_loc(u), size(u), shape(u))

    call phy_field_add(ap_data, 'northward_wind', 'm s-1',             &
                       c_loc(v), size(v), shape(v))

    call phy_field_sort(ap_data)

    call ipd_init(filename, ap_data%suite)

    do ipd_loop = 1 , ap_data%suite%ipds_max
        ap_data%suite%ipd_n = ipd_loop
        call ipd_run(ap_data)
    end do

    print *, 'In ATM DRIVER'
    print *, 'gravity: ', gravity
    print *, 'surf_t:  ', surf_t(1:2)
    print *, 'u: ', u(1,1,1)
    print *, 'v: ', v(1,1,1)

end program atm_drv
