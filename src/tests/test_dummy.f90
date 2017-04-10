!>
!! @brief A Test Atmospheric Driver Program.
!!
program test_dummy

    use, intrinsic :: iso_c_binding,                                   &
                      only: c_loc, c_f_pointer
    use            :: ccpp_types,                                      &
                      only: STR_LEN, ccpp_t
    use            :: ccpp,                                            &
                      only: ccpp_init
    use            :: ccpp_ipd,                                        &
                      only: ccpp_ipd_run
    use            :: ccpp_fields,                                     &
                      only: ccpp_fields_add

    implicit none

    type(ccpp_t), target                        :: cdata
    character(len=STR_LEN)                      :: filename
    integer                                     :: len
    integer                                     :: ierr
    integer                                     :: ipd_loop, phys_loop
    integer                                     :: asize

    real, target                                 :: gravity
    real, target, allocatable, dimension(:)      :: surf_t
    real, target, allocatable, dimension(:,:,:)  :: u
    real, target, allocatable, dimension(:,:,:)  :: v

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
    gravity = 9.80665
    surf_t = [290.0, 291.0, 292.0, 293.0, 294.0]
    u = 0.0
    v = 10.0

    ! Initalize the CCPP (with the filename of the suite to load).
    call ccpp_init(filename, cdata, ierr)

    ! Add all the fields we want to expose to the physics driver.
    call ccpp_fields_add(cdata, 'gravity', 'm s-2', gravity, ierr)

    call ccpp_fields_add(cdata, 'surface_temperature', 'K', surf_t, ierr)

    call ccpp_fields_add(cdata, 'eastward_wind', 'm s-1', u, ierr)

    call ccpp_fields_add(cdata, 'northward_wind', 'm s-1', v, ierr)

    do ipd_loop = 1, cdata%suite%ipds_max
        cdata%suite%ipd_n = ipd_loop
        call ccpp_ipd_run(cdata)
    end do

    print *, 'In test dummy main'
    print *, 'gravity: ', gravity
    print *, 'surf_t:  ', surf_t(1:2)
    print *, 'u: ', u(1,1,1)
    print *, 'v: ', v(1,1,1)

end program test_dummy
