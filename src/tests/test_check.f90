!>
!! @brief A Test Atmospheric Driver Program.
!!
program test_check

    use, intrinsic :: iso_c_binding,                                   &
                      only: c_loc, c_f_pointer
    use            :: ccpp_types,                                      &
                      only: STR_LEN, ccpp_t
    use            :: ccpp,                                            &
                      only: ccpp_init
    use            :: ccpp_fcall,                                      &
                      only: ccpp_run
    use            :: ccpp_fields,                                     &
                      only: ccpp_fields_add

    implicit none

    type(ccpp_t), target                         :: cdata
    character(len=STR_LEN)                       :: filename
    integer                                      :: len
    integer                                      :: ierr
    integer                                      :: ipd_loop, phys_loop
    integer                                      :: asize
    real, target                                 :: gravity
    real, target, allocatable, dimension(:)      :: surf_t
    real, target, allocatable, dimension(:,:,:)  :: u
    real, target, allocatable, dimension(:,:,:)  :: v

    ierr = 0

    call get_command_argument(1, filename, len, ierr)
    if (ierr /= 0) then
            call exit(1)
    end if

    ! Allocate the data
    asize = 5
    allocate(surf_t(asize), stat=ierr)
    if (ierr /= 0) then
            print *, 'Unable to allocate surface temperature array'
            call exit(1)
    end if

    allocate(u(asize,asize,asize), stat=ierr)
    if (ierr /= 0) then
            print *, 'Unable to allocate U array'
            call exit(1)
    end if

    allocate(v(asize,asize,asize), stat=ierr)
    if (ierr /= 0) then
            print *, 'Unable to allocate U array'
            call exit(1)
    end if

    ! Generate data to pass into a physics driver
    gravity = 9.80665
    surf_t = [290.0, 291.0, 292.0, 293.0, 294.0]
    u = 0.0
    v = 10.0

    ! Initalize the CCPP (with the filename of the suite to load).
    call ccpp_init(filename, cdata, ierr)
    if (ierr /= 0) then
            call exit(1)
    end if

    ! Add all the fields we want to expose to the physics driver.
    call ccpp_fields_add(cdata, 'gravity', gravity, ierr, 'm s-2')
    if (ierr /= 0) then
            call exit(1)
    end if

    call ccpp_fields_add(cdata, 'surface_temperature', surf_t, ierr, 'K')

    call ccpp_fields_add(cdata, 'eastward_wind', u, ierr, 'm s-1')

    call ccpp_fields_add(cdata, 'northward_wind', v, ierr, 'm s-1')

    call ccpp_run(cdata%suite%ipds(1)%subcycles(1)%schemes(1), cdata, ierr)

    print *, 'In test dummy main'
    print *, 'gravity: ', gravity
    print *, 'surf_t:  ', surf_t(1:2)
    print *, 'u: ', u(1,1,1)
    print *, 'v: ', v(1,1,1)

end program test_check
