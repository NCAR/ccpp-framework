!>
!! @brief Physics infrastructure module.
!
module phy_infra

   use, intrinsic :: iso_c_binding
   use            :: kinds,                                            &
                     only: i_sp, r_dp
   use            :: types,                                            &
                     only: STR_LEN, suite_t, ipd_t, subcycle_t,        &
                           aip_t, field_t
   use            :: strings,                                          &
                     only: fstr, cstr
   use            :: xml
   implicit none

   private
   public :: phy_init

   !>
   !! @brief Suite XML tags.
   !!
   !! @details These suite xml tags must match the elements and attributes
   !!          of the suite.xsd.
   !
   character(len=STR_LEN), parameter :: XML_ELE_SUITE    = "suite"
   character(len=STR_LEN), parameter :: XML_ELE_IPD      = "ipd"
   character(len=STR_LEN), parameter :: XML_ELE_SUBCYCLE = "subcycle"
   character(len=STR_LEN), parameter :: XML_ELE_SCHEME   = "scheme"
   character(len=STR_LEN), parameter :: XML_ATT_NAME     = "name"
   character(len=STR_LEN), parameter :: XML_ATT_PART     = "part"
   character(len=STR_LEN), parameter :: XML_ATT_LOOP     = "loop"

   contains

   !>
   !! Physics infrastructure initialization subroutine.
   !!
   !! @param[in]    filename The file name of the XML scheme file to load.
   !! @param[inout] suite    The suite_t type to initalize from the scheme
   !!                        XML file.
   !
   subroutine phy_init(filename, suite)
        implicit none

        character(len=*),       intent(in)    :: filename
        type(suite_t),          intent(inout) :: suite

        integer                               :: ierr
        integer                               :: i
        integer                               :: j
        integer                               :: k
        integer                               :: n
        integer                               :: max_num_schemes_per_ipd_call
        integer                               :: num_schemes_this_ipd_call
        type(c_ptr)                           :: xml
        type(c_ptr)                           :: root
        type(c_ptr)                           :: ipd
        type(c_ptr)                           :: subcycle
        type(c_ptr)                           :: scheme
        character(kind=c_char), target        :: tmp(STR_LEN)

        ierr = 0
        max_num_schemes_per_ipd_call = 0

        ! Load the xml document.
        ierr = xml_load(cstr(filename), xml, root)
        if (ierr /= 0) then
            print *, 'Error loading suite: ', trim(filename)
        end if

        ! Get the suite name
        ierr = xml_ele_att(root, cstr(XML_ATT_NAME), c_loc(tmp))
        suite%name = fstr(tmp)

        ! Count the number of IPDs
        ierr = xml_ele_count(root, cstr(XML_ELE_IPD), suite%ipds_max)
        allocate(suite%ipds(suite%ipds_max), stat=ierr)

        ! Find the first IPD
        ierr = xml_ele_find(root, cstr(XML_ELE_IPD), ipd)

        ! Loop over all IPDs
        do i=1, suite%ipds_max
            ! Get the part number
            ierr = xml_ele_att(ipd, cstr(XML_ATT_PART), c_loc(tmp))
            read(tmp,*,iostat=ierr) suite%ipds(i)%part

            ! Count the number of subcycles in this IPD
            ierr = xml_ele_count(ipd, cstr(XML_ELE_SUBCYCLE), n)
            suite%ipds(i)%subcycles_max = n
            allocate(suite%ipds(i)%subcycles(n), stat=ierr)

            ! Count the number of scheme calls for this IPD
            num_schemes_this_ipd_call = 0

            ! Find the first subcycle
            ierr = xml_ele_find(ipd, cstr(XML_ELE_SUBCYCLE), subcycle)

            ! Loop over all subcycles
            do j=1, suite%ipds(i)%subcycles_max
                ! Get the subcycle loop number
                ierr = xml_ele_att(subcycle, cstr(XML_ATT_LOOP), c_loc(tmp))
                read(tmp,*,iostat=ierr) suite%ipds(i)%subcycles(j)%loop

                ! Count the number of schemes
                ierr = xml_ele_count(subcycle, cstr(XML_ELE_SCHEME), n)
                suite%ipds(i)%subcycles(j)%schemes_max = n
                allocate(suite%ipds(i)%subcycles(j)%schemes(n), stat=ierr)
                num_schemes_this_ipd_call = n
                max_num_schemes_per_ipd_call = MAX ( max_num_schemes_per_ipd_call , num_schemes_this_ipd_call )

                ! Find the first scheme
                ierr = xml_ele_find(subcycle, cstr(XML_ELE_SCHEME), scheme)

                ! Loop over all scheme
                do k=1, suite%ipds(i)%subcycles(j)%schemes_max
                    ierr = xml_ele_contents(scheme, c_loc(tmp))
                    suite%ipds(i)%subcycles(j)%schemes(k) = fstr(tmp)
                    ! Find the next scheme
                    ierr = xml_ele_next(scheme, cstr(XML_ELE_SCHEME), scheme)
                end do
                ! Find the next subcycle
                ierr = xml_ele_next(ipd, cstr(XML_ELE_SUBCYCLE), subcycle)
            end do
            ! Find the next IPD
            ierr = xml_ele_next(ipd, cstr(XML_ELE_IPD), ipd)
        end do

        ! Save max number of schemes that appear in any single IPD call
        !suite%total_schemes_max = max_num_schemes_per_ipd_call

        write(*, '(A, A)') 'Suite name: ', trim(suite%name)
        write(*, '(A, I4)') 'IPDs: ', suite%ipds_max

        do i=1, suite%ipds_max
            write(*, '(A, I4, A, I4)') 'IPD: ', i, ' part: ', suite%ipds(i)%part
            write(*, '(A, I4)') 'subcycles: ',  suite%ipds(i)%subcycles_max
            do j=1, suite%ipds(i)%subcycles_max
                write(*, '(A, I4, A, I4)') 'subcycle: ', j, ' loop: ', suite%ipds(i)%subcycles(j)%loop
                write(*, '(A, I4)') 'schemes: ', suite%ipds(i)%subcycles(j)%schemes_max
                do k=1, suite%ipds(i)%subcycles(j)%schemes_max
                    write(*, '(A, A)') 'scheme: ', trim(suite%ipds(i)%subcycles(j)%schemes(k))
                end do
            end do
        end do

        ierr = xml_unload(xml)

   end subroutine phy_init

end module phy_infra
