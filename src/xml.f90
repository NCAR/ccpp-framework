!>
!! @brief XML functions and subroutines module.
!!
!! @details The XML module provides functions and 
!!          subroutines for accessing the C versions
!!          of the functions.
!
module xml

    use, intrinsic :: iso_c_binding

    implicit none

    private
    public :: xml_load,                                              &
              xml_unload,                                            &
              xml_ele_find,                                          &
              xml_ele_next,                                          &
              xml_ele_contents,                                      &
              xml_ele_count,                                         &
              xml_ele_att

    interface
       integer(c_int32_t)                                             &
       function xml_load(filename, xml, root)                         &
                bind(c, name='xml_load')
           import :: c_int32_t, c_char, c_ptr
           character(kind=c_char), dimension(*) :: filename
           type(c_ptr)                          :: xml
           type(c_ptr)                          :: root
       end function xml_load

       integer(c_int32_t)                                             &
       function xml_unload(xml)                                       &
                bind(c, name='xml_unload')
           import :: c_int32_t, c_ptr
           type(c_ptr)                          :: xml
       end function xml_unload

       integer(c_int32_t)                                             &
       function xml_ele_find(xml, name, ele)                          &
                bind(c, name='xml_ele_find')
           import :: c_int32_t, c_ptr, c_char
           type(c_ptr)                          :: xml
           character(kind=c_char), dimension(*) :: name
           type(c_ptr)                          :: ele
       end function xml_ele_find

       integer(c_int32_t)                                             &
       function xml_ele_next(xml, name, ele)                          &
                bind(c, name='xml_ele_next')
           import :: c_int32_t, c_ptr, c_char
           type(c_ptr)                          :: xml
           character(kind=c_char), dimension(*) :: name
           type(c_ptr)                          :: ele
       end function xml_ele_next

       integer(c_int32_t)                                             &
       function xml_ele_contents(xml, val)                            &
                bind(c, name='xml_ele_contents')
           import :: c_int32_t, c_ptr, c_char
           type(c_ptr)                          :: xml
           type(c_ptr)                          :: val
       end function xml_ele_contents

       integer(c_int32_t)                                             &
       function xml_ele_count(xml, name, n)                           &
                bind(c, name='xml_ele_count')
           import :: c_int32_t, c_ptr, c_char
           type(c_ptr)                          :: xml
           character(kind=c_char), dimension(*) :: name
           integer(c_int32_t)                   :: n
       end function xml_ele_count

       integer(c_int32_t)                                             &
       function xml_ele_att(node, name, val)                          &
                bind(c, name='xml_ele_att')
           import :: c_int32_t, c_ptr, c_char
           type(c_ptr)                          :: node
           character(kind=c_char), dimension(*) :: name
           type(c_ptr)                          :: val
       end function xml_ele_att

   end interface

end module xml
