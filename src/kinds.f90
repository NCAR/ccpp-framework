module kinds

    use, intrinsic :: iso_fortran_env

    implicit none

    private
    public  :: i_sp, i_dp,      &
               r_sp, r_dp, r_qp

    integer, parameter :: i_sp = INT32
    integer, parameter :: i_dp = INT64

    integer, parameter :: r_sp = REAL32
    integer, parameter :: r_dp = REAL64
    integer, parameter :: r_qp = REAL128

end module kinds

