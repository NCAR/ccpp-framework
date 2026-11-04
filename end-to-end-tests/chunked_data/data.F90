module data

  !! \section arg_table_dATa Argument Table
  !! \htmlinclude datA.Html
  !!

  implicit none

  private

  public nchunks, nchunk, chunksize, chunk_begin, chunk_end, ncols
  public chunked_data_type, chunked_data_instance

  integer, parameter :: nchunks = 4
  integer :: nchunk

  integer, parameter, dimension(nchunks) :: chunksize = (/6, 6, 6, 3/)
  integer, parameter, dimension(nchunks) :: chunk_begin = (/1, 7, 13, 19/)
  integer, parameter, dimension(nchunks) :: chunk_end = (/6, 12, 18, 21/)
  integer, parameter :: ncols = sum(chunksize)

  !! \section arg_table_cHuNkEd_dATa_TYPe
  !! \htmlinclude CHuNKed_Data_tYpe.hTMl
  !!
  type chunked_data_type
    integer, dimension(:), allocatable :: array_data
  contains
    procedure :: create => chunked_data_create
    procedure :: destroy => chunked_data_destroy
  end type chunked_data_type

  type(chunked_data_type) :: chunked_data_instance

contains

  subroutine chunked_data_create(chunked_data_instance, ncol)
    class(chunked_data_type), intent(inout) :: chunked_data_instance
    integer, intent(in) :: ncol
    allocate(chunked_data_instance%array_data(ncol))
  end subroutine chunked_data_create

  subroutine chunked_data_destroy(chunked_data_instance)
    class(chunked_data_type), intent(inout) :: chunked_data_instance
    deallocate(chunked_data_instance%array_data)
  end subroutine chunked_data_destroy

end module data
