! Test parameterization with no vertical level
!

MODULE temp_adjust

  USE ccpp_kinds, ONLY: kind_phys

  IMPLICIT NONE
  PRIVATE

  PUBLIC :: temp_adjust_init
  PUBLIC :: temp_adjust_run
  PUBLIC :: temp_adjust_finalize
  PUBLIC :: dyn_consts

CONTAINS

  !> \section arg_table_temp_adjust_run  Argument Table
  !! \htmlinclude arg_table_temp_adjust_run.html
  !!
  subroutine temp_adjust_run(foo, timestep, temp_prev, temp_layer, qv, ps,    &
       errmsg, errflg)

    integer,            intent(in)    :: foo
    real(kind_phys),    intent(in)    :: timestep
    real(kind_phys),    intent(inout) :: qv(:)
    real(kind_phys),    intent(inout) :: ps(:)
    REAL(kind_phys),    intent(in)    :: temp_prev(:)
    REAL(kind_phys),    intent(inout) :: temp_layer(foo)
    character(len=512), intent(out)   :: errmsg
    integer,            intent(out)   :: errflg
    !----------------------------------------------------------------

    integer :: col_index

    errmsg = ''
    errflg = 0

    do col_index = 1, foo
       temp_layer(col_index) = temp_layer(col_index) + temp_prev(col_index)
       qv(col_index) = qv(col_index) + 1.0_kind_phys
    end do

  END SUBROUTINE temp_adjust_run

  !> \section arg_table_temp_adjust_init  Argument Table
  !! \htmlinclude arg_table_temp_adjust_init.html
  !!
  subroutine temp_adjust_init (errmsg, errflg)

    character(len=512),      intent(out)   :: errmsg
    integer,                 intent(out)   :: errflg

    ! This routine currently does nothing

    errmsg = ''
    errflg = 0

  end subroutine temp_adjust_init

  !> \section arg_table_temp_adjust_finalize  Argument Table
  !! \htmlinclude arg_table_temp_adjust_finalize.html
  !!
  subroutine temp_adjust_finalize (errmsg, errflg)

    character(len=512),      intent(out)   :: errmsg
    integer,                 intent(out)   :: errflg

    ! This routine currently does nothing

    errmsg = ''
    errflg = 0

  end subroutine temp_adjust_finalize

  subroutine some_subroutine()
  contains
     subroutine dyn_consts(dyn_const, errcode, errmsg)
         use ccpp_constituent_prop_mod, only: ccpp_constituent_properties_t
         type(ccpp_constituent_properties_t), allocatable, intent(out) :: dyn_const(:)
         integer,                             intent(out) :: errcode
         character(len=512),                  intent(out) :: errmsg

         errmsg = ''
         errcode = 0
         allocate(dyn_const(1), stat=errcode)
         if (errcode /= 0) then
            errmsg = 'Error allocating dyn_const in dyn_consts'
         end if
         call dyn_const(1)%instantiate(std_name="dyn_const1", long_name='dyn const1', &
              units='kg kg-1', default_value=1._kind_phys,                            &
              vertical_dim='vertical_layer_dimension', advected=.true.,               &
              errcode=errcode, errmsg=errmsg)

      end subroutine dyn_consts

   end subroutine some_subroutine


END MODULE temp_adjust
