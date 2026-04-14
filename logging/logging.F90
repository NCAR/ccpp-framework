module marbl_logging

  ! ============
  ! Module Usage
  ! ============
  !
  ! Assume a variable named StatusLog (as appears in the marbl_interface_class)
  !
  ! -----------------------------------------------
  ! Use the following routines to write log entries
  ! -----------------------------------------------
  !
  ! (1) StatusLog%log_noerror -- this stores a log message in StatusLog that does
  !     not contain a fatal error
  ! (2) StatusLog%log_header -- this stores a log message in StatusLog that is
  !     meant to be read as a section header; e.g. StatusLog%log_header('HEADER',...)
  !     writes the following (including blank lines)
  !
  !     ------
  !     HEADER
  !     ------
  !
  ! (3) StatusLog%log_error -- this stores a log message in StatusLog that DOES
  !     contain a fatal error. It does this by setting StatusLog%labort_marbl =
  !     .true.; when a call from the GCM to MARBL returns, it is important for the
  !     GCM to check the value of StatusLog%labort_marbl and abort the run if an
  !     error has been reported.
  ! (4) StatusLog%log_error_trace -- this stores a log message in StatusLog
  !     detailing what subroutine was just called and where it was called from. It
  !     is meant to provide more information when trying to trace the path through
  !     the code that resulted in an error.
  !
  ! -----------------------------------------------
  ! Pseudo-code for writing StatusLog in the driver
  ! -----------------------------------------------
  !
  !  type(marbl_status_log_entry_type), pointer :: LogEntry
  !
  !  ! Set pointer to first entry of the log
  !  LogEntry => StatusLog%FullLog
  !
  !  do while (associated(LogEntry))
  !    ! If running in parallel, you may want to check if you are the master
  !    ! task or if LogEntry%lalltasks = .true.
  !    write(stdout,*) trim(LogEntry%LogMessage)
  !    LogEntry => LogEntry%next
  !  end do
  !
  !  ! Erase contents of log now that they have been written out
  !  call StatusLog%erase()
  !
  !  if (StatusLog%labort_marbl) then
  !    [GCM abort call: "error found in MARBL"]
  !  end if
  !

  use marbl_kinds_mod, only : char_len

  implicit none
  private
  save

  integer, parameter, private :: marbl_log_len = 2 * char_len

  !****************************************************************************

  type, public :: marbl_status_log_entry_type
    integer :: elementind = -1 ! ElementInd < 0 implies no location data
    logical :: lonly_master_writes ! True => message should be written to stdout
    !                  master task; False => all tasks
    character(len=marbl_log_len) :: logmessage ! Message text
    character(len=char_len) :: codelocation ! Information on where log was written

    type(marbl_status_log_entry_type), pointer :: next
  end type marbl_status_log_entry_type

  !****************************************************************************

  ! Note: this data type is not in use at the moment, but it is included as an
  !       initial step towards allowing the user some control over what types
  !       of messages are added to the log. For example, if you do not want
  !       the contents of namelists written to the log, you would simply set
  !
  !       lLogNamelist = .false.
  !
  !       In the future we hope to be able to set these options via namelist,
  !       but for now lLogNamelist, lLogGeneral, lLogWarning, and lLogError are
  !       all set to .true. and can not be changed without modifying the source
  !       code in this file.
  type, private :: marbl_log_output_options_type
    logical :: labort_on_warning ! True => elevate Warnings to Errors
    logical :: llogverbose ! Debugging output should be given Verbose label
    logical :: llognamelist ! Write namelists to log?
    logical :: lloggeneral ! General diagnostic output
    logical :: llogwarning ! Warnings (can be elevated to errors via labort_on_warning)
    logical :: llogerror ! Errors (will toggle labort_marbl whether log
    ! is written or not)
  contains
    procedure :: construct => marbl_output_options_constructor
  end type marbl_log_output_options_type

  !****************************************************************************

  type, public :: marbl_log_type
    logical, private :: lconstructed = .false. ! True => constructor was already called
    logical, public :: labort_marbl = .false. ! True => driver should abort GCM
    logical, public :: lwarning = .false. ! True => warnings are present
    type(marbl_log_output_options_type) :: outputoptions
    type(marbl_status_log_entry_type), pointer :: fulllog
    type(marbl_status_log_entry_type), pointer :: lastentry
  contains
    procedure, public :: construct => marbl_log_constructor
    procedure, public :: log_header => marbl_log_header
    procedure, public :: log_error => marbl_log_error
    procedure, public :: log_warning => marbl_log_warning
    procedure, public :: log_noerror => marbl_log_noerror
    procedure, public :: log_error_trace => marbl_log_error_trace
    procedure, public :: log_warning_trace => marbl_log_warning_trace
    procedure, public :: erase => marbl_log_erase
    procedure, private :: append_to_log
  end type marbl_log_type

  !****************************************************************************

contains

  !****************************************************************************

  subroutine marbl_output_options_constructor(this, labort_on_warning, logverbose, lognamelist, &
      loggeneral, logwarning, logerror)

    class(marbl_log_output_options_type), intent(inout) :: this
    logical, intent(in), optional :: labort_on_warning, logverbose, lognamelist
    logical, intent(in), optional :: loggeneral, logwarning, logerror

    if (present(labort_on_warning)) then
      this%labort_on_warning = labort_on_warning
    else
      this%labort_on_warning = .false.
    end if

    if (present(logverbose)) then
      this%llogverbose = logverbose
    else
      this%llogverbose = .false.
    end if

    if (present(lognamelist)) then
      this%llognamelist = lognamelist
    else
      this%llognamelist = .true.
    end if

    if (present(loggeneral)) then
      this%lloggeneral = loggeneral
    else
      this%lloggeneral = .true.
    end if

    if (present(logwarning)) then
      this%llogwarning = logwarning
    else
      this%llogwarning = .true.
    end if

    if (present(logerror)) then
      this%llogerror = logerror
    else
      this%llogerror = .true.
    end if

  end subroutine marbl_output_options_constructor

  !****************************************************************************

  subroutine marbl_log_constructor(this)

    class(marbl_log_type), intent(inout) :: this

    if (this%lconstructed) return
    this%lconstructed = .true.
    nullify(this%fulllog)
    nullify(this%lastentry)
    call this%outputoptions%construct()

  end subroutine marbl_log_constructor

  !****************************************************************************

  subroutine marbl_log_header(this, headermsg, codeloc)

    class(marbl_log_type), intent(inout) :: this
    ! StatusMsg is the message to be printed in the log; it does not need to
    !    contain the name of the module or subroutine producing the log message
    ! CodeLoc is the name of the subroutine that is calling StatusLog%log_noerror
    character(len=*), intent(in) :: headermsg, codeloc

    character(len=len_trim(headermsg)) :: dashes
    integer :: n

    do n = 1, len(dashes)
      dashes(n:n) = '-'
    end do
    call this%log_noerror('', codeloc)
    call this%log_noerror(dashes, codeloc)
    call this%log_noerror(headermsg, codeloc)
    call this%log_noerror(dashes, codeloc)
    call this%log_noerror('', codeloc)

  end subroutine marbl_log_header

  !****************************************************************************

  subroutine marbl_log_error(this, errormsg, codeloc, elemind)

    class(marbl_log_type), intent(inout) :: this
    ! ErrorMsg is the error message to be printed in the log; it does not need
    !     to contain the name of the module or subroutine triggering the error
    ! CodeLoc is the name of the subroutine that is calling StatusLog%log_error
    character(len=*), intent(in) :: errormsg, codeloc
    integer, optional, intent(in) :: elemind

    character(len=marbl_log_len) :: errormsg_loc ! Message text

    this%labort_marbl = .true.

    ! Only allocate memory and add entry if we want to log full namelist!
    if ( .not. this%outputoptions%llogerror) then
      return
    end if

    write(errormsg_loc, "(4A)") "MARBL ERROR (", trim(codeloc), "): ", &
        trim(errormsg)

    call this%append_to_log(errormsg_loc, codeloc, elemind, lonly_master_writes=.false.)

  end subroutine marbl_log_error

  !****************************************************************************

  subroutine marbl_log_warning(this, warningmsg, codeloc, elemind)

    class(marbl_log_type), intent(inout) :: this
    ! WarningMsg is the message to be printed in the log; it does not need to
    !    contain the name of the module or subroutine producing the log message
    ! CodeLoc is the name of the subroutine that is calling StatusLog%log_warning
    character(len=*), intent(in) :: warningmsg, codeloc
    integer, optional, intent(in) :: elemind

    character(len=marbl_log_len) :: warningmsg_loc ! Message text

    this%lwarning = .true.

    ! Only allocate memory and add entry if we want to log full namelist!
    if ( .not. this%outputoptions%llogwarning) then
      return
    end if

    write(warningmsg_loc, "(4A)") "MARBL WARNING (", trim(codeloc), "): ", &
        trim(warningmsg)

    call this%append_to_log(warningmsg_loc, codeloc, elemind, lonly_master_writes=.false.)

  end subroutine marbl_log_warning

  !****************************************************************************

  subroutine marbl_log_noerror(this, statusmsg, codeloc, elemind, lonly_master_writes)

    class(marbl_log_type), intent(inout) :: this
    ! StatusMsg is the message to be printed in the log; it does not need to
    !    contain the name of the module or subroutine producing the log message
    ! CodeLoc is the name of the subroutine that is calling StatusLog%log_noerror
    character(len=*), intent(in) :: statusmsg, codeloc
    integer, optional, intent(in) :: elemind
    ! If lonly_master_writes is .false., then this is a message that should be
    ! printed out regardless of which task produced it. By default, MARBL assumes
    ! that only the master task needs to print a message
    logical, optional, intent(in) :: lonly_master_writes

    ! Only allocate memory and add entry if we want to log full namelist!
    if ( .not. this%outputoptions%lloggeneral) then
      return
    end if

    call this%append_to_log(statusmsg, codeloc, elemind, lonly_master_writes)

  end subroutine marbl_log_noerror

  !****************************************************************************

  subroutine append_to_log(this, statusmsg, codeloc, elemind, lonly_master_writes)

    class(marbl_log_type), intent(inout) :: this
    ! StatusMsg is the message to be printed in the log; it does not need to
    !    contain the name of the module or subroutine producing the log message
    ! CodeLoc is the name of the subroutine that is calling StatusLog%log_noerror
    character(len=*), intent(in) :: statusmsg, codeloc
    integer, optional, intent(in) :: elemind
    ! If lonly_master_writes is .false., then this is a message that should be
    ! printed out regardless of which task produced it. By default, MARBL assumes
    ! that only the master task needs to print a message
    logical, optional, intent(in) :: lonly_master_writes
    type(marbl_status_log_entry_type), pointer :: new_entry

    allocate(new_entry)
    nullify(new_entry%next)
    if (present(elemind)) then
      new_entry%elementind = elemind
    else
      new_entry%elementind = -1
    end if
    new_entry%logmessage = trim(statusmsg)
    new_entry%codelocation = trim(codeloc)
    if (present(lonly_master_writes)) then
      new_entry%lonly_master_writes = lonly_master_writes
    else
      new_entry%lonly_master_writes = .true.
    end if

    if (associated(this%fulllog)) then
      ! Append new entry to last entry in the log
      this%lastentry%next => new_entry
    else
      this%fulllog => new_entry
    end if
    ! Update LastEntry attribute of linked list
    this%lastentry => new_entry

  end subroutine append_to_log

  !****************************************************************************

  subroutine marbl_log_error_trace(this, routinename, codeloc, elemind)

    ! This routine should only be called if another subroutine has returned and
    ! StatusLog%labort_marbl = .true.

    class(marbl_log_type), intent(inout) :: this
    ! RoutineName is the name of the subroutine that returned with
    !             labort_marbl = .true.
    ! CodeLoc is the name of the subroutine that is calling StatusLog%log_error_trace
    !
    ! Log will contain a message along the lines of
    !
    ! "(CodeLoc) Error reported from RoutineName"
    !
    ! When the log is printed, this will provide a traceback through the sequence
    ! of calls that led to the original error message.
    character(len=*), intent(in) :: routinename, codeloc
    integer, optional, intent(in) :: elemind
    character(len=char_len) :: log_message

    write(log_message, "(2A)") "Error reported from ", trim(routinename)
    call this%log_error(log_message, codeloc, elemind)

  end subroutine marbl_log_error_trace

  !****************************************************************************

  subroutine marbl_log_warning_trace(this, routinename, codeloc, elemind)

    ! This routine should only be called if another subroutine has returned and
    ! StatusLog%lwarning = .true.

    class(marbl_log_type), intent(inout) :: this
    ! RoutineName is the name of the subroutine that returned with
    !             lwarning = .true.
    ! CodeLoc is the name of the subroutine that is calling StatusLog%log_warning_trace
    !
    ! Log will contain a message along the lines of
    !
    ! "(CodeLoc) Warning reported from RoutineName"
    !
    ! When the log is printed, this will provide a traceback through the sequence
    ! of calls that led to the original warning message.
    character(len=*), intent(in) :: routinename, codeloc
    integer, optional, intent(in) :: elemind
    character(len=char_len) :: log_message

    write(log_message, "(2A)") "Warning reported from ", trim(routinename)
    call this%log_warning(log_message, codeloc, elemind)
    this%lwarning = .false.

  end subroutine marbl_log_warning_trace

  !****************************************************************************

  subroutine marbl_log_erase(this)

    class(marbl_log_type), intent(inout) :: this
    type(marbl_status_log_entry_type), pointer :: tmp

    do while (associated(this%fulllog))
      tmp => this%fulllog%next
      deallocate(this%fulllog)
      this%fulllog => tmp
    end do
    nullify(this%fulllog)
    nullify(this%lastentry)

    this%lwarning = .false.

  end subroutine marbl_log_erase

end module marbl_logging
