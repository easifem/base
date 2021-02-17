! This document belongs to easifem
! Copyright (c) 2020-2021, Vikas Sharma, Ph. D.
! All rights reserved.
!---------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! This module contains error handling tools
MODULE ErrorHandling
USE GlobalData
USE Display_Method
IMPLICIT NONE
PRIVATE

INTEGER( I4B ), PARAMETER, PUBLIC :: IO_OPEN = 1
  !! Constant for file open used by fErr
INTEGER( I4B ), PARAMETER, PUBLIC :: IO_READ = 2
  !! Constant for file read used by fErr
INTEGER( I4B ), PARAMETER, PUBLIC :: IO_WRITE = 3
  !! Constant for file write used by fErr
INTEGER( I4B ), PARAMETER, PUBLIC :: IO_CLOSE = 4
  !! Constant for file close used by fErr
INTEGER( I4B ), PARAMETER, PUBLIC :: OPT_ALLOC = 1
INTEGER( I4B ), PARAMETER, PUBLIC :: OPT_DEALLOC = 2

PUBLIC :: ErrorMSG, WarningMSG, FileError, AllocationErr

CONTAINS
!----------------------------------------------------------------------------
!                                                                 ErrorMsg
!----------------------------------------------------------------------------

SUBROUTINE ErrorMSG( Msg, File, Routine, Line, UnitNo )
  !! Write a message
  CHARACTER( LEN = * ), INTENT( IN ) :: Msg, File, Routine
  !! Message to write
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: UnitNo
  !! file id to write the message to
  INTEGER( I4B ), INTENT( IN ) :: Line
  !! Line number

  INTEGER( I4B ) :: Unit_No

  IF( PRESENT( UnitNo ) ) THEN
    Unit_No = UnitNo
  ELSE
    Unit_No = stdout
  END IF

  CALL Display( FILE, "Error:: In File ", UnitNo = Unit_No )
  CALL Display( LINE, "Line number = ", UnitNo = Unit_No )
  CALL Display( " ", "Routine named "// TRIM(Routine) // &
    & " has ERROR with message ", UnitNo = Unit_No )
  CALL Display(  Msg, UnitNo = Unit_No )
END SUBROUTINE ErrorMSG

!----------------------------------------------------------------------------
!                                                                WarningMSG
!----------------------------------------------------------------------------

SUBROUTINE WarningMSG( Msg, File, Routine, Line, UnitNo )
  !! This subroutine prints the warning message
  CHARACTER( LEN = * ), INTENT( IN ) :: Msg, File, Routine
  !! Message to write
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: UnitNo
  !! file id to write the message to
  INTEGER( I4B ), INTENT( IN ) :: Line
  !! Line number

  INTEGER( I4B ) :: Unit_No

  IF( PRESENT( UnitNo ) ) THEN
    Unit_No = UnitNo
  ELSE
    Unit_No = stdout
  END IF

  CALL Display( FILE, "Warning:: In File ", UnitNo = Unit_No )
  CALL Display( LINE, "Line number = ", UnitNo = Unit_No )
  CALL Display( " ", "Routine named "// TRIM(Routine) // &
    & " has ERROR with message ", UnitNo = Unit_No )
  CALL Display(  Msg, UnitNo = Unit_No )
END SUBROUTINE WarningMSG

!----------------------------------------------------------------------------
!                                                                 FileError
!----------------------------------------------------------------------------

SUBROUTINE FileError(istat, fname, flg, UnitNo, File, Routine, Line )
  !! Checks for a file error
  INTEGER( I4B ), INTENT( IN ) :: istat
    !! Result of iostat=istat for open,read,write,close
  CHARACTER(len=*), INTENT( IN ) :: fname
    !! Name of the file
  INTEGER( I4B ), INTENT( IN ) :: flg
    !! IO_OPEN=Open, IO_READ=Read, IO_WRITE=Write, IO_CLOSE=Close
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: UnitNo
    !! file id to write the error to
  CHARACTER( LEN = * ), INTENT( IN ) :: File, Routine
    !! Filename and routine name
  INTEGER( I4B ), INTENT( IN ) :: Line

  ! Define internal variables
  CHARACTER(len=:),allocatable :: Amsg
  INTEGER( I4B ) :: iunit

  IF ( istat == 0 ) RETURN

  IF( PRESENT( UnitNo ) ) THEN
    iunit = UnitNo
  ELSE
    iunit = stdout
  END IF

  Amsg=''

  SELECT CASE(flg)
  CASE(IO_OPEN)
    Amsg='Opening file: '//trim(fname)
  CASE(IO_READ)
    Amsg='Reading from: '//trim(fname)
  CASE(IO_WRITE)
    Amsg='Writing to file: '//trim(fname)
  CASE(IO_CLOSE)
    Amsg='Closing file: '//trim(fname)
  CASE DEFAULT
    Amsg='Error:Invalid error flag [1-4]'
  END SELECT

  call ErrorMSG( Msg=aMsg, UnitNo=iunit, File=File, Line=Line, &
    & Routine=Routine)

END SUBROUTINE FileError

!----------------------------------------------------------------------------
!                                                              AllocationErr
!----------------------------------------------------------------------------

SUBROUTINE AllocationErr( istat, aMsg, alloc, UnitNo, File, Routine, Line)
  !! Checks for successful (de)allocation.  Stops the code.
  !!
  !! Use this after an allocate/deallocate statement
  !! allocate(x(nz,ny,nx), stat=istat); call mErr(istat,'x',1)
  !! deallocate(x, stat=istat); call mErr(istat,'x',2)
  !====================================================================!
  INTEGER( I4B ), intent(in) :: istat
    !! results of stat=istat in (de)allocate
  character(len=*), intent(in) :: aMsg
    !! Message associated with the (de)allocate
  INTEGER( I4B ), intent(in) :: alloc
    !! 1 = allocate, 2 = deallocate
  INTEGER( I4B ), OPTIONAL, intent(in) :: UnitNo
    !! Optional file id to write the message to
  CHARACTER( LEN = * ), INTENT( IN ) :: File, Routine
    !! Filename and routine name
  INTEGER( I4B ), INTENT( IN ) :: Line

  ! Define internal variables
  CHARACTER( LEN = : ), ALLOCATABLE :: tmp
  INTEGER( I4B ) :: iunit

  IF ( istat == 0 ) RETURN

  tmp=''
  SELECT CASE( alloc )
  CASE( OPT_ALLOC )
    tmp='Allocating Memory: '//trim(aMsg)
  CASE( OPT_DEALLOC )
    tmp='Deallocating Memory: '//trim(aMsg)
  END SELECT

  call ErrorMSG( Msg=tmp, UnitNo=iunit, File=File, Line=Line, &
    & Routine=Routine)

END SUBROUTINE AllocationErr

END MODULE ErrorHandling
