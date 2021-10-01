! This program is a part of EASIFEM library
! Copyright (C) 2020-2021  Vikas Sharma, Ph.D
!
! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <https: //www.gnu.org/licenses/>
!


!> [[ErrorHandling]] module contains error handling routines.

MODULE ErrorHandling
USE GlobalData
USE Display_Method
IMPLICIT NONE
PRIVATE

PUBLIC :: ErrorMSG, WarningMSG, FileError, AllocationErr

CONTAINS
!----------------------------------------------------------------------------
!                                                                 ErrorMsg
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! This subroutine prints the error message
!
! #Usage
! ```fortran
! call ErrorMSG( &
!   & Msg="Some Error Message", &
!   & File= "test_ErrorHandling", &
!   & Routine =  "test1", &
!   & Line = 29 &
! )
! ```

SUBROUTINE ErrorMSG( Msg, File, Routine, Line, UnitNo )
  CHARACTER( LEN = * ), INTENT( IN ) :: Msg
  !! Message
  CHARACTER( LEN = * ), INTENT( IN ) :: File
  !! Name of the file
  CHARACTER( LEN = * ), INTENT( IN ) :: Routine
  !! Name of the routine where error has occured
  INTEGER( I4B ), INTENT( IN ) :: Line
  !! Line number where error has occured
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: UnitNo
  !! Unit number

  ! Internal variable
  INTEGER( I4B ) :: Unit_No

  IF( PRESENT( UnitNo ) ) THEN
    Unit_No = UnitNo
  ELSE
    Unit_No = stdout
  END IF

  CALL Display( FILE, "ERROR :: In File :: ", UnitNo = Unit_No )
  CALL Display( LINE, "at line number :: ", UnitNo = Unit_No )
  CALL Display( " ", "in Routine named :: "// TRIM(Routine) // &
    & " with following message :: ", UnitNo = Unit_No )
  CALL DashLine(UnitNo = Unit_No)
  CALL Display(  Msg, UnitNo = Unit_No )
  CALL DashLine(UnitNo = Unit_No)
END SUBROUTINE ErrorMSG

!----------------------------------------------------------------------------
!                                                                WarningMSG
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! This subroutine prints the warning message

SUBROUTINE WarningMSG( Msg, File, Routine, Line, UnitNo )
  !! This subroutine prints the warning message
  CHARACTER( LEN = * ), INTENT( IN ) :: Msg
  !! Message
  CHARACTER( LEN = * ), INTENT( IN ) :: File
  !! Name of the file
  CHARACTER( LEN = * ), INTENT( IN ) :: Routine
  !! Name of the routine where error has occured
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: UnitNo
  !! file id to write the message to
  INTEGER( I4B ), INTENT( IN ) :: Line
  !! Line number

  ! Define internal variables
  INTEGER( I4B ) :: Unit_No

  IF( PRESENT( UnitNo ) ) THEN
    Unit_No = UnitNo
  ELSE
    Unit_No = stdout
  END IF

  CALL Display( FILE, "WARNING :: In File ::", UnitNo = Unit_No )
  CALL Display( LINE, "Line number ::", UnitNo = Unit_No )
  CALL Display( " ", "in Routine named :: "// TRIM(Routine) // &
    & " with following message :: ", UnitNo = Unit_No )
  CALL DashLine(UnitNo = Unit_No)
  CALL Display(  Msg, UnitNo = Unit_No )
  CALL DashLine(UnitNo = Unit_No)
END SUBROUTINE WarningMSG

!----------------------------------------------------------------------------
!                                                                 FileError
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! This subroutine prints error while handling a file

SUBROUTINE FileError(istat, filename, flg, UnitNo, File, Routine, Line )
  ! Dummy argumnet
  INTEGER( I4B ), INTENT( IN ) :: istat
    !! Result of iostat=istat for open,read,write,close
  CHARACTER(len=*), INTENT( IN ) :: filename
    !! Name of the file (IO related)
  INTEGER( I4B ), INTENT( IN ) :: flg
    !! IO_OPEN=Open, IO_READ=Read, IO_WRITE=Write, IO_CLOSE=Close
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: UnitNo
    !! file id to write the error to
  CHARACTER( LEN = * ), INTENT( IN ) :: File, Routine
    !! Name of the source code file and routine name
  INTEGER( I4B ), INTENT( IN ) :: Line
    !! Line number

  ! Define internal variables
  CHARACTER(len=:),allocatable :: Amsg
  INTEGER( I4B ) :: iunit

  ! Return if no error
  IF ( istat == 0 ) THEN
    RETURN
  END IF


  IF( PRESENT( UnitNo ) ) THEN
    iunit = UnitNo
  ELSE
    iunit = stdout
  END IF

  Amsg = ""

  SELECT CASE(flg)
  CASE(OPT_OPEN)
    Amsg='Opening file: '//  TRIM(filename)
  CASE(OPT_READ)
    Amsg='Reading from: '// TRIM(filename)
  CASE(OPT_WRITE)
    Amsg='Writing to file: '// TRIM(filename)
  CASE(OPT_CLOSE)
    Amsg='Closing file: '// TRIM(filename)
  CASE DEFAULT
    Amsg='Error:Invalid error flag [1-4]'
  END SELECT

  CALL ErrorMSG( Msg=AMsg, UnitNo=iunit, File=File, Line=Line, &
    & Routine=Routine )

END SUBROUTINE FileError

!----------------------------------------------------------------------------
!                                                              AllocationErr
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! This subroutine prints the error which occurs while allocating/
! deallocating an array
!
! Use this after an allocate/deallocate statement
! allocate(x(nz,ny,nx), stat=istat); call AllocationErr(istat,'x',1)
! deallocate(x, stat=istat); call AllocationErr(istat,'x',2)

SUBROUTINE AllocationErr( istat, aMsg, alloc, UnitNo, File, Routine, Line)
  INTEGER( I4B ), INTENT( IN ) :: istat
  !! results of stat=istat in (de)allocate
  CHARACTER(LEN=*), INTENT( IN ) :: aMsg
  !! Message associated with the (de)allocate
  INTEGER( I4B ), INTENT( IN ) :: alloc
  !! For OPT_ALLOC = allocate, for OPT_DEALLOC = deallocate
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: UnitNo
  !! Optional file id to write the message to
  CHARACTER( LEN = * ), INTENT( IN ) :: File, Routine
  !! Filename and routine name
  INTEGER( I4B ), INTENT( IN ) :: Line

  ! Define internal variables
  CHARACTER( LEN = : ), ALLOCATABLE :: tmp
  INTEGER( I4B ) :: iunit

  IF ( istat == 0 ) RETURN

  tmp = ""
  SELECT CASE( alloc )
  CASE( OPT_ALLOC )
    tmp='Allocating Memory: '// TRIM( aMsg )
  CASE( OPT_DEALLOC )
    tmp='Deallocating Memory: '// TRIM( aMsg )
  END SELECT

  call ErrorMSG( Msg=tmp, UnitNo=iunit, File=File, Line=Line, &
    & Routine=Routine)

END SUBROUTINE AllocationErr

END MODULE ErrorHandling
