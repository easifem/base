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
USE GlobalData, ONLY: I4B, OPT_ALLOC, OPT_DEALLOC, OPT_OPEN,  &
  & OPT_READ, OPT_WRITE, OPT_CLOSE
USE Display_Method, ONLY: Display, DashLine
IMPLICIT NONE
PRIVATE

PUBLIC :: Errormsg, Warningmsg, fileError, AllocationErr

CONTAINS

!----------------------------------------------------------------------------
!                                                                 Errormsg
!----------------------------------------------------------------------------

!> author: Dr. Vikas Sharma
!
! This subroutine prints the error message
!
! #Usage
! ```fortran
! call Errormsg( &
!   & msg="Some Error Message", &
!   & file= "test_ErrorHandling", &
!   & routine =  "test1", &
!   & line = 29 &
! )
! ```

SUBROUTINE Errormsg(msg, file, routine, line, unitno)
  CHARACTER(*), INTENT(IN) :: msg
  !! Message
  CHARACTER(*), INTENT(IN) :: file
  !! Name of the file
  CHARACTER(*), INTENT(IN) :: routine
  !! Name of the routine where error has occured
  INTEGER(I4B), INTENT(IN) :: line
  !! line number where error has occured
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  !! Unit number
  !!
  CALL Display(file, "ERROR :: In file :: ", unitno=unitno)
  CALL Display(LINE, "at line number :: ", unitno=unitno)
  CALL Display(" ", "in routine named :: "//TRIM(routine)// &
    & " with following message :: ", unitno=unitno)
  CALL Dashline(unitno=unitno)
  CALL Display(msg, unitno=unitno)
  CALL Dashline(unitno=unitno)
END SUBROUTINE Errormsg

!----------------------------------------------------------------------------
!                                                                Warningmsg
!----------------------------------------------------------------------------

!> author: Dr. Vikas Sharma
!
! This subroutine prints the warning message

SUBROUTINE Warningmsg(msg, file, routine, line, unitno)
  !! This subroutine prints the warning message
  CHARACTER(*), INTENT(IN) :: msg
  !! Message
  CHARACTER(*), INTENT(IN) :: file
  !! Name of the file
  CHARACTER(*), INTENT(IN) :: routine
  !! Name of the routine where error has occured
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  !! file id to write the message to
  INTEGER(I4B), INTENT(IN) :: line
  !! line number
  !!
  CALL Display(file, "WARNING :: In file ::", unitno=unitno)
  CALL Display(LINE, "line number ::", unitno=unitno)
  CALL Display(" ", "in routine named :: "//TRIM(routine)// &
    & " with following message :: ", unitno=unitno)
  CALL Dashline(unitno=unitno)
  CALL Display(msg, unitno=unitno)
  CALL Dashline(unitno=unitno)
END SUBROUTINE Warningmsg

!----------------------------------------------------------------------------
!                                                                 fileError
!----------------------------------------------------------------------------

!> author: Dr. Vikas Sharma
!
! This subroutine prints error while handling a file

SUBROUTINE fileError(istat, filename, flg, unitno, file, routine, line)
  ! Dummy argumnet
  INTEGER(I4B), INTENT(IN) :: istat
    !! Result of iostat=istat for open,read,write,close
  CHARACTER(*), INTENT(IN) :: filename
    !! Name of the file (IO related)
  INTEGER(I4B), INTENT(IN) :: flg
    !! IO_OPEN=Open, IO_READ=Read, IO_WRITE=Write, IO_CLOSE=Close
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
    !! file id to write the error to
  CHARACTER(*), INTENT(IN) :: file, routine
    !! Name of the source code file and routine name
  INTEGER(I4B), INTENT(IN) :: line
    !! line number
  !!
  ! Define internal variables
  CHARACTER(:), ALLOCATABLE :: Amsg
  !!
  ! Return if no error
  IF (istat == 0) THEN
    RETURN
  END IF
  !!
  Amsg = ""
  !!
  SELECT CASE (flg)
  CASE (OPT_OPEN)
    Amsg = 'Opening file: '//TRIM(filename)
  CASE (OPT_READ)
    Amsg = 'Reading from: '//TRIM(filename)
  CASE (OPT_WRITE)
    Amsg = 'Writing to file: '//TRIM(filename)
  CASE (OPT_CLOSE)
    Amsg = 'Closing file: '//TRIM(filename)
  CASE DEFAULT
    Amsg = 'Error:Invalid error flag [1-4]'
  END SELECT
  !!
  CALL Errormsg(msg=Amsg, unitno=unitno, file=file, line=line, &
    & routine=routine)
  !!
END SUBROUTINE fileError

!----------------------------------------------------------------------------
!                                                              AllocationErr
!----------------------------------------------------------------------------

!> author: Dr. Vikas Sharma
!
! This subroutine prints the error which occurs while allocating/
! deallocating an array
!
! Use this after an allocate/deallocate statement
! allocate(x(nz,ny,nx), stat=istat); call AllocationErr(istat,'x',1)
! deallocate(x, stat=istat); call AllocationErr(istat,'x',2)

SUBROUTINE AllocationErr(istat, amsg, alloc, unitno, file, routine, line)
  INTEGER(I4B), INTENT(IN) :: istat
  !! results of stat=istat in (de)allocate
  CHARACTER(*), INTENT(IN) :: amsg
  !! Message associated with the (de)allocate
  INTEGER(I4B), INTENT(IN) :: alloc
  !! For OPT_ALLOC = allocate, for OPT_DEALLOC = deallocate
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  !! Optional file id to write the message to
  CHARACTER(*), INTENT(IN) :: file, routine
  !! filename and routine name
  INTEGER(I4B), INTENT(IN) :: line
  !!
  ! Define internal variables
  CHARACTER(:), ALLOCATABLE :: tmp
  !!
  IF (istat == 0) RETURN
  !!
  tmp = ""
  SELECT CASE (alloc)
  CASE (OPT_ALLOC)
    tmp = 'Allocating Memory: '//TRIM(amsg)
  CASE (OPT_DEALLOC)
    tmp = 'Deallocating Memory: '//TRIM(amsg)
  END SELECT
  !!
  CALL Errormsg(msg=tmp, unitno=unitno, file=file, line=line, &
    & routine=routine)
  !!
END SUBROUTINE AllocationErr

END MODULE ErrorHandling
