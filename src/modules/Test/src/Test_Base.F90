! This program is a part of EASIFEM library
! Expandable And Scalable Infrastructure for Finite Element Methods
! htttps://www.easifem.com
! Vikas Sharma, Ph.D., vickysharma0812@gmail.com
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

MODULE Test_Base
USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: OUTPUT_UNIT, ERROR_UNIT
IMPLICIT NONE

! Kept as variables instead of aliases,
! so that test output or diagonostic output can be redirected
INTEGER :: test_unit = OUTPUT_UNIT, diago_unit = ERROR_UNIT

INTEGER :: tests = 0, todos = 0
CHARACTER(120) :: todomsg = ""

INTERFACE todo
  MODULE PROCEDURE todo_i, todo_s, todo_s_i, todo
END INTERFACE

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE diago(msg)
  CHARACTER(*), INTENT(in) :: msg
  WRITE (diago_unit, '("# ",A)') TRIM(msg) ! only trailing spaces
END SUBROUTINE diago

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE note(msg)
  CHARACTER(*), INTENT(in) :: msg
  WRITE (test_unit, '("# ",A)') TRIM(msg)
END SUBROUTINE note

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE testline(ok, msg, idmsg, gotmsg, expectedmsg)
  LOGICAL, INTENT(in) :: ok
  CHARACTER(*), INTENT(in) :: msg, idmsg, gotmsg, expectedmsg

  tests = tests + 1
  IF (.NOT. ok) CALL out("not ")
  WRITE (test_unit, '("ok ",I0)', advance="NO") tests

  IF (msg /= "" .OR. todos > 0) CALL out(" - ")

  IF (msg /= "") CALL out(TRIM(msg))

  IF (todos > 0) THEN
    todos = todos - 1
    IF (msg /= "") CALL out(" ")
    CALL out("# TODO")
    IF (todomsg .NE. "") THEN
      CALL out(": ")
      CALL out(TRIM(todomsg))
    END IF
  END IF
  IF (todos == 0) todomsg = ""

  WRITE (test_unit, *) ""

  IF (.NOT. ok) THEN
    ! 3 spaces prepended = 4 spaces indentation after # on diago
    IF (idmsg /= "") CALL diago("   "//idmsg)
    IF (gotmsg /= "") CALL diago("   "//gotmsg)
    IF (expectedmsg /= "") CALL diago("   "//expectedmsg)
  END IF
CONTAINS
  SUBROUTINE out(str)
    CHARACTER(*), INTENT(in) :: str
    WRITE (test_unit, '(A)', advance="NO") str
  END
END SUBROUTINE testline

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE ok(condition, msg)
  LOGICAL, INTENT(in) :: condition
  CHARACTER(*), INTENT(in), OPTIONAL :: msg
  IF (PRESENT(msg)) THEN
    CALL testline(condition, msg, "", "", "")
  ELSE
    CALL testline(condition, "", "", "", "")
  END IF
END SUBROUTINE ok

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE PASS(msg)
  CHARACTER(*), INTENT(in), OPTIONAL :: msg
  CALL ok(.TRUE., msg)
END SUBROUTINE PASS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE fail(msg)
  CHARACTER(*), INTENT(in), OPTIONAL :: msg
  CALL ok(.FALSE., msg)
END SUBROUTINE fail

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE todo_s_i(msg, howmany)
  CHARACTER(*), INTENT(in) :: msg
  INTEGER, INTENT(in) :: howmany
  todomsg = msg
  todos = howmany
END SUBROUTINE todo_s_i

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE todo
  CALL todo_s_i("", 1)
END SUBROUTINE todo

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE todo_s(msg)
  CHARACTER(*), INTENT(in) :: msg
  CALL todo_s_i(msg, 1)
END SUBROUTINE todo_s

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE todo_i(howmany)
  INTEGER, INTENT(in) :: howmany
  CALL todo_s_i("", howmany)
END SUBROUTINE todo_i

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE Test_Base
