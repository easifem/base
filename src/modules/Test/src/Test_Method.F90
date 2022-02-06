! Copyright 2015 Dennis Decker Jensen
! See <http://testanything.org> and <https://metacpan.org/pod/Test::More>
! Tectonics: gfortran -g -Wall -Wextra -std=f2008ts -c test.f08
!
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

MODULE test_base
USE, INTRINSIC :: iso_fortran_env, ONLY: output_unit, error_unit
IMPLICIT NONE

! Kept as variables instead of aliases,
! so that test output or diagonostic output can be redirected
INTEGER :: test_unit = output_unit, diago_unit = error_unit

INTEGER :: tests = 0, todos = 0
CHARACTER(len=120) :: todomsg = ""

INTERFACE todo
  MODULE PROCEDURE todo_i, todo_s, todo_s_i, todo
END INTERFACE

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE diago(msg)
  CHARACTER(len=*), INTENT(in) :: msg
  WRITE (diago_unit, '("# ",A)') TRIM(msg) ! only trailing spaces
END SUBROUTINE diago

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE note(msg)
  CHARACTER(len=*), INTENT(in) :: msg
  WRITE (test_unit, '("# ",A)') TRIM(msg)
END SUBROUTINE note

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE testline(ok, msg, idmsg, gotmsg, expectedmsg)
  LOGICAL, INTENT(in) :: ok
  CHARACTER(len=*), INTENT(in) :: msg, idmsg, gotmsg, expectedmsg

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
    CHARACTER(len=*), INTENT(in) :: str
    WRITE (test_unit, '(A)', advance="NO") str
  END
END SUBROUTINE testline

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE ok(condition, msg)
  LOGICAL, INTENT(in) :: condition
  CHARACTER(len=*), INTENT(in), OPTIONAL :: msg
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
  CHARACTER(len=*), INTENT(in), OPTIONAL :: msg
  CALL ok(.TRUE., msg)
END SUBROUTINE PASS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE fail(msg)
  CHARACTER(len=*), INTENT(in), OPTIONAL :: msg
  CALL ok(.FALSE., msg)
END SUBROUTINE fail

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE todo_s_i(msg, howmany)
  CHARACTER(len=*), INTENT(in) :: msg
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
  CHARACTER(len=*), INTENT(in) :: msg
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

END MODULE test_base


!----------------------------------------------------------------------------
!                                                             test_planning
!----------------------------------------------------------------------------

MODULE test_planning
USE test_base, ONLY: test_unit, tests
IMPLICIT NONE

INTEGER, PRIVATE :: planned = 0

CONTAINS

SUBROUTINE bail_out(msg)
  CHARACTER(len=*), INTENT(in), OPTIONAL :: msg
  IF (PRESENT(msg)) THEN
    WRITE (test_unit, '("Bail out! ",A)') msg
  ELSE
    WRITE (test_unit, '("Bail out!")')
  END IF
  STOP
END SUBROUTINE bail_out


SUBROUTINE plan(tests)
  INTEGER, INTENT(in) :: tests

  SELECT CASE (tests)
  CASE (:-1)
    CALL bail_out("A plan with a negative number of tests")
  CASE (0)
    WRITE (test_unit, '("1..0")')
    STOP ! The same as skip_all without a given reason
  CASE (1:)
    IF (planned > 0) &
       & CALL bail_out("More than one plan in test output")
    planned = tests
    WRITE (test_unit, '("1..",I0)') planned
  END SELECT
END SUBROUTINE plan


SUBROUTINE done_testing(howmany)
  INTEGER, INTENT(in), OPTIONAL :: howmany

  ! Put plan at the end of test output
  IF (PRESENT(howmany)) THEN
    CALL plan(howmany)
  ELSE
    IF (planned == 0) CALL plan(tests)
    ! else - We already have a plan
  END IF
END SUBROUTINE done_testing


SUBROUTINE skip_all(msg)
  CHARACTER(len=*), INTENT(in), OPTIONAL :: msg
  IF (PRESENT(msg)) THEN
    WRITE (test_unit, '("1..0 # Skipped: ",A)') msg
  ELSE
    WRITE (test_unit, '("1..0 # Skipped all")')
  END IF
  STOP
END SUBROUTINE skip_all

END MODULE test_planning

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
!
!! Template instances of integer kinds for "is"

MODULE is_i8_mod
USE, INTRINSIC :: iso_fortran_env, ONLY: wp => int8
USE, non_INTRINSIC :: test_base, ONLY: testline, tests
CONTAINS
INCLUDE "is_i.inc"
END MODULE is_i8_mod


MODULE is_i16_mod
USE, INTRINSIC :: iso_fortran_env, ONLY: wp => int16
USE, non_INTRINSIC :: test_base, ONLY: testline, tests
CONTAINS
INCLUDE "is_i.inc"
END MODULE is_i16_mod


MODULE is_i32_mod
USE, INTRINSIC :: iso_fortran_env, ONLY: wp => int32
USE, non_INTRINSIC :: test_base, ONLY: testline, tests
CONTAINS
INCLUDE "is_i.inc"
END MODULE is_i32_mod


MODULE is_i64_mod
USE, INTRINSIC :: iso_fortran_env, ONLY: wp => int64
USE, non_INTRINSIC :: test_base, ONLY: testline, tests
CONTAINS
INCLUDE "is_i.inc"
END MODULE is_i64_mod


MODULE is_i
USE is_i8_mod, ONLY: is_i8 => is
USE is_i16_mod, ONLY: is_i16 => is
USE is_i32_mod, ONLY: is_i32 => is
USE is_i64_mod, ONLY: is_i64 => is
INTERFACE is
  MODULE PROCEDURE is_i8, is_i16, is_i32, is_i64
END INTERFACE
END MODULE is_i


! Template instances of real kinds for "is"

MODULE is_r32_mod
USE, INTRINSIC :: iso_fortran_env, ONLY: wp => real32
USE, non_INTRINSIC :: test_base, ONLY: testline, tests
CONTAINS
INCLUDE "is_r.inc"
END MODULE is_r32_mod


MODULE is_r64_mod
USE, INTRINSIC :: iso_fortran_env, ONLY: wp => real64
USE, non_INTRINSIC :: test_base, ONLY: testline, tests
CONTAINS
INCLUDE "is_r.inc"
END MODULE is_r64_mod


MODULE is_r128_mod
USE, INTRINSIC :: iso_fortran_env, ONLY: wp => real128
USE, non_INTRINSIC :: test_base, ONLY: testline, tests
CONTAINS
INCLUDE "is_r.inc"
END MODULE is_r128_mod


MODULE is_r
USE is_r32_mod, ONLY: isrel_r32 => isrel, isabs_r32 => isabs, &
                        & isnear_r32 => isnear
USE is_r64_mod, ONLY: isrel_r64 => isrel, isabs_r64 => isabs, &
                        & isnear_r64 => isnear
USE is_r128_mod, ONLY: isrel_r128 => isrel, isabs_r128 => isabs, &
                        & isnear_r128 => isnear
INTERFACE isrel
  MODULE PROCEDURE isrel_r32, isrel_r64, isrel_r128
END INTERFACE

INTERFACE isabs
  MODULE PROCEDURE isabs_r32, isabs_r64, isabs_r128
END INTERFACE

INTERFACE isnear
  MODULE PROCEDURE isnear_r32, isnear_r64, isnear_r128
END INTERFACE
END MODULE is_r


MODULE test_more
USE test_base, ONLY: testline, tests, test_unit
USE test_planning, ONLY: bail_out ! for negative skips
USE is_i, ONLY: is, is_i8, is_i16, is_i32, is_i64
USE is_r, ONLY: isabs, isrel, isnear, &
                  & isabs_r32, isrel_r32, isnear_r32, &
                  & isabs_r64, isrel_r64, isnear_r64, &
                  & isabs_r128, isrel_r128, isnear_r128

! Complex numbers cannot be compared, hence no is_c module

IMPLICIT NONE

INTERFACE skip
  MODULE PROCEDURE skip_i, skip_s, skip_s_i, skip
END INTERFACE

INTERFACE is
  MODULE PROCEDURE is_s, is_l
END INTERFACE

CONTAINS

SUBROUTINE skip_s_i(msg, howmany)
  CHARACTER(len=*), INTENT(in) :: msg
  INTEGER, INTENT(in) :: howmany
  CHARACTER(len=120) skipmsg
  INTEGER i

  IF (howmany <= 0) THEN
    CALL bail_out("Skipped non-positive number of tests")
  END IF

  IF (msg == "") THEN
    skipmsg = "# SKIP"
  ELSE
    skipmsg = "# SKIP: "//TRIM(msg)
  END IF

  DO i = 1, howmany
    tests = tests + 1
    WRITE (test_unit, '("ok ",I0," ",A)') tests, TRIM(skipmsg)
  END DO
END SUBROUTINE skip_s_i


SUBROUTINE skip
  CALL skip_s_i("", 1)
END SUBROUTINE skip


SUBROUTINE skip_s(msg)
  CHARACTER(len=*), INTENT(in) :: msg
  CALL skip_s_i(msg, 1)
END SUBROUTINE skip_s


SUBROUTINE skip_i(howmany)
  INTEGER, INTENT(in) :: howmany
  CALL skip_s_i("", howmany)
END SUBROUTINE skip_i


! Duplicates of is_i routines in file is_i.inc and ditto is_r
! They are not factored any further, because it is easier
! to see all the output together rather than in separate routines

SUBROUTINE is_s(got, expected, msg)
  CHARACTER(len=*), INTENT(in) :: got
  CHARACTER(len=*), INTENT(in) :: expected
  CHARACTER(len=*), INTENT(in), OPTIONAL :: msg
  CHARACTER(len=:), ALLOCATABLE :: testmsg, idmsg
  CHARACTER(len=120) gotmsg, expectedmsg
  LOGICAL good

  IF (PRESENT(msg)) THEN
    ALLOCATE (CHARACTER(len=len_TRIM(msg) + 20) :: testmsg, idmsg)
    WRITE (unit=idmsg, fmt='(A,A,A)') 'Failed test: "', TRIM(msg), '"'
    testmsg = TRIM(msg)
  ELSE
    ALLOCATE (CHARACTER(len=30) :: testmsg, idmsg)
    WRITE (unit=idmsg, fmt='(A,I0)') 'Failed test no. ', tests + 1
    testmsg = ""
  END IF
  WRITE (unit=gotmsg, fmt='(A,A,A)') '     got: "', got, '"'
  WRITE (unit=expectedmsg, fmt='(A,A,A)') 'expected: "', expected, '"'

  good = got == expected
  CALL testline(good, testmsg, idmsg, gotmsg, expectedmsg)
END SUBROUTINE is_s


SUBROUTINE is_l(got, expected, msg)
  LOGICAL, INTENT(in) :: got, expected
  CHARACTER(len=*), INTENT(in), OPTIONAL :: msg
  CHARACTER(len=:), ALLOCATABLE :: testmsg, idmsg
  CHARACTER(len=120) gotmsg, expectedmsg
  LOGICAL good

  IF (PRESENT(msg)) THEN
    ALLOCATE (CHARACTER(len=len_TRIM(msg) + 20) :: testmsg, idmsg)
    WRITE (unit=idmsg, fmt='(A,A,A)') 'Failed test: "', TRIM(msg), '"'
    testmsg = TRIM(msg)
  ELSE
    ALLOCATE (CHARACTER(len=30) :: testmsg, idmsg)
    WRITE (unit=idmsg, fmt='(A,I0)') 'Failed test no. ', tests + 1
    testmsg = ""
  END IF
  WRITE (unit=gotmsg, fmt='(A,L1)') '     got: ', got
  WRITE (unit=expectedmsg, fmt='(A,L1)') 'expected: ', expected

  good = got .EQV. expected
  CALL testline(good, testmsg, idmsg, gotmsg, expectedmsg)
END SUBROUTINE is_l

END MODULE test_more

MODULE test_method
USE test_base, ONLY: test_unit, diago_unit, &
                  & ok, diago, note, PASS, fail, todo
USE test_planning, ONLY: plan, done_testing, skip_all, bail_out
USE test_more, ONLY: is, isabs, isrel, isnear, skip
END MODULE test_method
