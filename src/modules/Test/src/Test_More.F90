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

MODULE Test_More
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

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE skip_s_i(msg, howmany)
  CHARACTER(*), INTENT(in) :: msg
  INTEGER, INTENT(in) :: howmany
  CHARACTER(120) skipmsg
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

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE skip
  CALL skip_s_i("", 1)
END SUBROUTINE skip

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE skip_s(msg)
  CHARACTER(*), INTENT(in) :: msg
  CALL skip_s_i(msg, 1)
END SUBROUTINE skip_s

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE skip_i(howmany)
  INTEGER, INTENT(in) :: howmany
  CALL skip_s_i("", howmany)
END SUBROUTINE skip_i

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Duplicates of is_i routines in file is_i.inc and ditto is_r
! They are not factored any further, because it is easier
! to see all the output together rather than in separate routines

SUBROUTINE is_s(got, expected, msg)
  CHARACTER(*), INTENT(in) :: got
  CHARACTER(*), INTENT(in) :: expected
  CHARACTER(*), INTENT(in), OPTIONAL :: msg
  CHARACTER(:), ALLOCATABLE :: testmsg, idmsg
  CHARACTER(120) gotmsg, expectedmsg
  LOGICAL good

  IF (PRESENT(msg)) THEN
    ALLOCATE (CHARACTER(LEN_TRIM(msg) + 20) :: testmsg, idmsg)
    WRITE (unit=idmsg, fmt='(A,A,A)') 'Failed test: "', TRIM(msg), '"'
    testmsg = TRIM(msg)
  ELSE
    ALLOCATE (CHARACTER(30) :: testmsg, idmsg)
    WRITE (unit=idmsg, fmt='(A,I0)') 'Failed test no. ', tests + 1
    testmsg = ""
  END IF
  WRITE (unit=gotmsg, fmt='(A,A,A)') '     got: "', got, '"'
  WRITE (unit=expectedmsg, fmt='(A,A,A)') 'expected: "', expected, '"'

  good = got == expected
  CALL testline(good, testmsg, idmsg, gotmsg, expectedmsg)
END SUBROUTINE is_s

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE is_l(got, expected, msg)
  LOGICAL, INTENT(in) :: got, expected
  CHARACTER(*), INTENT(in), OPTIONAL :: msg
  CHARACTER(:), ALLOCATABLE :: testmsg, idmsg
  CHARACTER(120) gotmsg, expectedmsg
  LOGICAL good

  IF (PRESENT(msg)) THEN
    ALLOCATE (CHARACTER(LEN_TRIM(msg) + 20) :: testmsg, idmsg)
    WRITE (unit=idmsg, fmt='(A,A,A)') 'Failed test: "', TRIM(msg), '"'
    testmsg = TRIM(msg)
  ELSE
    ALLOCATE (CHARACTER(30) :: testmsg, idmsg)
    WRITE (unit=idmsg, fmt='(A,I0)') 'Failed test no. ', tests + 1
    testmsg = ""
  END IF
  WRITE (unit=gotmsg, fmt='(A,L1)') '     got: ', got
  WRITE (unit=expectedmsg, fmt='(A,L1)') 'expected: ', expected

  good = got .EQV. expected
  CALL testline(good, testmsg, idmsg, gotmsg, expectedmsg)
END SUBROUTINE is_l

END MODULE Test_More
