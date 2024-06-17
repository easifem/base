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

MODULE Test_Planning
USE test_base, ONLY: test_unit, tests
IMPLICIT NONE

INTEGER, PRIVATE :: planned = 0

CONTAINS

SUBROUTINE bail_out(msg)
  CHARACTER(*), INTENT(in), OPTIONAL :: msg
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
  CHARACTER(*), INTENT(in), OPTIONAL :: msg
  IF (PRESENT(msg)) THEN
    WRITE (test_unit, '("1..0 # Skipped: ",A)') msg
  ELSE
    WRITE (test_unit, '("1..0 # Skipped all")')
  END IF
  STOP
END SUBROUTINE skip_all

END MODULE Test_Planning
