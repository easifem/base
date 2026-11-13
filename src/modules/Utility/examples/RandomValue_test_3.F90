!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: RandomValue for an integer between from and to

PROGRAM main
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE RandomUtility, ONLY: RandomValue
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B), PARAMETER :: n = 1000
INTEGER(I4B) :: i, k(n)
LOGICAL(LGT) :: isok

! values are between from and to, both included
DO i = 1, n
  k(i) = RandomValue(from=1_I4B, to=3_I4B)
END DO
isok = ALL(k .GE. 1_I4B) .AND. ALL(k .LE. 3_I4B)
CALL OK(isok, "value is in [1, 3]")
isok = ANY(k .EQ. 1_I4B) .AND. ANY(k .EQ. 3_I4B)
CALL OK(isok, "both end points are reached")

! the order of from and to does not matter
DO i = 1, n
  k(i) = RandomValue(from=3_I4B, to=1_I4B)
END DO
isok = ALL(k .GE. 1_I4B) .AND. ALL(k .LE. 3_I4B)
CALL OK(isok, "from > to gives a value in [1, 3]")

! negative bounds
DO i = 1, n
  k(i) = RandomValue(from=-5_I4B, to=-2_I4B)
END DO
isok = ALL(k .GE. -5_I4B) .AND. ALL(k .LE. -2_I4B)
CALL OK(isok, "value is in [-5, -2]")

! from equal to to
CALL OK(RandomValue(4_I4B, 4_I4B) .EQ. 4_I4B, "from = to")

END PROGRAM main
