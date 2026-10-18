!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: RandomValue for a real number between from and to

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE RandomUtility, ONLY: RandomValue
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B), PARAMETER :: n = 1000
INTEGER(I4B) :: i
REAL(DFP) :: x(n)
LOGICAL(LGT) :: isok

! values are between from and to
DO i = 1, n
  x(i) = RandomValue(from=2.0_DFP, to=5.0_DFP)
END DO
isok = ALL(x .GE. 2.0_DFP) .AND. ALL(x .LE. 5.0_DFP)
CALL OK(isok, "value is in [2, 5]")

! the order of from and to does not matter
DO i = 1, n
  x(i) = RandomValue(from=5.0_DFP, to=2.0_DFP)
END DO
isok = ALL(x .GE. 2.0_DFP) .AND. ALL(x .LE. 5.0_DFP)
CALL OK(isok, "from > to gives a value in [2, 5]")

! negative bounds
DO i = 1, n
  x(i) = RandomValue(from=-1.0_DFP, to=-0.5_DFP)
END DO
isok = ALL(x .GE. -1.0_DFP) .AND. ALL(x .LE. -0.5_DFP)
CALL OK(isok, "value is in [-1, -0.5]")

! from equal to to
CALL OK(RandomValue(3.0_DFP, 3.0_DFP) .EQ. 3.0_DFP, "from = to")

END PROGRAM main
