!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: RandomValue with an optional distribution

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

! default: uniform value in [0, 1)
DO i = 1, n
  x(i) = RandomValue()
END DO
isok = ALL(x .GE. 0.0_DFP) .AND. ALL(x .LT. 1.0_DFP)
CALL OK(isok, "default is in [0, 1)")

! an unknown distribution falls back to the default
DO i = 1, n
  x(i) = RandomValue(distribution="Uniform")
END DO
isok = ALL(x .GE. 0.0_DFP) .AND. ALL(x .LT. 1.0_DFP)
CALL OK(isok, "unknown distribution is in [0, 1)")

! Binomial: sum of 20 uniform values minus 10
DO i = 1, n
  x(i) = RandomValue(distribution="Binomial")
END DO
isok = ALL(x .GE. -10.0_DFP) .AND. ALL(x .LT. 10.0_DFP)
CALL OK(isok, "Binomial is in [-10, 10)")
CALL OK(ABS(SUM(x) / n) .LT. 0.3_DFP, "Binomial mean is close to 0")

! the lower case name is also accepted
DO i = 1, n
  x(i) = RandomValue(distribution="binomial")
END DO
CALL OK(ANY(x .LT. 0.0_DFP), "binomial gives negative values")

END PROGRAM main
