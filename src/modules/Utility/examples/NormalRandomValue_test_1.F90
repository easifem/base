!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: NormalRandomValue samples the standard normal distribution

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE RandomUtility, ONLY: NormalRandomValue
USE Test_Method, ONLY: OK
IMPLICIT NONE

! n is odd, see the docs of NormalRandomValue
INTEGER(I4B), PARAMETER :: n = 10001
REAL(DFP) :: x(n), y(n), mu, sd

x = NormalRandomValue(n, seed=12345_I4B)

! sample mean and standard deviation
mu = SUM(x) / n
sd = SQRT(SUM((x - mu)**2) / (n - 1))
CALL OK(ABS(mu) .LT. 0.05_DFP, "mean is close to 0")
CALL OK(ABS(sd - 1.0_DFP) .LT. 0.05_DFP, "std is close to 1")

! the same seed gives the same values
y = NormalRandomValue(n, seed=12345_I4B)
CALL OK(ALL(x .EQ. y), "same seed gives the same values")

! a different seed gives different values
y = NormalRandomValue(n, seed=7_I4B)
CALL OK(ANY(x .NE. y), "different seed gives different values")

! n = 0 gives an empty vector
CALL OK(SIZE(NormalRandomValue(0_I4B, seed=1_I4B)) .EQ. 0_I4B, "n = 0")

END PROGRAM main
