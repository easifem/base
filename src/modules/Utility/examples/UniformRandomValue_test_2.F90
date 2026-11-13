!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: UniformRandomValue in (a, b) with a seed

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE RandomUtility, ONLY: UniformRandomValue
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B), PARAMETER :: n = 1000
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP) :: a, b, x(n), y(n), u(n)
LOGICAL(LGT) :: isok

a = -2.0_DFP
b = 3.0_DFP

x = UniformRandomValue(n, a, b, seed=11_I4B)

! range
isok = ALL(x .GT. a) .AND. ALL(x .LT. b)
CALL OK(isok, "values are in (a, b)")

! the same seed gives the same values
y = UniformRandomValue(n, a, b, seed=11_I4B)
CALL OK(ALL(x .EQ. y), "same seed gives the same values")

! the result is a + (b - a) * u, where u is the unit version
u = UniformRandomValue(n, seed=11_I4B)
CALL OK(ALL(ABS(x - (a + (b - a) * u)) .LT. tol), "x = a + (b - a) u")

! a > b gives values in (b, a)
x = UniformRandomValue(n, b, a, seed=11_I4B)
isok = ALL(x .GT. a) .AND. ALL(x .LT. b)
CALL OK(isok, "a > b gives values in (b, a)")

END PROGRAM main
