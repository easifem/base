!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: UniformRandomValue in (0, 1) with a seed

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE RandomUtility, ONLY: UniformRandomValue
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B), PARAMETER :: n = 1000
REAL(DFP), PARAMETER :: tol = 1.0E-14_DFP
REAL(DFP) :: u(n), v(n), w(3)
LOGICAL(LGT) :: isok

u = UniformRandomValue(n, seed=7_I4B)

! size and range
CALL OK(SIZE(UniformRandomValue(n, seed=7_I4B)) .EQ. n, "size of result")
isok = ALL(u .GT. 0.0_DFP) .AND. ALL(u .LT. 1.0_DFP)
CALL OK(isok, "values are in (0, 1)")

! the same seed gives the same values
v = UniformRandomValue(n, seed=7_I4B)
CALL OK(ALL(u .EQ. v), "same seed gives the same values")

! a different seed gives different values
v = UniformRandomValue(n, seed=8_I4B)
CALL OK(ANY(u .NE. v), "different seed gives different values")

! the default seed is 1
w = UniformRandomValue(3_I4B)
CALL OK(ALL(w .EQ. UniformRandomValue(3_I4B, seed=1_I4B)), &
        "default seed is 1")

! first value for seed = 1 is 16807 / (2**31 - 1)
CALL OK(ABS(w(1) - 7.826369259012501E-6_DFP) .LT. tol, &
        "first value for seed 1")

! seed = 0 gives zeros
w = UniformRandomValue(3_I4B, seed=0_I4B)
CALL OK(ALL(w .EQ. 0.0_DFP), "seed 0 gives zeros")

END PROGRAM main
