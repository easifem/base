!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: UniformRandomValueScalar with a seed

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE RandomUtility, ONLY: UniformRandomValue
USE RandomUtility, ONLY: UniformRandomValueScalar
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-14_DFP
REAL(DFP) :: u, v(1)

! default seed is 1, value is 16807 / (2**31 - 1)
u = UniformRandomValueScalar()
CALL OK(ABS(u - 7.826369259012501E-6_DFP) .LT. tol, "default seed")

! a given seed
u = UniformRandomValueScalar(seed=12345_I4B)
CALL OK(ABS(u - 0.09661652850250932_DFP) .LT. tol, "seed 12345")
CALL OK(u .GT. 0.0_DFP .AND. u .LT. 1.0_DFP, "value is in (0, 1)")

! same as the first entry of UniformRandomValue
v = UniformRandomValue(1_I4B, seed=12345_I4B)
CALL OK(u .EQ. v(1), "same as UniformRandomValue(1, seed)")

! the seed is not updated, so the same seed gives the same value
CALL OK(UniformRandomValueScalar(5_I4B) .EQ. &
        UniformRandomValueScalar(5_I4B), "seed is not updated")

! seed = 0 gives zero
CALL OK(UniformRandomValueScalar(0_I4B) .EQ. 0.0_DFP, "seed 0 gives 0")

END PROGRAM main
