!> author: Vikas Sharma, Ph. D.
! date: 2026-10-19
! summary: UniformUnitRandomValue returns a seeded random unit vector

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE RandomUtility, ONLY: UniformUnitRandomValue
USE RandomUtility, ONLY: NormalRandomValue
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP) :: w(3), z(3), w4(4)

! the vector has length one
w = UniformUnitRandomValue(3_I4B, seed=7_I4B)
CALL OK(ABS(NORM2(w) - 1.0_DFP) .LT. tol, "norm is 1")

! it is the normalized output of NormalRandomValue
z = NormalRandomValue(3_I4B, seed=7_I4B)
CALL OK(ALL(ABS(w - z / NORM2(z)) .LT. tol), "w = z / |z|")

! the same seed gives the same vector, also for an even size
w4 = UniformUnitRandomValue(4_I4B, seed=3_I4B)
CALL OK(ALL(w4 .EQ. UniformUnitRandomValue(4_I4B, seed=3_I4B)), &
        "same seed, same vector")
CALL OK(ABS(NORM2(w4) - 1.0_DFP) .LT. tol, "norm is 1 for m = 4")

END PROGRAM main
