!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Ones for a real rank-4 array

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE OnesUtility, ONLY: Ones
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), ALLOCATABLE :: a(:, :, :, :)
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP

a = Ones(2_I4B, 3_I4B, 4_I4B, 5_I4B, 1.0_DFP)
CALL OK(ALL(SHAPE(a) .EQ. [2_I4B, 3_I4B, 4_I4B, 5_I4B]), &
        "shape of the result")
CALL OK(ALL(ABS(a - 1.0_DFP) .LT. tol), "all entries are one")
CALL OK(ABS(SUM(a) - 120.0_DFP) .LT. tol, "sum of entries is the size")
CALL OK(KIND(Ones(1_I4B, 1_I4B, 1_I4B, 1_I4B, 0.0_DFP)) .EQ. DFP, &
        "kind of the result")

END PROGRAM main
