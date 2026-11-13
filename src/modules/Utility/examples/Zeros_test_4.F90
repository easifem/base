!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Zeros for a real matrix

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE ZerosUtility, ONLY: Zeros
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), ALLOCATABLE :: a(:, :)
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP

a = Zeros(2_I4B, 3_I4B, 1.0_DFP)
CALL OK(ALL(SHAPE(a) .EQ. [2_I4B, 3_I4B]), "shape of the result")
CALL OK(ALL(ABS(a) .LT. tol), "all entries are zero")
CALL OK(KIND(Zeros(2_I4B, 3_I4B, 0.0_DFP)) .EQ. DFP, "kind of the result")

! zero size in one dimension
a = Zeros(0_I4B, 3_I4B, 1.0_DFP)
CALL OK(SIZE(a) .EQ. 0_I4B, "zero size")

END PROGRAM main
