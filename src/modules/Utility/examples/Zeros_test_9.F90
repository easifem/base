!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Zeros for a matrix whose shape is given by a vector

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE ZerosUtility, ONLY: Zeros
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B), ALLOCATABLE :: ia(:, :)
REAL(DFP), ALLOCATABLE :: ra(:, :)
REAL(DFP) :: b(4, 2)
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP

! integer matrix of shape s = [2, 3]
ia = Zeros([2_I4B, 3_I4B], 1_I4B)
CALL OK(ALL(SHAPE(ia) .EQ. [2_I4B, 3_I4B]), "shape (integer)")
CALL OK(ALL(ia .EQ. 0_I4B), "all entries are zero (integer)")

! real matrix with the same shape as another matrix
b = 1.0_DFP
ra = Zeros(SHAPE(b), 1.0_DFP)
CALL OK(ALL(SHAPE(ra) .EQ. SHAPE(b)), "shape is SHAPE(b)")
CALL OK(ALL(ABS(ra) .LT. tol), "all entries are zero (real)")
CALL OK(KIND(ra) .EQ. DFP, "kind of the result")

! zero size in one dimension
ra = Zeros([0_I4B, 3_I4B], 1.0_DFP)
CALL OK(SIZE(ra) .EQ. 0_I4B, "zero size")

END PROGRAM main
