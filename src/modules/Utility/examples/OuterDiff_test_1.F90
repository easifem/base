!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: OuterDiff returns ans(i,j) = a(i) - b(j)

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE MiscUtility, ONLY: OuterDiff
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
INTEGER(I4B) :: ia(3), ib(2), ians(3, 2)
REAL(DFP) :: ra(2), rb(3), rans(2, 3)

! integer vectors
ia = [1, 2, 3]
ib = [10, 20]
ians = OuterDiff(ia, ib)
CALL OK(ALL(SHAPE(ians) .EQ. [3, 2]), "shape is (SIZE(a), SIZE(b))")
CALL OK(ALL(ians(:, 1) .EQ. [-9, -8, -7]), "integer column 1")
CALL OK(ALL(ians(:, 2) .EQ. [-19, -18, -17]), "integer column 2")

! real vectors
ra = [1.5_DFP, -0.5_DFP]
rb = [0.5_DFP, 1.0_DFP, 2.0_DFP]
rans = OuterDiff(ra, rb)
CALL OK(ABS(rans(1, 1) - 1.0_DFP) .LT. tol, "real (1,1)")
CALL OK(ABS(rans(2, 3) + 2.5_DFP) .LT. tol, "real (2,3)")
CALL OK(ALL(ABS(rans(1, :) - (ra(1) - rb)) .LT. tol), "real row 1")

END PROGRAM main
