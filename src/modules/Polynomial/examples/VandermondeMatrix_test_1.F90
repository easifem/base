!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Build a one dimensional monomial Vandermonde matrix.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE InterpolationUtility, ONLY: VandermondeMatrix
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: x(3), v(3, 3)

x = [-1.0_DFP, 0.0_DFP, 1.0_DFP]
v = VandermondeMatrix(order=2_I4B, x=x)
CALL OK(MAXVAL(ABS(v(:, 1)-1.0_DFP)) < 1.0E-12_DFP, &
  "Constant column")
CALL OK(MAXVAL(ABS(v(:, 3)-x*x)) < 1.0E-12_DFP, &
  "Quadratic column")
END PROGRAM main
