!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Compute monomial coefficients of the nodal basis.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Line
USE LagrangePolynomialUtility, ONLY: LagrangeCoeff_
USE LagrangePolynomialUtility, ONLY: LagrangeVandermonde
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: nodes(1, 3)
REAL(DFP) :: y(3, 3)
INTEGER(I4B) :: nr, nc

nodes(1, :) = [-1.0_DFP, 0.0_DFP, 1.0_DFP]
CALL LagrangeCoeff_(order=2_I4B, elemType=Line, xij=nodes, &
  ans=y, nrow=nr, ncol=nc)
CALL OK(MAXVAL(ABS(MATMUL( &
  LagrangeVandermonde(xij=nodes, order=2_I4B, elemType=Line), &
  y)-RESHAPE([1.0_DFP, 0.0_DFP, 0.0_DFP, &
  0.0_DFP, 1.0_DFP, 0.0_DFP, &
  0.0_DFP, 0.0_DFP, 1.0_DFP], [3, 3]))) < 1.0E-10_DFP, &
  "Interpolation construction")
END PROGRAM main
