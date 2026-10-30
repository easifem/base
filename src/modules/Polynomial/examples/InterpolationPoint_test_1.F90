!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Construct interpolation nodes for a selected node family.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Line
USE GlobalData, ONLY: Equidistance
USE LagrangePolynomialUtility, ONLY: InterpolationPoint
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: nodes(1, 3)
REAL(DFP) :: y(1, 3)

nodes(1, :) = [-1.0_DFP, 0.0_DFP, 1.0_DFP]
y = InterpolationPoint(order=2_I4B, elemType=Line, ipType=Equidistance, &
  xij=nodes(:, [1, 3]), layout="INCREASING")
CALL OK(MAXVAL(ABS(y-nodes)) < 1.0E-11_DFP, &
  "Interpolation construction")
END PROGRAM main
