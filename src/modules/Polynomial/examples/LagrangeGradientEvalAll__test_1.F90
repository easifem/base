!> author: OpenAI Codex
! date: 2026-10-30
! summary: Evaluate gradients of the nodal Lagrange basis.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Line
USE GlobalData, ONLY: LGT
USE GlobalData, ONLY: poly
USE LagrangePolynomialUtility, ONLY: LagrangeGradientEvalAll_
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: nodes(1, 3)
REAL(DFP) :: y(3, 3, 1)
INTEGER(I4B) :: nr, nc, nd
REAL(DFP) :: coeff(3, 3)
LOGICAL(LGT) :: firstCall
REAL(DFP) :: points(1, 3)

nodes(1, :) = [-1.0_DFP, 0.0_DFP, 1.0_DFP]
firstCall = .TRUE.
coeff = 0.0_DFP
points = nodes
CALL LagrangeGradientEvalAll_(order=2_I4B, elemType=Line, x=points, &
  xij=nodes, &
  domainName="BIUNIT", coeff=coeff, firstCall=firstCall, &
  basisType=poly%Monomial, &
  ans=y, dim1=nr, dim2=nc, dim3=nd)
CALL OK(MAXVAL(ABS(SUM(y(:, :, 1), DIM=2))) < 1.0E-10_DFP, &
  "Gradient of partition of unity")
END PROGRAM main
