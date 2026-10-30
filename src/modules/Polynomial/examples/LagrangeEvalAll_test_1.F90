!> author: OpenAI Codex
! date: 2026-10-30
! summary: Evaluate the nodal Lagrange basis.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Line
USE GlobalData, ONLY: LGT
USE GlobalData, ONLY: poly
USE LagrangePolynomialUtility, ONLY: LagrangeEvalAll
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: nodes(1, 3)
REAL(DFP) :: y(3, 3)
REAL(DFP) :: coeff(3, 3)
LOGICAL(LGT) :: firstCall
REAL(DFP) :: points(1, 3)

nodes(1, :) = [-1.0_DFP, 0.0_DFP, 1.0_DFP]
firstCall = .TRUE.
coeff = 0.0_DFP
points = nodes
y = LagrangeEvalAll(order=2_I4B, elemType=Line, x=points, xij=nodes, &
  domainName="BIUNIT", coeff=coeff, firstCall=firstCall, &
  basisType=poly%Monomial)
CALL OK(MAXVAL(ABS(y-RESHAPE( &
  [1.0_DFP, 0.0_DFP, 0.0_DFP, &
  0.0_DFP, 1.0_DFP, 0.0_DFP, &
  0.0_DFP, 0.0_DFP, 1.0_DFP], [3, 3]))) < 1.0E-10_DFP, &
  "Nodal interpolation identity")
END PROGRAM main
