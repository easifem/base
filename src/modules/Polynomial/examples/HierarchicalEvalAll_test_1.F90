!> author: OpenAI Codex
! date: 2026-10-30
! summary: Evaluate hierarchical element basis functions.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Line
USE HierarchicalPolynomialUtility, ONLY: HierarchicalEvalAll
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: x(1, 3)
REAL(DFP), ALLOCATABLE :: y(:, :)

x(1, :) = [-1.0_DFP, 0.0_DFP, 1.0_DFP]
y = HierarchicalEvalAll(elemType=Line, xij=x, &
  domainName="BIUNIT", cellOrder=[1_I4B], cellOrient=[1_I4B], &
  faceOrder=RESHAPE([1_I4B], [1, 1]), edgeOrder=[1_I4B], &
  faceOrient=RESHAPE([1_I4B], [1, 1]), edgeOrient=[1_I4B])
CALL OK(MAXVAL(ABS(SUM(y, DIM=2)-1.0_DFP)) &
  < 1.0E-11_DFP, "Linear endpoint modes")
END PROGRAM main
