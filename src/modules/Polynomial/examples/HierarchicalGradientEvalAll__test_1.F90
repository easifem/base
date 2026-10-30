!> author: OpenAI Codex
! date: 2026-10-30
! summary: Evaluate gradients of hierarchical element basis functions.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Line
USE HierarchicalPolynomialUtility, ONLY: HierarchicalGradientEvalAll_
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: x(1, 3)
REAL(DFP) :: y(3, 2, 1)
INTEGER(I4B) :: nr, nc, nd

x(1, :) = [-1.0_DFP, 0.0_DFP, 1.0_DFP]
CALL HierarchicalGradientEvalAll_(elemType=Line, xij=x, &
  domainName="BIUNIT", cellOrder=[1_I4B], cellOrient=[1_I4B], &
  faceOrder=RESHAPE([1_I4B], [1, 1]), edgeOrder=[1_I4B], &
  faceOrient=RESHAPE([1_I4B], [1, 1]), edgeOrient=[1_I4B], &
  ans=y, dim1=nr, dim2=nc, dim3=nd)
CALL OK(MAXVAL(ABS(y(:, 1, 1)+0.5_DFP)) &
  < 1.0E-11_DFP, "Linear endpoint modes")
END PROGRAM main
