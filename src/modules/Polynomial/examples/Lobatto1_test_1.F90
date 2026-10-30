!> author: OpenAI Codex
! date: 2026-10-30
! summary: Evaluate the fixed degree 1 Lobatto polynomial.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE LobattoPolynomialUtility, ONLY: Lobatto1
USE LobattoPolynomialUtility, ONLY: LobattoEval
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: y

y = Lobatto1(x=0.25_DFP)
CALL OK(ABS(y-LobattoEval(n=1_I4B, x=0.25_DFP)) &
  < 1.0E-11_DFP, "Fixed degree and general evaluator agree")
END PROGRAM main
