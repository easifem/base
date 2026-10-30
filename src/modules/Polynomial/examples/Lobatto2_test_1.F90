!> author: OpenAI Codex
! date: 2026-10-30
! summary: Evaluate the fixed degree 2 Lobatto polynomial.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE LobattoPolynomialUtility, ONLY: Lobatto2
USE LobattoPolynomialUtility, ONLY: LobattoEval
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: y

y = Lobatto2(x=0.25_DFP)
CALL OK(ABS(y-LobattoEval(n=2_I4B, x=0.25_DFP)) &
  < 1.0E-11_DFP, "Fixed degree and general evaluator agree")
END PROGRAM main
