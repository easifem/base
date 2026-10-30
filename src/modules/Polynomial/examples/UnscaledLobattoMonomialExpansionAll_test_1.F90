!> author: OpenAI Codex
! date: 2026-10-30
! summary: Expand all polynomials through degree n in monomials.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE UnscaledLobattoPolynomialUtility, ONLY: &
  UnscaledLobattoMonomialExpansionAll
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: c(3, 3)

c = UnscaledLobattoMonomialExpansionAll(n=2_I4B)
CALL OK(MAXVAL(ABS(c(:, 3)- &
  ([-1.5_DFP, 0.0_DFP, 1.5_DFP]/3.0_DFP))) < 1.0E-11_DFP, &
  "Coefficients are ordered by increasing power")
END PROGRAM main
