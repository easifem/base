!> author: OpenAI Codex
! date: 2026-10-30
! summary: Return the ratio of consecutive squared norms.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE Chebyshev1PolynomialUtility, ONLY: Chebyshev1NormSQRRatio
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: y

y = Chebyshev1NormSQRRatio(n=2_I4B)
CALL OK(ABS(y-(1.0_DFP)) < 1.0E-11_DFP, &
  "Degree two coefficient or norm")
END PROGRAM main
