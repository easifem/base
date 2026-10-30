!> author: OpenAI Codex
! date: 2026-10-30
! summary: Return the weighted squared norm.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE Chebyshev1PolynomialUtility, ONLY: Chebyshev1NormSQR
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: y

y = Chebyshev1NormSQR(n=2_I4B)
CALL OK(ABS(y-(ACOS(-1.0_DFP)/2.0_DFP)) < 1.0E-11_DFP, &
  "Degree two coefficient or norm")
END PROGRAM main
