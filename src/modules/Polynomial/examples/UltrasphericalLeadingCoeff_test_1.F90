!> author: OpenAI Codex
! date: 2026-10-30
! summary: Return the coefficient of the highest power.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE UltrasphericalPolynomialUtility, ONLY: UltrasphericalLeadingCoeff
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: y

y = UltrasphericalLeadingCoeff(lambda=0.5_DFP, n=2_I4B)
CALL OK(ABS(y-(1.5_DFP)) < 1.0E-11_DFP, &
  "Degree two coefficient or norm")
END PROGRAM main
