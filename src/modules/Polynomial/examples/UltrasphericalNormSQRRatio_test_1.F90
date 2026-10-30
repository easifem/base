!> author: OpenAI Codex
! date: 2026-10-30
! summary: Return the ratio of consecutive squared norms.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE UltrasphericalPolynomialUtility, ONLY: UltrasphericalNormSQRRatio
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: y

y = UltrasphericalNormSQRRatio(lambda=0.5_DFP, n=2_I4B)
CALL OK(ABS(y-(5.0_DFP/7.0_DFP)) < 1.0E-11_DFP, &
  "Degree two coefficient or norm")
END PROGRAM main
