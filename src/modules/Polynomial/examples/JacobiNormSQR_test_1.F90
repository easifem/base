!> author: OpenAI Codex
! date: 2026-10-30
! summary: Return the weighted squared norm.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE JacobiPolynomialUtility, ONLY: JacobiNormSQR
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: y

y = JacobiNormSQR(alpha=0.0_DFP, beta=0.0_DFP, n=2_I4B)
CALL OK(ABS(y-(2.0_DFP/5.0_DFP)) < 1.0E-11_DFP, &
  "Degree two coefficient or norm")
END PROGRAM main
