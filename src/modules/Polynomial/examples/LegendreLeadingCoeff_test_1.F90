!> author: OpenAI Codex
! date: 2026-10-30
! summary: Return the coefficient of the highest power.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE LegendrePolynomialUtility, ONLY: LegendreLeadingCoeff
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: y

y = LegendreLeadingCoeff(n=2_I4B)
CALL OK(ABS(y-(1.5_DFP)) < 1.0E-11_DFP, &
  "Degree two coefficient or norm")
END PROGRAM main
