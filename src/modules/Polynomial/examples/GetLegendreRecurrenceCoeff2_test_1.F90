!> author: OpenAI Codex
! date: 2026-10-30
! summary: Return standard three term recurrence coefficients.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE LegendrePolynomialUtility, ONLY: GetLegendreRecurrenceCoeff2
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: a(0:2), b(0:2), c(0:2)

CALL GetLegendreRecurrenceCoeff2(n=3_I4B, A=a, B=b, C=c)
CALL OK(MAXVAL(ABS(b)) < 1.0E-11_DFP, &
  "Symmetric family has no constant recurrence term")
CALL OK(ABS(a(1)-1.5_DFP) < 1.0E-11_DFP, &
  "Degree two recurrence multiplier")
END PROGRAM main
