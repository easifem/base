!> author: OpenAI Codex
! date: 2026-10-30
! summary: Differentiate modal coefficients in the same family.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE LegendrePolynomialUtility, ONLY: LegendreGradientCoeff
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: c(0:2), dc(0:2)

c = [0.0_DFP, 0.0_DFP, 1.0_DFP]
dc = LegendreGradientCoeff(n=2_I4B, coeff=c)
CALL OK(MAXVAL(ABS(dc-[0.0_DFP, 3.0_DFP, 0.0_DFP])) &
  < 1.0E-11_DFP, "Derivative coefficients in the same basis")
END PROGRAM main
