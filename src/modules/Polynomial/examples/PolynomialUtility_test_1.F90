!> author: OpenAI Codex
! date: 2026-10-30
! summary: Reexport polynomial and element interpolation utilities.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE PolynomialUtility, ONLY: LegendreEval
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: y

y = LegendreEval(n=2_I4B, x=0.0_DFP)
CALL OK(ABS(y+0.5_DFP) < 1.0E-12_DFP, "Umbrella reexports Legendre")
END PROGRAM main
