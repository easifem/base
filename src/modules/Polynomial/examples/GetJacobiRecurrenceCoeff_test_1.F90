!> author: OpenAI Codex
! date: 2026-10-30
! summary: Return monic three term recurrence coefficients.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE JacobiPolynomialUtility, ONLY: GetJacobiRecurrenceCoeff
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: a(0:2), b(0:2)

CALL GetJacobiRecurrenceCoeff( &
  alpha=0.0_DFP, beta=0.0_DFP, n=3_I4B, alphaCoeff=a, betaCoeff=b)
CALL OK(MAXVAL(ABS(a)) < 1.0E-11_DFP, &
  "Symmetric family has zero diagonal")
CALL OK(ABS(b(2)-4.0_DFP/15.0_DFP) &
  < 1.0E-11_DFP, "Monic beta coefficient")
END PROGRAM main
