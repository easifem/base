!> author: OpenAI Codex
! date: 2026-10-30
! summary: Return monic three term recurrence coefficients.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE Chebyshev1PolynomialUtility, ONLY: GetChebyshev1RecurrenceCoeff
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: a(0:2), b(0:2)

CALL GetChebyshev1RecurrenceCoeff( &
  n=3_I4B, alphaCoeff=a, betaCoeff=b)
CALL OK(MAXVAL(ABS(a)) < 1.0E-11_DFP, &
  "Symmetric family has zero diagonal")
CALL OK(ABS(b(2)-0.25_DFP) &
  < 1.0E-11_DFP, "Monic beta coefficient")
END PROGRAM main
