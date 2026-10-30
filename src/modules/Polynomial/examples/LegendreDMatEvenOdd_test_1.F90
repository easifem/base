!> author: OpenAI Codex
! date: 2026-10-30
! summary: Form even and odd blocks by pairing mirrored columns.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE LegendrePolynomialUtility, ONLY: LegendreDMatEvenOdd
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: d(0:3, 0:3), e(0:1, 0:1), o(0:1, 0:1)
INTEGER(I4B) :: j

DO j = 0, 3
  d(:, j) = REAL(j, DFP)
END DO
CALL LegendreDMatEvenOdd(n=3_I4B, D=d, e=e, o=o)
CALL OK(MAXVAL(ABS(e-3.0_DFP)) < 1.0E-11_DFP, &
  "Even block combines mirrored columns")
CALL OK(MAXVAL(ABS(o(:, 0)+3.0_DFP)) < 1.0E-11_DFP, &
  "Odd block subtracts mirrored columns")
END PROGRAM main
