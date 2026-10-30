!> author: OpenAI Codex
! date: 2026-10-30
! summary: Return Lobatto interpolation nodes.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE LobattoPolynomialUtility, ONLY: LobattoZeros
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: z(3)

z = LobattoZeros(n=3_I4B)
CALL OK(MAXVAL(ABS(z- &
  [-1.0_DFP, 0.0_DFP, 1.0_DFP])) < 1.0E-11_DFP, &
  "Three nodes include both endpoints")
END PROGRAM main
