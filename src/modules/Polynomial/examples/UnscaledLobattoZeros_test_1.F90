!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Return Lobatto interpolation nodes.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE UnscaledLobattoPolynomialUtility, ONLY: UnscaledLobattoZeros
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: z(3)

z = UnscaledLobattoZeros(n=3_I4B)
CALL OK(MAXVAL(ABS(z- &
  [-1.0_DFP, 0.0_DFP, 1.0_DFP])) < 1.0E-11_DFP, &
  "Three nodes include both endpoints")
END PROGRAM main
