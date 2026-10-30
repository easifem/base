!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Evaluate derivatives of Lobatto kernel modes.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE LobattoPolynomialUtility, ONLY: LobattoKernelGradientEvalAll
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: x(3), y(3, 0:2)

x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
y = LobattoKernelGradientEvalAll(n=2_I4B, x=x)
CALL OK(MAXVAL(ABS(y(:, 0)-(0.0_DFP))) &
  < 1.0E-11_DFP, "Constant kernel mode")
END PROGRAM main
