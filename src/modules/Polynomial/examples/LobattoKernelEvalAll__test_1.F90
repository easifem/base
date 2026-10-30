!> author: OpenAI Codex
! date: 2026-10-30
! summary: Evaluate Lobatto kernel modes.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE LobattoPolynomialUtility, ONLY: LobattoKernelEvalAll_
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: x(3), y(3, 0:2)
INTEGER(I4B) :: nr, nc

x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
CALL LobattoKernelEvalAll_(n=2_I4B, x=x, ans=y, nrow=nr, ncol=nc)
CALL OK(MAXVAL(ABS(y(:, 0)-(-SQRT(6.0_DFP)))) &
  < 1.0E-11_DFP, "Constant kernel mode")
END PROGRAM main
