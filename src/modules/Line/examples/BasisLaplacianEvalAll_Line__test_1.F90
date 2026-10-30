!> author: OpenAI Codex
! date: 2026-10-30
! summary: Checked example for LineInterpolationUtility.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: poly
USE LineInterpolationUtility, ONLY: BasisLaplacianEvalAll_Line_
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: x(1,3), y(3,3)
INTEGER(I4B) :: nr,nc

x(1,:) = [-0.5_DFP,0.0_DFP,0.5_DFP]
CALL BasisLaplacianEvalAll_Line_(order=2_I4B, x=x(1,:), &
  refLine="BIUNIT", basisType=poly%Monomial, ans=y, nrow=nr,ncol=nc)
CALL OK(MAXVAL(ABS(y(:,3)- &
  (2.0_DFP))) < 1.0E-10_DFP, "Analytic quadratic mode")
END PROGRAM main
