!> author: OpenAI Codex
! date: 2026-10-30
! summary: Checked example for QuadrangleInterpolationUtility.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE QuadrangleInterpolationUtility, ONLY: DubinerGradient_Quadrangle_
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: x(2,2),v(2,3,2)
INTEGER(I4B) :: nr,nc,nd

x(:,1) = [0.2_DFP,-0.4_DFP]
x(:,2) = [-0.3_DFP,0.5_DFP]
CALL DubinerGradient_Quadrangle_(order=1_I4B,xij=x,ans=v,tsize1=nr, &
  tsize2=nc,tsize3=nd)
CALL OK(MAXVAL(ABS(v(:,1,:))) < 1.0E-12_DFP, "Constant gradient")
CALL OK(MAXVAL(ABS(v(:,2,2)-1.5_DFP)) < 1.0E-12_DFP, &
  "Jacobi linear mode derivative")
END PROGRAM main
