!> author: OpenAI Codex
! date: 2026-10-30
! summary: Checked example for QuadrangleInterpolationUtility.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE QuadrangleInterpolationUtility, &
  ONLY: HeirarchicalBasisGradient_Quadrangle_
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: x(2,2),v(2,4,2)
INTEGER(I4B) :: nr,nc,nd

x(:,1) = [0.2_DFP,-0.4_DFP]
x(:,2) = [-0.3_DFP,0.5_DFP]
CALL HeirarchicalBasisGradient_Quadrangle_(p=1_I4B,q=1_I4B,xij=x, &
  ans=v,dim1=nr,dim2=nc,dim3=nd)
CALL OK(MAXVAL(ABS(SUM(v,DIM=2))) &
  < 1.0E-12_DFP, "Linear vertex partition identity")
END PROGRAM main
