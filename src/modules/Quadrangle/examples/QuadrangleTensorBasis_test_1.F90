!> author: OpenAI Codex
! date: 2026-10-30
! summary: Checked example for QuadrangleInterpolationUtility.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: poly
USE QuadrangleInterpolationUtility, ONLY: TensorProdBasis_Quadrangle
USE QuadrangleInterpolationUtility, ONLY: OrthogonalBasis_Quadrangle
USE QuadrangleInterpolationUtility, ONLY: OrthogonalBasis_Quadrangle_
USE QuadrangleInterpolationUtility, ONLY: TensorProdBasisGradient_Quadrangle
USE QuadrangleInterpolationUtility, ONLY: OrthogonalBasisGradient_Quadrangle
USE QuadrangleInterpolationUtility, &
  ONLY: OrthogonalBasisGradient_Quadrangle_
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: x(2,2),v(2,6),alias(2,6),g(2,6,2),ga(2,6,2)
INTEGER(I4B) :: nr,nc,nd

x(:,1) = [0.2_DFP,-0.4_DFP]
x(:,2) = [-0.3_DFP,0.5_DFP]
v = TensorProdBasis_Quadrangle(p=2_I4B,q=1_I4B,xij=x, &
  basisType1=poly%Monomial,basisType2=poly%Monomial)
alias = OrthogonalBasis_Quadrangle(p=2_I4B,q=1_I4B,xij=x, &
  basisType1=poly%Monomial,basisType2=poly%Monomial)
CALL OK(MAXVAL(ABS(v-alias)) < 1.0E-12_DFP, "Value aliases agree")
CALL OrthogonalBasis_Quadrangle_(p=2_I4B,q=1_I4B,xij=x, &
  basisType1=poly%Monomial,basisType2=poly%Monomial, &
  ans=alias,nrow=nr,ncol=nc)
CALL OK(nr == 2 .AND. nc == 6, "Anisotropic output dimensions")
CALL OK(MAXVAL(ABS(v(:,6)-x(1,:)**2*x(2,:))) &
  < 1.0E-12_DFP, "First coordinate degree varies fastest")
g = TensorProdBasisGradient_Quadrangle(p=2_I4B,q=1_I4B,xij=x, &
  basisType1=poly%Monomial,basisType2=poly%Monomial)
ga = OrthogonalBasisGradient_Quadrangle(p=2_I4B,q=1_I4B,xij=x, &
  basisType1=poly%Monomial,basisType2=poly%Monomial)
CALL OK(MAXVAL(ABS(g-ga)) < 1.0E-12_DFP, "Gradient aliases agree")
CALL OrthogonalBasisGradient_Quadrangle_(p=2_I4B,q=1_I4B,xij=x, &
  basisType1=poly%Monomial,basisType2=poly%Monomial, &
  ans=ga,dim1=nr,dim2=nc,dim3=nd)
CALL OK(MAXVAL(ABS(ga(:,6,1)-2.0_DFP*x(1,:)*x(2,:))) &
  < 1.0E-12_DFP, "Derivative in first direction")
CALL OK(MAXVAL(ABS(ga(:,6,2)-x(1,:)**2)) < 1.0E-12_DFP, &
  "Derivative in second direction")
END PROGRAM main
