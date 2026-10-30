!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Checked example for QuadrangleInterpolationUtility.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE QuadrangleInterpolationUtility, ONLY: Dubiner_Quadrangle
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: x(2,2),v(2,3)

x(:,1) = [0.2_DFP,-0.4_DFP]
x(:,2) = [-0.3_DFP,0.5_DFP]
v = Dubiner_Quadrangle(order=1_I4B,xij=x)
CALL OK(MAXVAL(ABS(v(:,1)-1.0_DFP)) < 1.0E-12_DFP, &
  "Constant total degree mode")
CALL OK(MAXVAL(ABS(v(:,2)-(0.5_DFP+1.5_DFP*x(2,:)))) &
  < 1.0E-12_DFP, "First nonconstant Jacobi mode")
END PROGRAM main
