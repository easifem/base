!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Checked example for TetrahedronInterpolationUtility.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE TetrahedronInterpolationUtility, ONLY: OrthogonalBasis_Tetrahedron
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: x(3,2),v(2,4)

x(:,1) = [0.1_DFP,0.2_DFP,0.1_DFP]
x(:,2) = [0.2_DFP,0.1_DFP,0.2_DFP]
v = OrthogonalBasis_Tetrahedron(order=1_I4B,xij=x,refTetrahedron="UNIT")
CALL OK(MAXVAL(ABS(v(:,1)-1.0_DFP)) < 1.0E-11_DFP, "Constant mode")
CALL OK(MAXVAL(ABS(v(:,2)-(4.0_DFP*x(3,:)-1.0_DFP))) &
  < 1.0E-11_DFP, "First nonconstant UNIT mode")
END PROGRAM main
