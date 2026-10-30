!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Checked example for TetrahedronInterpolationUtility.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE TetrahedronInterpolationUtility, &
  ONLY: HeirarchicalBasisGradient_Tetrahedron
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: x(3,2),g(2,4,3)

x(:,1) = [0.1_DFP,0.2_DFP,0.1_DFP]
x(:,2) = [0.2_DFP,0.1_DFP,0.2_DFP]
g = HeirarchicalBasisGradient_Tetrahedron(order=1_I4B,pe1=1_I4B, &
  pe2=1_I4B,pe3=1_I4B,pe4=1_I4B,pe5=1_I4B,pe6=1_I4B,ps1=1_I4B, &
  ps2=1_I4B,ps3=1_I4B,ps4=1_I4B,xij=x,refTetrahedron="UNIT")
CALL OK(MAXVAL(ABS(SUM(g,DIM=2))) < 1.0E-11_DFP, &
  "Vertex gradient partition")
CALL OK(MAXVAL(ABS(g(:,2,1)-1.0_DFP)) < 1.0E-11_DFP, &
  "UNIT barycentric coordinate derivative")
END PROGRAM main
