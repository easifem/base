!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Checked example for TriangleInterpolationUtility.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE TriangleInterpolationUtility, ONLY: IJ2VEFC_Triangle
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: xi(2,2), eta(2,2), nodes(2,3)

xi(:,1) = [0.0_DFP,1.0_DFP]
xi(:,2) = xi(:,1)
eta(1,:) = [0.0_DFP,1.0_DFP]
eta(2,:) = eta(1,:)
CALL IJ2VEFC_Triangle(xi,eta,nodes,order=1_I4B,N=3_I4B)
CALL OK(MAXVAL(ABS(nodes-RESHAPE( &
  [0.0_DFP,0.0_DFP,1.0_DFP,0.0_DFP,0.0_DFP,1.0_DFP],[2,3]))) &
  < 1.0E-12_DFP, "Grid vertices become VEFC coordinates")
END PROGRAM main
