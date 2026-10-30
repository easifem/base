!> author: OpenAI Codex
! date: 2026-10-30
! summary: Checked example for QuadrangleInterpolationUtility.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE QuadrangleInterpolationUtility, ONLY: IJ2VEFC_Quadrangle_Clockwise
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: xi(2,2),eta(2,2),nodes(2,4)

xi(:,1) = [-1.0_DFP,1.0_DFP]
xi(:,2) = xi(:,1)
eta(1,:) = [-1.0_DFP,1.0_DFP]
eta(2,:) = eta(1,:)
CALL IJ2VEFC_Quadrangle_Clockwise(xi,eta,nodes, &
  p=1_I4B,q=1_I4B,startNode=1_I4B)
CALL OK(MAXVAL(ABS(nodes(:,1)+1.0_DFP)) < 1.0E-12_DFP, &
  "Requested first corner")
CALL OK(ALL(ABS(nodes) == 1.0_DFP), "All four corners are retained")
CALL OK(ABS(SUM(nodes(1,:))) < 1.0E-12_DFP .AND. &
  ABS(SUM(nodes(2,:))) < 1.0E-12_DFP, "Each coordinate is balanced")
END PROGRAM main
