!> author: OpenAI Codex
! date: 2026-10-30
! summary: Checked example for QuadrangleInterpolationUtility.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE QuadrangleInterpolationUtility, ONLY: EquidistanceInPoint_Quadrangle
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP), ALLOCATABLE :: nodes(:,:)

nodes = EquidistanceInPoint_Quadrangle(order=2_I4B)
CALL OK(ALL(SHAPE(nodes) == [2,1]), &
  "Interpolation coordinate dimensions")
CALL OK(MAXVAL(ABS(nodes-(0.0_DFP))) &
  < 1.0E-11_DFP, "Single interior point is the centroid")
END PROGRAM main
