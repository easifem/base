!> author: OpenAI Codex
! date: 2026-10-30
! summary: Checked example for QuadrangleInterpolationUtility.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE QuadrangleInterpolationUtility, ONLY: FacetConnectivity_Quadrangle
USE Test_Method, ONLY: OK
IMPLICIT NONE
INTEGER(I4B) :: edges(2,4)

edges = FacetConnectivity_Quadrangle()
CALL OK(ALL(edges(:,1) == [1_I4B,2_I4B]) .AND. &
  ALL(edges(:,4) == [4_I4B,1_I4B]), "Counterclockwise boundary edges")
END PROGRAM main
