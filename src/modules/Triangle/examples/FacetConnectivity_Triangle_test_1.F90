!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Checked example for TriangleInterpolationUtility.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE TriangleInterpolationUtility, ONLY: FacetConnectivity_Triangle
USE Test_Method, ONLY: OK
IMPLICIT NONE
INTEGER(I4B) :: edges(2,3)

edges = FacetConnectivity_Triangle()
CALL OK(ALL(edges(:,1) == [1_I4B,2_I4B]) .AND. &
  ALL(edges(:,2) == [2_I4B,3_I4B]) .AND. &
  ALL(edges(:,3) == [3_I4B,1_I4B]), "Counterclockwise boundary edges")
END PROGRAM main
