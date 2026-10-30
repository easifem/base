!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Checked example for TetrahedronInterpolationUtility.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE TetrahedronInterpolationUtility, ONLY: EdgeConnectivity_Tetrahedron
USE TetrahedronInterpolationUtility, ONLY: FacetConnectivity_Tetrahedron
USE Test_Method, ONLY: OK
IMPLICIT NONE
INTEGER(I4B) :: edges(2,6),nodal(3,4),modal(3,4)

edges = EdgeConnectivity_Tetrahedron("Lagrange","H1")
nodal = FacetConnectivity_Tetrahedron("Lagrange","H1")
modal = FacetConnectivity_Tetrahedron("Hierarchy","H1")
CALL OK(ALL(edges(:,1) == [1_I4B,2_I4B]) .AND. &
  ALL(edges(:,6) == [3_I4B,4_I4B]), "Six pairwise vertex edges")
CALL OK(ALL(nodal(:,1) == [1_I4B,3_I4B,2_I4B]), "Nodal facet orientation")
CALL OK(ALL(modal(:,1) == [1_I4B,2_I4B,3_I4B]), "Modal facet orientation")
END PROGRAM main
