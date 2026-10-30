!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Checked example for ReferenceTriangle_Method.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Triangle3
USE BaseType, ONLY: ReferenceTopology_
USE BaseType, ONLY: ReferenceElement_
USE ReferenceTriangle_Method, ONLY: GetEdgeConnectivity_Triangle
USE ReferenceTriangle_Method, ONLY: GetFaceConnectivity_Triangle
USE ReferenceTriangle_Method, ONLY: GetFaceElemType_Triangle
USE ReferenceTriangle_Method, ONLY: FacetTopology_Triangle
USE ReferenceTriangle_Method, ONLY: FacetElements_Triangle
USE Test_Method, ONLY: OK
IMPLICIT NONE
INTEGER(I4B), PARAMETER :: kind = Triangle3
INTEGER(I4B) :: edge(2, 3), face(2, 3)
INTEGER(I4B) :: faceTypes(3), nodeCounts(3), nr, nc
TYPE(ReferenceTopology_) :: topology(3)
TYPE(ReferenceElement_) :: facets(3)

CALL GetEdgeConnectivity_Triangle(con=edge, order=1_I4B, &
  nrow=nr, ncol=nc)
CALL OK(nr == 2 .AND. nc == 3, "Connectivity dimensions")
CALL GetFaceConnectivity_Triangle(con=face, order=1_I4B)
CALL GetFaceElemType_Triangle(elemType=kind, &
  faceElemType=faceTypes, tFaceNodes=nodeCounts)
CALL OK(ALL(nodeCounts == 2), "Linear facet node count")
CALL FacetTopology_Triangle(elemType=kind, &
  nptrs=[1_I4B, 2_I4B, 3_I4B], ans=topology)
CALL OK(ALL(topology(1)%nptrs == face(:, 1)), "Facet pointers")
CALL FacetElements_Triangle(elemType=kind, nsd=2_I4B, ans=facets)
CALL OK(ALL(facets%order == 1), "Facet polynomial order")
END PROGRAM main
