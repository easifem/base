!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Checked example for ReferenceLine_Method.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Line2
USE BaseType, ONLY: ReferenceTopology_
USE BaseType, ONLY: ReferenceElement_
USE ReferenceLine_Method, ONLY: GetEdgeConnectivity_Line
USE ReferenceLine_Method, ONLY: GetFaceConnectivity_Line
USE ReferenceLine_Method, ONLY: GetFaceElemType_Line
USE ReferenceLine_Method, ONLY: FacetTopology_Line
USE ReferenceLine_Method, ONLY: FacetElements_Line
USE Test_Method, ONLY: OK
IMPLICIT NONE
INTEGER(I4B), PARAMETER :: kind = Line2
INTEGER(I4B) :: edge(1, 2), face(1, 2)
INTEGER(I4B) :: faceTypes(2), nodeCounts(2), nr, nc
TYPE(ReferenceTopology_) :: topology(2)
TYPE(ReferenceElement_) :: facets(2)

CALL GetEdgeConnectivity_Line(con=edge, order=1_I4B, &
  nrow=nr, ncol=nc)
CALL OK(nr == 1 .AND. nc == 2, "Connectivity dimensions")
CALL GetFaceConnectivity_Line(con=face, order=1_I4B)
CALL GetFaceElemType_Line(elemType=kind, &
  faceElemType=faceTypes, tFaceNodes=nodeCounts)
CALL OK(ALL(nodeCounts == 1), "Linear facet node count")
CALL FacetTopology_Line(elemType=kind, &
  nptrs=[1_I4B, 2_I4B], ans=topology)
CALL OK(ALL(topology(1)%nptrs == face(:, 1)), "Facet pointers")
CALL FacetElements_Line(elemType=kind, nsd=1_I4B, ans=facets)
CALL OK(ALL(facets%order == 0), "Facet polynomial order")
END PROGRAM main
