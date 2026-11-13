!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Checked example for ReferenceTetrahedron_Method.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Tetrahedron4
USE BaseType, ONLY: ReferenceTopology_
USE BaseType, ONLY: ReferenceElement_
USE ReferenceTetrahedron_Method, ONLY: GetEdgeConnectivity_Tetrahedron
USE ReferenceTetrahedron_Method, ONLY: GetFaceConnectivity_Tetrahedron
USE ReferenceTetrahedron_Method, ONLY: GetFaceElemType_Tetrahedron
USE ReferenceTetrahedron_Method, ONLY: FacetTopology_Tetrahedron
USE ReferenceTetrahedron_Method, ONLY: FacetElements_Tetrahedron
USE Test_Method, ONLY: OK
IMPLICIT NONE
INTEGER(I4B), PARAMETER :: kind = Tetrahedron4
INTEGER(I4B) :: edge(2,6), face(3,4), nr,nc
INTEGER(I4B) :: types(4),counts(4)
TYPE(ReferenceTopology_) :: topology(4)
TYPE(ReferenceElement_) :: facets(4)

CALL GetEdgeConnectivity_Tetrahedron(con=edge,order=1_I4B,nrow=nr,ncol=nc)
CALL OK(nr == 2 .AND. nc == 6, "Edge connectivity dimensions")
CALL GetFaceConnectivity_Tetrahedron(con=face,order=1_I4B,nrow=nr,ncol=nc)
CALL OK(nr == 3 .AND. nc == 4, "Facet dimensions")
CALL GetFaceElemType_Tetrahedron(elemType=kind, &
  faceElemType=types,tFaceNodes=counts)
CALL OK(ALL(counts == 3), "Linear facet node count")
CALL FacetTopology_Tetrahedron(elemType=kind, &
  nptrs=[1_I4B,2_I4B,3_I4B,4_I4B],ans=topology)
CALL OK(ALL(topology(1)%nptrs == face(:,1)), "First facet pointers")
CALL FacetElements_Tetrahedron(elemType=kind,nsd=3_I4B,ans=facets)
CALL OK(ALL(facets%order == 1), "Linear reference facets")
END PROGRAM main
