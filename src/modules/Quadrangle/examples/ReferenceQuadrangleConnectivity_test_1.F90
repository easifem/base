!> author: OpenAI Codex
! date: 2026-10-30
! summary: Checked example for ReferenceQuadrangle_Method.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Quadrangle4
USE BaseType, ONLY: ReferenceTopology_
USE BaseType, ONLY: ReferenceElement_
USE ReferenceQuadrangle_Method, ONLY: GetEdgeConnectivity_Quadrangle
USE ReferenceQuadrangle_Method, ONLY: GetFaceConnectivity_Quadrangle
USE ReferenceQuadrangle_Method, ONLY: GetFaceElemType_Quadrangle
USE ReferenceQuadrangle_Method, ONLY: FacetTopology_Quadrangle
USE ReferenceQuadrangle_Method, ONLY: FacetElements_Quadrangle
USE Test_Method, ONLY: OK
IMPLICIT NONE
INTEGER(I4B), PARAMETER :: kind = Quadrangle4
INTEGER(I4B) :: edge(2,4), face(2,4), nr,nc
INTEGER(I4B) :: types(4),counts(4)
TYPE(ReferenceTopology_) :: topology(4)
TYPE(ReferenceElement_) :: facets(4)

CALL GetEdgeConnectivity_Quadrangle(con=edge,order=1_I4B,nrow=nr,ncol=nc)
CALL OK(nr == 2 .AND. nc == 4, "Edge connectivity dimensions")
CALL GetFaceConnectivity_Quadrangle(con=face,order=1_I4B,nrow=nr,ncol=nc)
CALL OK(nr == 2 .AND. nc == 4, "Facet dimensions")
CALL GetFaceElemType_Quadrangle(elemType=kind, &
  faceElemType=types,tFaceNodes=counts)
CALL OK(ALL(counts == 2), "Linear facet node count")
CALL FacetTopology_Quadrangle(elemType=kind, &
  nptrs=[1_I4B,2_I4B,3_I4B,4_I4B],ans=topology)
CALL OK(ALL(topology(1)%nptrs == face(:,1)), "First facet pointers")
CALL FacetElements_Quadrangle(elemType=kind,nsd=2_I4B,ans=facets)
CALL OK(ALL(facets%order == 1), "Linear reference facets")
END PROGRAM main
