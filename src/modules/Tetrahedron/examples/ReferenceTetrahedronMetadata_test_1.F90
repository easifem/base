!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Checked example for ReferenceTetrahedron_Method.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE ReferenceTetrahedron_Method, ONLY: ElementName_Tetrahedron
USE ReferenceTetrahedron_Method, ONLY: ElementType_Tetrahedron
USE ReferenceTetrahedron_Method, ONLY: ElementOrder_Tetrahedron
USE ReferenceTetrahedron_Method, ONLY: TotalNodesInElement_Tetrahedron
USE ReferenceTetrahedron_Method, ONLY: TotalEntities_Tetrahedron
USE ReferenceTetrahedron_Method, ONLY: MaxOrder_Tetrahedron
USE Test_Method, ONLY: OK
IMPLICIT NONE
INTEGER(I4B) :: kind,counts(4)

kind = ElementType_Tetrahedron("Tetrahedron4")
CALL OK(ElementName_Tetrahedron(kind) == "Tetrahedron4", &
  "Element name round trip")
CALL OK(ElementOrder_Tetrahedron(kind) == 1, "First order element")
CALL OK(TotalNodesInElement_Tetrahedron(kind) == 4, "Four vertices")
counts = TotalEntities_Tetrahedron(kind)
CALL OK(ALL(counts == [4_I4B,6_I4B,4_I4B,1_I4B]), &
  "Node, edge, face and cell counts")
CALL OK(MaxOrder_Tetrahedron >= 1, "Configured maximum order")
END PROGRAM main
