!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Checked example for ReferenceQuadrangle_Method.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE ReferenceQuadrangle_Method, ONLY: ElementName_Quadrangle
USE ReferenceQuadrangle_Method, ONLY: ElementType_Quadrangle
USE ReferenceQuadrangle_Method, ONLY: ElementOrder_Quadrangle
USE ReferenceQuadrangle_Method, ONLY: TotalNodesInElement_Quadrangle
USE ReferenceQuadrangle_Method, ONLY: TotalEntities_Quadrangle
USE ReferenceQuadrangle_Method, ONLY: MaxOrder_Quadrangle
USE ReferenceQuadrangle_Method, ONLY: QuadrangleName
USE ReferenceQuadrangle_Method, ONLY: DEFAULT_OPT_QUADRANGLE_EDGE_CON
USE Test_Method, ONLY: OK
IMPLICIT NONE
INTEGER(I4B) :: kind,counts(4)

kind = ElementType_Quadrangle("Quadrangle4")
CALL OK(ElementName_Quadrangle(kind) == "Quadrangle4", &
  "Element name round trip")
CALL OK(ElementOrder_Quadrangle(kind) == 1, "First order element")
CALL OK(TotalNodesInElement_Quadrangle(kind) == 4, "Four vertices")
counts = TotalEntities_Quadrangle(kind)
CALL OK(ALL(counts == [4_I4B,4_I4B,1_I4B,0_I4B]), &
  "Node, edge, face and cell counts")
CALL OK(MaxOrder_Quadrangle >= 1, "Configured maximum order")
CALL OK(QuadrangleName(1_I4B) == kind, "Order to element identifier")
CALL OK(DEFAULT_OPT_QUADRANGLE_EDGE_CON == 1 .OR. &
  DEFAULT_OPT_QUADRANGLE_EDGE_CON == 2, "Connectivity configuration")
END PROGRAM main
