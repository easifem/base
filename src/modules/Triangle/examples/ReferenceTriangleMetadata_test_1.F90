!> author: OpenAI Codex
! date: 2026-10-30
! summary: Checked example for ReferenceTriangle_Method.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE ReferenceTriangle_Method, ONLY: ElementName_Triangle
USE ReferenceTriangle_Method, ONLY: ElementType_Triangle
USE ReferenceTriangle_Method, ONLY: ElementOrder_Triangle
USE ReferenceTriangle_Method, ONLY: TotalNodesInElement_Triangle
USE ReferenceTriangle_Method, ONLY: TotalEntities_Triangle
USE ReferenceTriangle_Method, ONLY: MaxOrder_Triangle
USE ReferenceTriangle_Method, ONLY: DEFAULT_OPT_TRIANGLE_EDGE_CON
USE Test_Method, ONLY: OK
IMPLICIT NONE
INTEGER(I4B) :: kind, entities(4)

kind = ElementType_Triangle("Triangle3")
CALL OK(ElementName_Triangle(kind) == "Triangle3", "Name round trip")
CALL OK(ElementOrder_Triangle(kind) == 1, "Linear element order")
CALL OK(TotalNodesInElement_Triangle(kind) == 3, "Vertex count")
entities = TotalEntities_Triangle(kind)
CALL OK(entities(1) == 3, "Entity counts start with node count")
CALL OK(MaxOrder_Triangle >= 1, "Configured maximum order")
CALL OK(DEFAULT_OPT_TRIANGLE_EDGE_CON == 1 .OR. &
  DEFAULT_OPT_TRIANGLE_EDGE_CON == 2, "Configured connectivity option")
END PROGRAM main
