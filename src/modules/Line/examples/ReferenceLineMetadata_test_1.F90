!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Checked example for ReferenceLine_Method.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE ReferenceLine_Method, ONLY: ElementName_Line
USE ReferenceLine_Method, ONLY: ElementType_Line
USE ReferenceLine_Method, ONLY: ElementOrder_Line
USE ReferenceLine_Method, ONLY: TotalNodesInElement_Line
USE ReferenceLine_Method, ONLY: TotalEntities_Line
USE ReferenceLine_Method, ONLY: MaxOrder_Line
USE ReferenceLine_Method, ONLY: LineName
USE ReferenceLine_Method, ONLY: DEFAULT_Ref_LINE_COORD
USE Test_Method, ONLY: OK
IMPLICIT NONE
INTEGER(I4B) :: kind, entities(4)

kind = ElementType_Line("Line2")
CALL OK(ElementName_Line(kind) == "Line2", "Name round trip")
CALL OK(ElementOrder_Line(kind) == 1, "Linear element order")
CALL OK(TotalNodesInElement_Line(kind) == 2, "Vertex count")
entities = TotalEntities_Line(kind)
CALL OK(entities(1) == 2, "Entity counts start with node count")
CALL OK(MaxOrder_Line >= 1, "Configured maximum order")
CALL OK(LineName(1_I4B) == kind, "Order to element identifier")
CALL OK(ALL(SHAPE(DEFAULT_Ref_LINE_COORD) == [3, 2]), &
  "Compile time default coordinate storage")
END PROGRAM main
