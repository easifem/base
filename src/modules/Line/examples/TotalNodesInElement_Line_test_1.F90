!> author: Vikas Sharma, Ph.D.
! date: 2026-11-20
! summary: Number of nodes of the line element types.

PROGRAM main
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Line2
USE GlobalData, ONLY: Line4
USE GlobalData, ONLY: Line11
USE GlobalData, ONLY: Point1
USE ReferenceLine_Method, ONLY: TotalNodesInElement_Line
USE Test_Method, ONLY: OK
IMPLICIT NONE

! Test 1: node counts of known types
BLOCK
  CALL OK(TotalNodesInElement_Line(Point1) == 1, "Point1 has 1 node")
  CALL OK(TotalNodesInElement_Line(Line2) == 2, "Line2 has 2 nodes")
  CALL OK(TotalNodesInElement_Line(Line4) == 4, "Line4 has 4 nodes")
  CALL OK(TotalNodesInElement_Line(Line11) == 11, "Line11 has 11 nodes")
END BLOCK

! Test 2: an unknown type gives 0
BLOCK
  CALL OK(TotalNodesInElement_Line(-1_I4B) == 0, "Unknown type gives 0")
END BLOCK

END PROGRAM main
