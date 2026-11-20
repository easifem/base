!> author: Vikas Sharma, Ph.D.
! date: 2026-11-20
! summary: Counts of nodes, edges, faces and cells of a line element.

PROGRAM main
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Line2
USE GlobalData, ONLY: Line3
USE ReferenceLine_Method, ONLY: TotalEntities_Line
USE Test_Method, ONLY: OK
IMPLICIT NONE

! Test 1: linear line has two nodes and one edge
BLOCK
  CALL OK(ALL(TotalEntities_Line(Line2) == [2, 1, 0, 0]), &
          "Line2 entity counts")
END BLOCK

! Test 2: quadratic line has three nodes and one edge
BLOCK
  CALL OK(ALL(TotalEntities_Line(Line3) == [3, 1, 0, 0]), &
          "Line3 entity counts")
END BLOCK

! Test 3: an unknown type has no nodes, but the edge count stays 1
BLOCK
  CALL OK(ALL(TotalEntities_Line(-1_I4B) == [0, 1, 0, 0]), &
          "Unknown type entity counts")
END BLOCK

END PROGRAM main
