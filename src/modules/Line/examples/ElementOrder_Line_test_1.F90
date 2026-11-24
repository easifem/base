!> author: Vikas Sharma, Ph.D.
! date: 2026-11-20
! summary: Polynomial order of the line element types.

PROGRAM main
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Line2
USE GlobalData, ONLY: Line3
USE GlobalData, ONLY: Line11
USE GlobalData, ONLY: Point1
USE ReferenceLine_Method, ONLY: ElementOrder_Line
USE Test_Method, ONLY: OK
IMPLICIT NONE

! Test 1: order of Line2 to Line11
BLOCK
  CALL OK(ElementOrder_Line(Line2) == 1, "Line2 has order 1")
  CALL OK(ElementOrder_Line(Line3) == 2, "Line3 has order 2")
  CALL OK(ElementOrder_Line(Line11) == 10, "Line11 has order 10")
END BLOCK

! Test 2: Point1 and unknown types give 0
BLOCK
  CALL OK(ElementOrder_Line(Point1) == 0, "Point1 has order 0")
  CALL OK(ElementOrder_Line(-1_I4B) == 0, "Unknown type gives 0")
END BLOCK

END PROGRAM main
