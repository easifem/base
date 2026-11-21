!> author: Vikas Sharma, Ph.D.
! date: 2026-11-20
! summary: Names of the line element types.

PROGRAM main
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Line2
USE GlobalData, ONLY: Line3
USE GlobalData, ONLY: Line11
USE GlobalData, ONLY: Point1
USE ReferenceLine_Method, ONLY: ElementName_Line
USE Test_Method, ONLY: OK
IMPLICIT NONE

! Test 1: names of known types
BLOCK
  CALL OK(ElementName_Line(Point1) == "Point1", "Point1 name")
  CALL OK(ElementName_Line(Line2) == "Line2", "Line2 name")
  CALL OK(ElementName_Line(Line3) == "Line3", "Line3 name")
  CALL OK(ElementName_Line(Line11) == "Line11", "Line11 name")
END BLOCK

! Test 2: an unknown type gives NONE
BLOCK
  CALL OK(ElementName_Line(-1_I4B) == "NONE", "Unknown type gives NONE")
END BLOCK

END PROGRAM main
