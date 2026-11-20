!> author: Vikas Sharma, Ph.D.
! date: 2026-11-20
! summary: Element type numbers from line element names.

PROGRAM main
USE GlobalData, ONLY: Line2
USE GlobalData, ONLY: Line4
USE GlobalData, ONLY: Line11
USE GlobalData, ONLY: Point1
USE ReferenceLine_Method, ONLY: ElementType_Line
USE Test_Method, ONLY: OK
IMPLICIT NONE

! Test 1: point names
BLOCK
  CALL OK(ElementType_Line("Point") == Point1, "Point gives Point1")
  CALL OK(ElementType_Line("Point1") == Point1, "Point1 gives Point1")
  CALL OK(ElementType_Line("Line1") == Point1, "Line1 gives Point1")
END BLOCK

! Test 2: line names
BLOCK
  CALL OK(ElementType_Line("Line") == Line2, "Line gives Line2")
  CALL OK(ElementType_Line("Line2") == Line2, "Line2 gives Line2")
  CALL OK(ElementType_Line("Line4") == Line4, "Line4 gives Line4")
  CALL OK(ElementType_Line("Line11") == Line11, "Line11 gives Line11")
END BLOCK

! Test 3: names are case sensitive; unknown names give 0
BLOCK
  CALL OK(ElementType_Line("line2") == 0, "Lower case name gives 0")
  CALL OK(ElementType_Line("Triangle3") == 0, "Unknown name gives 0")
END BLOCK

END PROGRAM main
