!> author: Vikas Sharma, Ph.D.
! date: 2026-11-20
! summary: Line element type number from the polynomial order.

PROGRAM main
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Line2
USE GlobalData, ONLY: Line3
USE GlobalData, ONLY: Line11
USE ReferenceLine_Method, ONLY: ElementOrder_Line
USE ReferenceLine_Method, ONLY: LineName
USE Test_Method, ONLY: OK
IMPLICIT NONE

! Test 1: orders 1, 2 and 10
BLOCK
  CALL OK(LineName(1_I4B) == Line2, "Order 1 gives Line2")
  CALL OK(LineName(2_I4B) == Line3, "Order 2 gives Line3")
  CALL OK(LineName(10_I4B) == Line11, "Order 10 gives Line11")
END BLOCK

! Test 2: LineName is the inverse of ElementOrder_Line
BLOCK
  INTEGER(I4B) :: order
  LOGICAL :: isok
  isok = .TRUE.
  DO order = 1, 10
    isok = isok .AND. (ElementOrder_Line(LineName(order)) == order)
  END DO
  CALL OK(isok, "ElementOrder_Line(LineName(p)) == p")
END BLOCK

! Test 3: orders outside 1 to 10 give 0
BLOCK
  CALL OK(LineName(0_I4B) == 0, "Order 0 gives 0")
  CALL OK(LineName(11_I4B) == 0, "Order 11 gives 0")
END BLOCK

END PROGRAM main
