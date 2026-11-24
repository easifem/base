!> author: Vikas Sharma, Ph.D.
! date: 2026-11-20
! summary: Number of interior DOFs of a line element; never negative.

PROGRAM main
USE GlobalData, ONLY: I4B
USE LineInterpolationUtility, ONLY: GetTotalInDOF_Line
USE Test_Method, ONLY: OK
IMPLICIT NONE

! Test 1: interior DOFs for order 2 and above
BLOCK
  CALL OK(GetTotalInDOF_Line(2_I4B, "H1", "Lagrange") == 1, &
          "Order 2: 1 interior DOF")
  CALL OK(GetTotalInDOF_Line(5_I4B, "H1", "Hierarchy") == 4, &
          "Order 5: 4 interior DOFs")
END BLOCK

! Test 2: order 1 and order 0 give 0
BLOCK
  CALL OK(GetTotalInDOF_Line(1_I4B, "H1", "Lagrange") == 0, &
          "Order 1: no interior DOF")
  CALL OK(GetTotalInDOF_Line(0_I4B, "H1", "Lagrange") == 0, &
          "Order 0: clipped to 0")
END BLOCK

END PROGRAM main
