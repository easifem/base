!> author: Vikas Sharma, Ph.D.
! date: 2026-11-20
! summary: Total number of DOFs of a line element for any basis.

PROGRAM main
USE GlobalData, ONLY: I4B
USE LineInterpolationUtility, ONLY: GetTotalDOF_Line
USE Test_Method, ONLY: OK
IMPLICIT NONE

! Test 1: H1 Lagrange
BLOCK
  CALL OK(GetTotalDOF_Line(3_I4B, "H1", "Lagrange") == 4, &
          "Cubic H1 Lagrange: 4 DOFs")
END BLOCK

! Test 2: the basis does not change the count
BLOCK
  CALL OK(GetTotalDOF_Line(3_I4B, "DG", "Hierarchy") == 4, &
          "Cubic DG Hierarchy: 4 DOFs")
  CALL OK(GetTotalDOF_Line(0_I4B, "H1", "Lagrange") == 1, &
          "Order 0: 1 DOF")
END BLOCK

END PROGRAM main
