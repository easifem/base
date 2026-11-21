!> author: Vikas Sharma, Ph.D.
! date: 2026-11-20
! summary: Number of interior Lagrange DOFs of a line, order - 1.

PROGRAM main
USE GlobalData, ONLY: I4B
USE LineInterpolationUtility, ONLY: LagrangeInDOF_Line
USE Test_Method, ONLY: OK
IMPLICIT NONE

! Test 1: interior DOFs for order 1 and above
BLOCK
  CALL OK(LagrangeInDOF_Line(1_I4B) == 0, "Order 1: no interior DOF")
  CALL OK(LagrangeInDOF_Line(2_I4B) == 1, "Order 2: 1 interior DOF")
  CALL OK(LagrangeInDOF_Line(4_I4B) == 3, "Order 4: 3 interior DOFs")
END BLOCK

! Test 2: order 0 gives -1; the result is not clipped
BLOCK
  CALL OK(LagrangeInDOF_Line(0_I4B) == -1, "Order 0 gives -1")
END BLOCK

END PROGRAM main
