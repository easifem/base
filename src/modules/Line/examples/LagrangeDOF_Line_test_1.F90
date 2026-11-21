!> author: Vikas Sharma, Ph.D.
! date: 2026-11-20
! summary: Total number of Lagrange DOFs of a line, order + 1.

PROGRAM main
USE GlobalData, ONLY: I4B
USE LineInterpolationUtility, ONLY: LagrangeDOF_Line
USE Test_Method, ONLY: OK
IMPLICIT NONE

! Test 1: total DOFs for some orders
BLOCK
  CALL OK(LagrangeDOF_Line(0_I4B) == 1, "Order 0: 1 DOF")
  CALL OK(LagrangeDOF_Line(1_I4B) == 2, "Order 1: 2 DOFs")
  CALL OK(LagrangeDOF_Line(4_I4B) == 5, "Order 4: 5 DOFs")
END BLOCK

END PROGRAM main
