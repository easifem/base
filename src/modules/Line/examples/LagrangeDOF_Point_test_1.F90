!> author: Vikas Sharma, Ph.D.
! date: 2026-11-20
! summary: A point has one Lagrange DOF for every order.

PROGRAM main
USE GlobalData, ONLY: I4B
USE LineInterpolationUtility, ONLY: LagrangeDOF_Point
USE Test_Method, ONLY: OK
IMPLICIT NONE

! Test 1: the result is 1 for any order
BLOCK
  CALL OK(LagrangeDOF_Point(0_I4B) == 1, "Order 0: 1 DOF")
  CALL OK(LagrangeDOF_Point(1_I4B) == 1, "Order 1: 1 DOF")
  CALL OK(LagrangeDOF_Point(5_I4B) == 1, "Order 5: 1 DOF")
END BLOCK

END PROGRAM main
