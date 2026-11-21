!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Testing LagrangeInDOF_Tetrahedron at low orders

PROGRAM main
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
USE TetrahedronInterpolationUtility, ONLY: LagrangeDOF_Tetrahedron
USE TetrahedronInterpolationUtility, ONLY: LagrangeInDOF_Tetrahedron
IMPLICIT NONE

! Test 1: orders 1 to 3 have no interior nodes
BLOCK
  CALL OK(LagrangeInDOF_Tetrahedron(1_I4B) .EQ. 0, "order 1")
  CALL OK(LagrangeInDOF_Tetrahedron(2_I4B) .EQ. 0, "order 2")
  CALL OK(LagrangeInDOF_Tetrahedron(3_I4B) .EQ. 0, "order 3")
END BLOCK

! Test 2: order 5 has 4 interior nodes, (p-1)(p-2)(p-3)/6
BLOCK
  CALL OK(LagrangeInDOF_Tetrahedron(5_I4B) .EQ. 4, "order 5")
END BLOCK

! Test 3: order 0 has one node; the interior count must not be negative
BLOCK
  CALL OK(LagrangeDOF_Tetrahedron(0_I4B) .EQ. 1, "order 0 has one node")
  CALL OK(LagrangeInDOF_Tetrahedron(0_I4B) .GE. 0, &
          "order 0 interior count is not negative")
END BLOCK

END PROGRAM main
