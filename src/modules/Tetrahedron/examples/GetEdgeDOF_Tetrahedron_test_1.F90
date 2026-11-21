!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Testing GetEdgeDOF_Tetrahedron with one order per edge

PROGRAM main
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
USE TetrahedronInterpolationUtility, ONLY: GetEdgeDOF_Tetrahedron
IMPLICIT NONE

! Test 1: an edge of order p has p-1 interior modes
BLOCK
  INTEGER(I4B) :: n
  n = GetEdgeDOF_Tetrahedron(pe1=1_I4B, pe2=2_I4B, pe3=3_I4B, &
                             pe4=4_I4B, pe5=5_I4B, pe6=6_I4B)
  CALL OK(n .EQ. 15, "edge orders 1 to 6 give 0+1+2+3+4+5 modes")
END BLOCK

! Test 2: linear edges have no interior modes
BLOCK
  CALL OK(GetEdgeDOF_Tetrahedron(1_I4B) .EQ. 0, "order 1 gives 0 modes")
END BLOCK

! Test 3: the single order overload repeats the order on all six edges
BLOCK
  CALL OK(GetEdgeDOF_Tetrahedron(3_I4B) .EQ. &
          GetEdgeDOF_Tetrahedron(3_I4B, 3_I4B, 3_I4B, 3_I4B, 3_I4B, 3_I4B), &
          "single order overload agrees")
END BLOCK

END PROGRAM main
