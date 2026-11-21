!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Testing GetFacetDOF_Tetrahedron with one order per face

PROGRAM main
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
USE TetrahedronInterpolationUtility, ONLY: GetFacetDOF_Tetrahedron
IMPLICIT NONE

! Test 1: a face of order p has (p-1)(p-2)/2 interior modes
BLOCK
  INTEGER(I4B) :: n
  n = GetFacetDOF_Tetrahedron(ps1=2_I4B, ps2=3_I4B, ps3=4_I4B, ps4=5_I4B)
  CALL OK(n .EQ. 10, "face orders 2 to 5 give 0+1+3+6 modes")
END BLOCK

! Test 2: linear and quadratic faces have no interior modes
BLOCK
  CALL OK(GetFacetDOF_Tetrahedron(1_I4B) .EQ. 0, "order 1 gives 0 modes")
  CALL OK(GetFacetDOF_Tetrahedron(2_I4B) .EQ. 0, "order 2 gives 0 modes")
END BLOCK

END PROGRAM main
