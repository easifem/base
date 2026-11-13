!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Checked example for TetrahedronInterpolationUtility.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE TetrahedronInterpolationUtility, ONLY: EdgeBasis_Tetrahedron
USE ReferenceTetrahedron_Method, ONLY: RefCoord_Tetrahedron
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: points(3,4),v(4,6)
INTEGER(I4B) :: i

points = RefCoord_Tetrahedron("UNIT")
v = EdgeBasis_Tetrahedron(pe1=2_I4B,pe2=2_I4B,pe3=2_I4B,pe4=2_I4B, &
  pe5=2_I4B,pe6=2_I4B,xij=points,refTetrahedron="UNIT")
CALL OK(MAXVAL(ABS(v)) < 1.0E-11_DFP, &
  "Higher entity modes vanish at every vertex")
END PROGRAM main
