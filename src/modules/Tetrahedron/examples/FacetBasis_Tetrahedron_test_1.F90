!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Checked example for TetrahedronInterpolationUtility.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE TetrahedronInterpolationUtility, ONLY: FacetBasis_Tetrahedron
USE ReferenceTetrahedron_Method, ONLY: RefCoord_Tetrahedron
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: points(3,4),v(4,4)
INTEGER(I4B) :: i

points = RefCoord_Tetrahedron("UNIT")
v = FacetBasis_Tetrahedron(ps1=3_I4B,ps2=3_I4B,ps3=3_I4B,ps4=3_I4B, &
  xij=points,refTetrahedron="UNIT")
CALL OK(MAXVAL(ABS(v)) < 1.0E-11_DFP, &
  "Higher entity modes vanish at every vertex")
END PROGRAM main
