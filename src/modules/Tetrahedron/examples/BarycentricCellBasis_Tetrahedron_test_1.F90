!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Checked example for TetrahedronInterpolationUtility.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE TetrahedronInterpolationUtility, ONLY: BarycentricCellBasis_Tetrahedron
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: points(4,4),v(4,1)
INTEGER(I4B) :: i

points = 0.0_DFP
DO i = 1,4
  points(i,i) = 1.0_DFP
END DO
v = BarycentricCellBasis_Tetrahedron(pb=4_I4B,lambda=points)
CALL OK(MAXVAL(ABS(v)) < 1.0E-11_DFP, &
  "Higher entity modes vanish at every vertex")
END PROGRAM main
