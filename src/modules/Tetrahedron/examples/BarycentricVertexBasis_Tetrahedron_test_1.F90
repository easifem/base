!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Checked example for TetrahedronInterpolationUtility.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE TetrahedronInterpolationUtility, &
  ONLY: BarycentricVertexBasis_Tetrahedron
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: points(4,4),v(4,4)
INTEGER(I4B) :: i

points = 0.0_DFP
DO i = 1,4
  points(i,i) = 1.0_DFP
END DO
v = BarycentricVertexBasis_Tetrahedron(lambda=points)
CALL OK(MAXVAL(ABS(SUM(v,DIM=2)-1.0_DFP)) < 1.0E-11_DFP, &
  "Linear vertex partition of unity")
CALL OK(ABS(v(1,1)-1.0_DFP) < 1.0E-11_DFP, "First vertex delta")
END PROGRAM main
