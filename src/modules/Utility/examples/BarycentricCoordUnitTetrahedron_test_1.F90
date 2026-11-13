!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: BarycentricCoordUnitTetrahedron for the unit tetrahedron

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE MappingUtility, ONLY: BarycentricCoordUnitTetrahedron
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP) :: xu(3, 5), ans(4, 5), eye(4, 4)
INTEGER(I4B) :: ii

! vertices and centroid of the unit tetrahedron
xu(:, 1) = [0.0_DFP, 0.0_DFP, 0.0_DFP]
xu(:, 2) = [1.0_DFP, 0.0_DFP, 0.0_DFP]
xu(:, 3) = [0.0_DFP, 1.0_DFP, 0.0_DFP]
xu(:, 4) = [0.0_DFP, 0.0_DFP, 1.0_DFP]
xu(:, 5) = 0.25_DFP

eye = 0.0_DFP
DO ii = 1_I4B, 4_I4B
  eye(ii, ii) = 1.0_DFP
END DO

ans = BarycentricCoordUnitTetrahedron(xu)
CALL OK(ALL(ABS(ans(:, 1:4) - eye) .LT. tol), "vertices")
CALL OK(ALL(ABS(ans(:, 5) - 0.25_DFP) .LT. tol), "centroid")
CALL OK(ALL(ABS(SUM(ans, 1) - 1.0_DFP) .LT. tol), "coordinates sum to 1")

END PROGRAM main
