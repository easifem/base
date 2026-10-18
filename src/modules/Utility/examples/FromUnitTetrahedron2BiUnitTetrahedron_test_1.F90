!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: FromUnitTetrahedron2BiUnitTetrahedron, unit to biunit

PROGRAM main
USE GlobalData, ONLY: DFP
USE MappingUtility, ONLY: FromUnitTetrahedron2BiUnitTetrahedron
USE MappingUtility, ONLY: FromBiUnitTetrahedron2UnitTetrahedron
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP) :: xb(3, 5), xu(3, 5), ans(3, 5)

! vertices and centroid of the biunit tetrahedron
xb(:, 1) = [-1.0_DFP, -1.0_DFP, -1.0_DFP]
xb(:, 2) = [1.0_DFP, -1.0_DFP, -1.0_DFP]
xb(:, 3) = [-1.0_DFP, 1.0_DFP, -1.0_DFP]
xb(:, 4) = [-1.0_DFP, -1.0_DFP, 1.0_DFP]
xb(:, 5) = -0.5_DFP

! vertices and centroid of the unit tetrahedron
xu(:, 1) = [0.0_DFP, 0.0_DFP, 0.0_DFP]
xu(:, 2) = [1.0_DFP, 0.0_DFP, 0.0_DFP]
xu(:, 3) = [0.0_DFP, 1.0_DFP, 0.0_DFP]
xu(:, 4) = [0.0_DFP, 0.0_DFP, 1.0_DFP]
xu(:, 5) = 0.25_DFP

ans = FromUnitTetrahedron2BiUnitTetrahedron(xu)
CALL OK(ALL(ABS(ans - xb) .LT. tol), "vertices and centroid")

! inverse of FromBiUnitTetrahedron2UnitTetrahedron
ans = FromBiUnitTetrahedron2UnitTetrahedron( &
      FromUnitTetrahedron2BiUnitTetrahedron(xu))
CALL OK(ALL(ABS(ans - xu) .LT. tol), "round trip")

END PROGRAM main
