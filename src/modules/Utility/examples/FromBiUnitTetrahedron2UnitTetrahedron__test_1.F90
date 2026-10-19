!> author: Vikas Sharma, Ph. D.
! date: 2026-10-19
! summary: FromBiUnitTetrahedron2UnitTetrahedron_ without allocation

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE MappingUtility, ONLY: FromBiUnitTetrahedron2UnitTetrahedron_
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP) :: xb(3, 2), ans(3, 4)
INTEGER(I4B) :: nrow, ncol

! vertex 2 and the centroid of the biunit tetrahedron
xb(:, 1) = [1.0_DFP, -1.0_DFP, -1.0_DFP]
xb(:, 2) = -0.5_DFP

! ans may be larger than xin
CALL FromBiUnitTetrahedron2UnitTetrahedron_(xb, ans, nrow, ncol)
CALL OK(nrow .EQ. 3_I4B .AND. ncol .EQ. 2_I4B, "nrow and ncol")
CALL OK(ALL(ABS(ans(:, 1) - [1.0_DFP, 0.0_DFP, 0.0_DFP]) .LT. tol), &
        "vertex 2")
CALL OK(ALL(ABS(ans(:, 2) - 0.25_DFP) .LT. tol), "centroid")

END PROGRAM main
