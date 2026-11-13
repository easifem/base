!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: BarycentricCoordBiUnitTetrahedron_ without allocation

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE MappingUtility, ONLY: BarycentricCoordBiUnitTetrahedron
USE MappingUtility, ONLY: BarycentricCoordBiUnitTetrahedron_
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP) :: xb(3, 5), ans(5, 6)
INTEGER(I4B) :: nrow, ncol

! vertices and centroid of the biunit tetrahedron
xb(:, 1) = [-1.0_DFP, -1.0_DFP, -1.0_DFP]
xb(:, 2) = [1.0_DFP, -1.0_DFP, -1.0_DFP]
xb(:, 3) = [-1.0_DFP, 1.0_DFP, -1.0_DFP]
xb(:, 4) = [-1.0_DFP, -1.0_DFP, 1.0_DFP]
xb(:, 5) = -0.5_DFP

! ans can be larger, only ans(1:nrow, 1:ncol) is written
CALL BarycentricCoordBiUnitTetrahedron_(xb, ans, nrow, ncol)
CALL OK(nrow .EQ. 4_I4B, "nrow is 4")
CALL OK(ncol .EQ. 5_I4B, "ncol is SIZE(xin, 2)")
CALL OK(ALL(ABS(ans(1:nrow, 1:ncol) - &
                BarycentricCoordBiUnitTetrahedron(xb)) .LT. tol), &
        "same as BarycentricCoordBiUnitTetrahedron")

END PROGRAM main
