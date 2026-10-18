!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: BarycentricCoordUnitTetrahedron_ without allocation

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE MappingUtility, ONLY: BarycentricCoordUnitTetrahedron
USE MappingUtility, ONLY: BarycentricCoordUnitTetrahedron_
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP) :: xu(3, 5), ans(5, 6)
INTEGER(I4B) :: nrow, ncol

! vertices and centroid of the unit tetrahedron
xu(:, 1) = [0.0_DFP, 0.0_DFP, 0.0_DFP]
xu(:, 2) = [1.0_DFP, 0.0_DFP, 0.0_DFP]
xu(:, 3) = [0.0_DFP, 1.0_DFP, 0.0_DFP]
xu(:, 4) = [0.0_DFP, 0.0_DFP, 1.0_DFP]
xu(:, 5) = 0.25_DFP

! ans can be larger, only ans(1:nrow, 1:ncol) is written
CALL BarycentricCoordUnitTetrahedron_(xu, ans, nrow, ncol)
CALL OK(nrow .EQ. 4_I4B, "nrow is 4")
CALL OK(ncol .EQ. 5_I4B, "ncol is SIZE(xin, 2)")
CALL OK(ALL(ABS(ans(1:nrow, 1:ncol) - &
                BarycentricCoordUnitTetrahedron(xu)) .LT. tol), &
        "same as BarycentricCoordUnitTetrahedron")

END PROGRAM main
