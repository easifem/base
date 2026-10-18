!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: BarycentricCoordTetrahedron_ without allocation

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE MappingUtility, ONLY: BarycentricCoordTetrahedron_
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP) :: xin(3, 2), ans(4, 3)
INTEGER(I4B) :: nrow, ncol

xin(:, 1) = [0.1_DFP, 0.2_DFP, 0.3_DFP]
xin(:, 2) = 0.25_DFP

CALL BarycentricCoordTetrahedron_(xin, "UNIT", ans, nrow, ncol)
CALL OK(nrow .EQ. 4_I4B, "nrow is 4")
CALL OK(ncol .EQ. 2_I4B, "ncol is SIZE(xin, 2)")
CALL OK(ALL(ABS(ans(1:4, 1) - [0.4_DFP, 0.1_DFP, 0.2_DFP, 0.3_DFP]) &
            .LT. tol), "point (0.1, 0.2, 0.3)")
CALL OK(ALL(ABS(ans(1:4, 2) - 0.25_DFP) .LT. tol), "centroid")

END PROGRAM main
