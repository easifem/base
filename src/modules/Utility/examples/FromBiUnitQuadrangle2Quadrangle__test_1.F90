!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: FromBiUnitQuadrangle2Quadrangle_ for a quadrangle in 3D

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE MappingUtility, ONLY: FromBiUnitQuadrangle2Quadrangle_
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP) :: xin(2, 2), x1(3), x2(3), x3(3), x4(3), ans(4, 5)
INTEGER(I4B) :: nrow, ncol

! centre and first vertex of the biunit quadrangle
xin = RESHAPE([0.0_DFP, 0.0_DFP, -1.0_DFP, -1.0_DFP], [2_I4B, 2_I4B])

! the unit square lifted to z = 2
x1 = [0.0_DFP, 0.0_DFP, 2.0_DFP]
x2 = [1.0_DFP, 0.0_DFP, 2.0_DFP]
x3 = [1.0_DFP, 1.0_DFP, 2.0_DFP]
x4 = [0.0_DFP, 1.0_DFP, 2.0_DFP]

CALL FromBiUnitQuadrangle2Quadrangle_(xin, x1, x2, x3, x4, ans, nrow, ncol)
CALL OK(nrow .EQ. 3_I4B, "nrow is SIZE(x1)")
CALL OK(ncol .EQ. 2_I4B, "ncol is SIZE(xin, 2)")
CALL OK(ALL(ABS(ans(1:3, 1) - [0.5_DFP, 0.5_DFP, 2.0_DFP]) .LT. tol), &
        "centre")
CALL OK(ALL(ABS(ans(1:3, 2) - x1) .LT. tol), "first vertex")

END PROGRAM main
