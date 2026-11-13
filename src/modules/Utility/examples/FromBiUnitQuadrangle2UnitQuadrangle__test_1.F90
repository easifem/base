!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: FromBiUnitQuadrangle2UnitQuadrangle_ without allocation

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE MappingUtility, ONLY: FromBiUnitQuadrangle2UnitQuadrangle_
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP) :: xin(2, 2), ans(3, 4)
INTEGER(I4B) :: nrow, ncol

xin = RESHAPE([0.0_DFP, 0.0_DFP, 0.5_DFP, -0.5_DFP], [2_I4B, 2_I4B])

CALL FromBiUnitQuadrangle2UnitQuadrangle_(xin, ans, nrow, ncol)
CALL OK(nrow .EQ. 2_I4B, "nrow is 2")
CALL OK(ncol .EQ. 2_I4B, "ncol is SIZE(xin, 2)")
CALL OK(ALL(ABS(ans(1:2, 1) - [0.5_DFP, 0.5_DFP]) .LT. tol), "centre")
CALL OK(ALL(ABS(ans(1:2, 2) - [0.75_DFP, 0.25_DFP]) .LT. tol), &
        "interior point")

END PROGRAM main
