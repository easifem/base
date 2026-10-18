!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: BarycentricCoordTriangle_ without allocation

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE MappingUtility, ONLY: BarycentricCoordTriangle_
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP) :: xin(2, 2), ans(3, 2)

xin = RESHAPE([0.2_DFP, 0.3_DFP, 0.5_DFP, 0.0_DFP], [2_I4B, 2_I4B])

! ans must have 3 rows and SIZE(xin, 2) columns
CALL BarycentricCoordTriangle_(xin, "UNIT", ans)
CALL OK(ALL(ABS(ans(:, 1) - [0.5_DFP, 0.2_DFP, 0.3_DFP]) .LT. tol), &
        "point (0.2, 0.3)")
CALL OK(ALL(ABS(ans(:, 2) - [0.5_DFP, 0.5_DFP, 0.0_DFP]) .LT. tol), &
        "point (0.5, 0.0)")

END PROGRAM main
