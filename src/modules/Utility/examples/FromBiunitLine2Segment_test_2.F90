!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: FromBiunitLine2Segment for a segment in 2D space

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE MappingUtility, ONLY: FromBiunitLine2Segment
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP) :: xin(3), x1(2), x2(2), ans(2, 3)

xin = [-1.0_DFP, 0.0_DFP, 1.0_DFP]
x1 = [0.0_DFP, 0.0_DFP]
x2 = [2.0_DFP, 4.0_DFP]

ans = FromBiunitLine2Segment(xin, x1, x2)
CALL OK(ALL(ABS(ans(:, 1) - x1) .LT. tol), "xi = -1 gives x1")
CALL OK(ALL(ABS(ans(:, 3) - x2) .LT. tol), "xi = 1 gives x2")
CALL OK(ALL(ABS(ans(:, 2) - [1.0_DFP, 2.0_DFP]) .LT. tol), &
        "xi = 0 gives the mid point")

! the shape of the result is (SIZE(x1), SIZE(xin))
CALL OK(ALL(SHAPE(FromBiunitLine2Segment(xin, x1, x2)) .EQ. &
            [2_I4B, 3_I4B]), "shape of the result")

END PROGRAM main
