!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: FromBiunitLine2Segment for a segment [x1, x2] on the real line

PROGRAM main
USE GlobalData, ONLY: DFP
USE MappingUtility, ONLY: FromBiunitLine2Segment
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP) :: xin(3), ans(3), ans1(1)

xin = [-1.0_DFP, 0.0_DFP, 1.0_DFP]

! map [-1, 1] to [2, 6]
ans = FromBiunitLine2Segment(xin, 2.0_DFP, 6.0_DFP)
CALL OK(ALL(ABS(ans - [2.0_DFP, 4.0_DFP, 6.0_DFP]) .LT. tol), &
        "end points and mid point")

! an interior point
ans1 = FromBiunitLine2Segment([0.5_DFP], 2.0_DFP, 6.0_DFP)
CALL OK(ABS(ans1(1) - 5.0_DFP) .LT. tol, "xi = 0.5")

! a reversed segment, x1 > x2
ans = FromBiunitLine2Segment(xin, 6.0_DFP, 2.0_DFP)
CALL OK(ALL(ABS(ans - [6.0_DFP, 4.0_DFP, 2.0_DFP]) .LT. tol), &
        "reversed segment")

END PROGRAM main
