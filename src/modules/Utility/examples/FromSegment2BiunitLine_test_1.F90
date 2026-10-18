!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: FromSegment2BiunitLine maps [x1, x2] to [-1, 1]

PROGRAM main
USE GlobalData, ONLY: DFP
USE MappingUtility, ONLY: FromSegment2BiunitLine
USE MappingUtility, ONLY: FromBiunitLine2Segment
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP) :: xin(3), ans(3)

! end points and mid point of [2, 6]
xin = [2.0_DFP, 4.0_DFP, 6.0_DFP]
ans = FromSegment2BiunitLine(xin, 2.0_DFP, 6.0_DFP)
CALL OK(ALL(ABS(ans - [-1.0_DFP, 0.0_DFP, 1.0_DFP]) .LT. tol), &
        "end points and mid point")

! inverse of FromBiunitLine2Segment
xin = [2.5_DFP, 3.0_DFP, 5.5_DFP]
ans = FromBiunitLine2Segment( &
      FromSegment2BiunitLine(xin, 2.0_DFP, 6.0_DFP), 2.0_DFP, 6.0_DFP)
CALL OK(ALL(ABS(ans - xin) .LT. tol), "round trip")

END PROGRAM main
