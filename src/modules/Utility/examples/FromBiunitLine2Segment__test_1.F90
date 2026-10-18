!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: FromBiunitLine2Segment_ for [x1, x2], no allocation

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE MappingUtility, ONLY: FromBiunitLine2Segment
USE MappingUtility, ONLY: FromBiunitLine2Segment_
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP) :: xin(3), ans(5)
INTEGER(I4B) :: tsize

xin = [-1.0_DFP, 0.0_DFP, 1.0_DFP]

! ans can be larger than xin, only ans(1:tsize) is written
ans = -100.0_DFP
CALL FromBiunitLine2Segment_(xin, 2.0_DFP, 6.0_DFP, ans, tsize)
CALL OK(tsize .EQ. 3_I4B, "tsize is SIZE(xin)")
CALL OK(ALL(ABS(ans(1:tsize) - [2.0_DFP, 4.0_DFP, 6.0_DFP]) .LT. tol), &
        "mapped values")
CALL OK(ALL(ABS(ans(4:5) + 100.0_DFP) .LT. tol), &
        "extra entries are not changed")

! same as the function
CALL OK(ALL(ABS(ans(1:tsize) - &
                FromBiunitLine2Segment(xin, 2.0_DFP, 6.0_DFP)) .LT. tol), &
        "same as FromBiunitLine2Segment")

END PROGRAM main
