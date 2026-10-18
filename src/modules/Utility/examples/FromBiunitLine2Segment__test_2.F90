!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: FromBiunitLine2Segment_ for a segment in space, no allocation

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE MappingUtility, ONLY: FromBiunitLine2Segment
USE MappingUtility, ONLY: FromBiunitLine2Segment_
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP) :: xin(3), x1(2), x2(2), ans(3, 4)
INTEGER(I4B) :: nrow, ncol

xin = [-1.0_DFP, 0.0_DFP, 1.0_DFP]
x1 = [0.0_DFP, 0.0_DFP]
x2 = [2.0_DFP, 4.0_DFP]

! ans can be larger, only ans(1:nrow, 1:ncol) is written
CALL FromBiunitLine2Segment_(xin, x1, x2, ans, nrow, ncol)
CALL OK(nrow .EQ. 2_I4B, "nrow is SIZE(x1)")
CALL OK(ncol .EQ. 3_I4B, "ncol is SIZE(xin)")
CALL OK(ALL(ABS(ans(1:2, 2) - [1.0_DFP, 2.0_DFP]) .LT. tol), &
        "mid point")
CALL OK(ALL(ABS(ans(1:nrow, 1:ncol) - &
                FromBiunitLine2Segment(xin, x1, x2)) .LT. tol), &
        "same as FromBiunitLine2Segment")

END PROGRAM main
