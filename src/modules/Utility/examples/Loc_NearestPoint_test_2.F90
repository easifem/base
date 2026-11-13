!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Loc_NearestPoint for points less than one unit apart
!
! Description:
!   Regression test for bug H-05 in bug-reports/Utility_bug_report.md.
!   The distances were stored in INTEGER(I4B) variables, so NORM2 was
!   truncated: 0.1, 0.3 and 0.8 all became 0, and 1.2 and 1.9 both
!   became 1. Equal truncated distances are treated as a tie and the
!   first column is returned. The routine also copied each point into
!   a work array xr(3), which is too small for more than three
!   coordinates; that case is checked last.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE MiscUtility, ONLY: Loc_NearestPoint
USE MiscUtility, ONLY: SearchNearestCoord
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP) :: a(1, 3), b(2, 2), q(2, 4), x4(4, 3)

! distances 0.8, 0.3 and 0.1: the nearest is column 3
a(1, :) = [0.0_DFP, 0.5_DFP, 0.9_DFP]
CALL OK(Loc_NearestPoint(a, [0.8_DFP]) .EQ. 3_I4B, &
        "distances below 1: nearest is column 3")

! distances 1.9 and 1.2: the nearest is column 2
b(:, 1) = [0.0_DFP, 0.0_DFP]
b(:, 2) = [3.1_DFP, 0.0_DFP]
CALL OK(Loc_NearestPoint(b, [1.9_DFP, 0.0_DFP]) .EQ. 2_I4B, &
        "distances 1.9 and 1.2: nearest is column 2")

! corners of the unit square
q(:, 1) = [0.0_DFP, 0.0_DFP]
q(:, 2) = [1.0_DFP, 0.0_DFP]
q(:, 3) = [1.0_DFP, 1.0_DFP]
q(:, 4) = [0.0_DFP, 1.0_DFP]
CALL OK(Loc_NearestPoint(q, [0.9_DFP, 0.8_DFP]) .EQ. 3_I4B, &
        "unit square: nearest to (0.9, 0.8) is column 3")
CALL OK(SearchNearestCoord(q, [0.1_DFP, 0.7_DFP]) .EQ. 4_I4B, &
        "unit square: nearest to (0.1, 0.7) is column 4")

! points with four coordinates
x4(:, 1) = [0.0_DFP, 0.0_DFP, 0.0_DFP, 0.0_DFP]
x4(:, 2) = [5.0_DFP, 5.0_DFP, 5.0_DFP, 5.0_DFP]
x4(:, 3) = [0.0_DFP, 0.0_DFP, 0.0_DFP, 9.0_DFP]
CALL OK(Loc_NearestPoint(x4, [0.0_DFP, 0.0_DFP, 0.0_DFP, 8.0_DFP]) &
        .EQ. 3_I4B, "4 coordinates: nearest is column 3")

END PROGRAM main
