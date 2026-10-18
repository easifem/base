!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Loc_NearestPoint returns the column nearest to a point

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE MiscUtility, ONLY: Loc_NearestPoint
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP) :: xij(2, 4), x3(3, 3)

! corners of a square, in xij format (one point per column)
xij(:, 1) = [0.0_DFP, 0.0_DFP]
xij(:, 2) = [10.0_DFP, 0.0_DFP]
xij(:, 3) = [0.0_DFP, 10.0_DFP]
xij(:, 4) = [10.0_DFP, 10.0_DFP]

CALL OK(Loc_NearestPoint(xij, [9.0_DFP, 8.0_DFP]) .EQ. 4_I4B, &
        "nearest to (9,8) is column 4")
CALL OK(Loc_NearestPoint(xij, [1.0_DFP, 2.0_DFP]) .EQ. 1_I4B, &
        "nearest to (1,2) is column 1")
CALL OK(Loc_NearestPoint(xij, [10.0_DFP, 0.0_DFP]) .EQ. 2_I4B, &
        "a point of the array returns its own column")

! a tie returns the first column
CALL OK(Loc_NearestPoint(xij, [5.0_DFP, 0.0_DFP]) .EQ. 1_I4B, &
        "tie returns the first column")

! three dimensional points
x3(:, 1) = [0.0_DFP, 0.0_DFP, 0.0_DFP]
x3(:, 2) = [0.0_DFP, 0.0_DFP, 20.0_DFP]
x3(:, 3) = [20.0_DFP, 0.0_DFP, 0.0_DFP]
CALL OK(Loc_NearestPoint(x3, [1.0_DFP, 2.0_DFP, 18.0_DFP]) .EQ. 2_I4B, &
        "3D: nearest is column 2")

END PROGRAM main
