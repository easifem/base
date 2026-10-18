!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: IMAXLOC returns the index of the largest entry

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE MiscUtility, ONLY: IMAXLOC
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B) :: empty(0)

! integer vector
CALL OK(IMAXLOC([3_I4B, 9_I4B, 1_I4B]) .EQ. 2_I4B, "integer")
CALL OK(IMAXLOC([3_I4B, 7_I4B, 7_I4B, 1_I4B]) .EQ. 2_I4B, &
        "first of equal maxima")
CALL OK(IMAXLOC([5_I4B]) .EQ. 1_I4B, "size 1")
CALL OK(IMAXLOC(empty) .EQ. 0_I4B, "empty vector gives 0")

! real vector
CALL OK(IMAXLOC([-1.0_DFP, -0.5_DFP, -2.0_DFP]) .EQ. 2_I4B, "real")

END PROGRAM main
