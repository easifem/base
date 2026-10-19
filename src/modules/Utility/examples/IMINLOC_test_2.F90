!> author: Vikas Sharma, Ph. D.
! date: 2026-10-19
! summary: IMINLOC of an integer vector

PROGRAM main
USE GlobalData, ONLY: I4B
USE MiscUtility, ONLY: IMINLOC
USE Test_Method, ONLY: OK
IMPLICIT NONE

CALL OK(IMINLOC([3_I4B, -1_I4B, 5_I4B]) .EQ. 2_I4B, "integer")
CALL OK(IMINLOC([3_I4B, -1_I4B, 5_I4B, -1_I4B]) .EQ. 2_I4B, &
        "first of equal minima")
CALL OK(IMINLOC([7_I4B]) .EQ. 1_I4B, "size 1")

END PROGRAM main
