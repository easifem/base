!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: IMINLOC returns the index of the smallest entry

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE MiscUtility, ONLY: IMINLOC
USE Test_Method, ONLY: OK
IMPLICIT NONE

CALL OK(IMINLOC([3.0_DFP, -1.0_DFP, 5.0_DFP]) .EQ. 2_I4B, "real")
CALL OK(IMINLOC([3.0_DFP, -1.0_DFP, 5.0_DFP, -1.0_DFP]) .EQ. 2_I4B, &
        "first of equal minima")
CALL OK(IMINLOC([0.25_DFP]) .EQ. 1_I4B, "size 1")

END PROGRAM main
