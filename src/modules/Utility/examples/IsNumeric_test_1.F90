PROGRAM main
USE GlobalData, ONLY: DFP, I4B, LGT
USE ApproxUtility, ONLY: CharToNumber
USE ApproxUtility, ONLY: OPERATOR(.APPROXEQ.)
USE Display_Method, ONLY: Display
USE Test_Method, ONLY: OK
IMPLICIT NONE
INTEGER(I4B) :: aint
REAL(DFP) :: areal

CALL CharToNumber(aint, "12")
CALL OK(aint .EQ. 12_I4B, "test 1: ")

CALL CharToNumber(areal, "12.23")
CALL OK(areal.APPROXEQ.12.23_DFP, "test 2: ")
END PROGRAM main

! ok 1 - test 1: 
! ok 2 - test 2: 
