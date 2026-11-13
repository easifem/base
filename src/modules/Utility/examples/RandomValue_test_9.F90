!> author: Vikas Sharma, Ph. D.
! date: 2026-10-19
! summary: RandomValue of an empty vector or matrix returns zero
!
! Description:
!   Regression test for bug M-06 in bug-reports/Utility_bug_report.md:
!   for an empty val the selected index was 0, out of bounds.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE RandomUtility, ONLY: RandomValue
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B) :: iv(0), im(0, 3)
REAL(DFP) :: rv(0), rm(2, 0)

CALL OK(RandomValue(iv) .EQ. 0_I4B, "empty integer vector")
CALL OK(RandomValue(im) .EQ. 0_I4B, "empty integer matrix")
CALL OK(RandomValue(rv) .EQ. 0.0_DFP, "empty real vector")
CALL OK(RandomValue(rm) .EQ. 0.0_DFP, "empty real matrix")

END PROGRAM main
