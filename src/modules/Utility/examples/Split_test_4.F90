!> author: Vikas Sharma, Ph. D.
! date: 2026-10-19
! summary: SPLIT of a vector with section other than 1 or 2
!
! Description:
!   Regression test for bug M-36 in bug-reports/Utility_bug_report.md.
!   The result was not allocated; now it is an empty vector, like the
!   empty string of the character version.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE SplitUtility, ONLY: SPLIT
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B), ALLOCATABLE :: b(:)
REAL(DFP), ALLOCATABLE :: r(:)

b = SPLIT([1_I4B, 2_I4B, 3_I4B], 3_I4B)
CALL OK(ALLOCATED(b), "integer: result is allocated")
CALL OK(SIZE(b) .EQ. 0_I4B, "integer: section 3 is empty")

r = SPLIT([1.0_DFP, 2.0_DFP], 0_I4B)
CALL OK(ALLOCATED(r), "real: result is allocated")
CALL OK(SIZE(r) .EQ. 0_I4B, "real: section 0 is empty")

END PROGRAM main
