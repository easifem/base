!> author: Vikas Sharma, Ph. D.
! date: 2026-10-19
! summary: LAST of an empty vector or string
!
! Description:
!   Regression test for bug M-37 in bug-reports/Utility_bug_report.md.
!   An empty vector read x(0) and an empty string read x(0:0). Now
!   the result is 0 or a blank.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE TailUtility, ONLY: LAST
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B) :: iz(0)
REAL(DFP) :: rz(0)

CALL OK(LAST(iz) .EQ. 0_I4B, "integer: empty vector gives 0")
CALL OK(LAST(rz) .EQ. 0.0_DFP, "real: empty vector gives 0")
CALL OK(LAST("") .EQ. " ", "character: empty string gives a blank")

END PROGRAM main
