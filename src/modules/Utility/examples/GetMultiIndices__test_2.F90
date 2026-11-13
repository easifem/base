!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: GetMultiIndices_(n, d, upto, ans, nrow, ncol), no allocation
!
! Description:
!   Includes the regression test for bug H-09 in
!   bug-reports/Utility_bug_report.md: the value of upto was not
!   used, so upto = .FALSE. gave the result for .TRUE.

PROGRAM main
USE GlobalData, ONLY: I4B
USE IntegerUtility, ONLY: GetMultiIndices
USE IntegerUtility, ONLY: GetMultiIndices_
USE IntegerUtility, ONLY: SIZE
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B) :: ans(3, 10)
INTEGER(I4B) :: nrow, ncol

CALL GetMultiIndices_(2_I4B, 2_I4B, .TRUE., ans, nrow, ncol)
CALL OK(nrow .EQ. 3_I4B, "nrow is d+1")
CALL OK(ncol .EQ. SIZE(2_I4B, 2_I4B, .TRUE.), &
        "ncol is Size(n, d, .TRUE.)")
CALL OK(ALL(ans .EQ. GetMultiIndices(2_I4B, 2_I4B, .TRUE.)), &
        "same as GetMultiIndices")

! the first column is the multi-index of order zero
CALL OK(ALL(ans(:, 1) .EQ. 0_I4B), "first column is zero")

! upto = .FALSE. gives only the multi-indices of order n
ans = -1_I4B
CALL GetMultiIndices_(2_I4B, 2_I4B, .FALSE., ans, nrow, ncol)
CALL OK(nrow .EQ. 3_I4B .AND. ncol .EQ. 6_I4B, &
        "upto = .FALSE.: nrow = 3, ncol = 6")
CALL OK(ALL(ans(:, 1:6) .EQ. GetMultiIndices(2_I4B, 2_I4B)), &
        "upto = .FALSE.: same as GetMultiIndices(n, d)")
CALL OK(ALL(ans(:, 7:10) .EQ. -1_I4B), "upto = .FALSE.: rest unchanged")

END PROGRAM main
