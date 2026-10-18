!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: GetMultiIndices(n, d, upto), multi-indices of order <= n
!
! Description:
!   Includes the regression test for bug H-09 in
!   bug-reports/Utility_bug_report.md: the value of upto was not
!   used, so upto = .FALSE. gave the result for .TRUE.

PROGRAM main
USE GlobalData, ONLY: I4B
USE IntegerUtility, ONLY: GetMultiIndices
USE IntegerUtility, ONLY: SIZE
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B), ALLOCATABLE :: ans(:, :)
INTEGER(I4B) :: want(2, 3)

! d = 1, n = 1: order 0 first, then order 1
want = RESHAPE([0_I4B, 0_I4B, 0_I4B, 1_I4B, 1_I4B, 0_I4B], &
               [2_I4B, 3_I4B])
ans = GetMultiIndices(1_I4B, 1_I4B, .TRUE.)
CALL OK(ALL(SHAPE(ans) .EQ. [2_I4B, 3_I4B]), "shape for d = 1")
CALL OK(ALL(ans .EQ. want), "indices for d = 1")

! d = 2, n = 2: columns are sorted by order
ans = GetMultiIndices(2_I4B, 2_I4B, upto=.TRUE.)
CALL OK(ALL(SHAPE(ans) .EQ. [3_I4B, 10_I4B]), "shape for d = 2")
CALL OK(SIZE(ans, 2) .EQ. SIZE(2_I4B, 2_I4B, .TRUE.), &
        "number of columns")
CALL OK(ALL(SUM(ans, 1) .EQ. [0_I4B, 1_I4B, 1_I4B, 1_I4B, 2_I4B, &
                              2_I4B, 2_I4B, 2_I4B, 2_I4B, 2_I4B]), &
        "order of each column")

! the last columns are the multi-indices of order n
CALL OK(ALL(ans(:, 5:10) .EQ. GetMultiIndices(2_I4B, 2_I4B)), &
        "last columns have order n")

! upto = .FALSE. gives only the multi-indices of order n
ans = GetMultiIndices(2_I4B, 2_I4B, upto=.FALSE.)
CALL OK(ALL(SHAPE(ans) .EQ. [3_I4B, 6_I4B]), "upto = .FALSE.: shape")
CALL OK(ALL(ans .EQ. GetMultiIndices(2_I4B, 2_I4B)), &
        "upto = .FALSE.: same as GetMultiIndices(n, d)")

END PROGRAM main
