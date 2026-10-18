!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: GetMultiIndices(n, d), multi-indices of order n

PROGRAM main
USE GlobalData, ONLY: I4B
USE IntegerUtility, ONLY: GetMultiIndices
USE IntegerUtility, ONLY: SIZE
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B), ALLOCATABLE :: ans(:, :)
INTEGER(I4B) :: want1(2, 3), want2(3, 6)

! d = 1, n = 2
want1 = RESHAPE([0_I4B, 2_I4B, 1_I4B, 1_I4B, 2_I4B, 0_I4B], &
                [2_I4B, 3_I4B])
ans = GetMultiIndices(2_I4B, 1_I4B)
CALL OK(ALL(SHAPE(ans) .EQ. [2_I4B, 3_I4B]), "shape for d = 1")
CALL OK(ALL(ans .EQ. want1), "indices for d = 1")

! d = 2, n = 2
want2 = RESHAPE([0_I4B, 0_I4B, 2_I4B, 0_I4B, 1_I4B, 1_I4B, &
                 0_I4B, 2_I4B, 0_I4B, 1_I4B, 0_I4B, 1_I4B, &
                 1_I4B, 1_I4B, 0_I4B, 2_I4B, 0_I4B, 0_I4B], &
                [3_I4B, 6_I4B])
ans = GetMultiIndices(n=2_I4B, d=2_I4B)
CALL OK(ALL(SHAPE(ans) .EQ. [3_I4B, 6_I4B]), "shape for d = 2")
CALL OK(ALL(ans .EQ. want2), "indices for d = 2")

! every column sums to n
ans = GetMultiIndices(4_I4B, 3_I4B)
CALL OK(SIZE(ans, 2) .EQ. SIZE(4_I4B, 3_I4B), "number of columns")
CALL OK(ALL(SUM(ans, 1) .EQ. 4_I4B), "each column sums to n")

END PROGRAM main
