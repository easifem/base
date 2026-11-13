!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: ArgPartition for integer vectors

PROGRAM main
USE GlobalData, ONLY: I4B
USE PartitionUtility, ONLY: ArgPartition
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B) :: t(7), idx(7), t2(6), idx2(6), i, k

! pivot is t(idx(1)) = 5
t = [5_I4B, 2_I4B, 8_I4B, 1_I4B, 9_I4B, 3_I4B, 7_I4B]
idx = [(k, k=1, 7)]
i = 0_I4B
CALL ArgPartition(t, idx, 1_I4B, 7_I4B, i)
CALL OK(i .EQ. 4_I4B, "pivot position")
CALL OK(t(idx(i)) .EQ. 5_I4B, "pivot value")
CALL OK(ALL(t(idx(1:i - 1)) .LE. t(idx(i))), "left part <= pivot")
CALL OK(ALL(t(idx(i + 1:7)) .GT. t(idx(i))), "right part > pivot")
CALL OK(ALL(idx .EQ. [4_I4B, 2_I4B, 6_I4B, 1_I4B, 5_I4B, 3_I4B, 7_I4B]), &
        "permuted indices")
CALL OK(ALL([(ANY(idx .EQ. k), k=1, 7)]), "idx is a permutation")
CALL OK(ALL(t .EQ. [5_I4B, 2_I4B, 8_I4B, 1_I4B, 9_I4B, 3_I4B, 7_I4B]), &
        "values are untouched")

! only idx2(2:5) is permuted
t2 = [10_I4B, 4_I4B, 6_I4B, 1_I4B, 3_I4B, 20_I4B]
idx2 = [(k, k=1, 6)]
CALL ArgPartition(t2, idx2, 2_I4B, 5_I4B, i)
CALL OK(i .EQ. 4_I4B, "pivot position in idx2(2:5)")
CALL OK(ALL(idx2 .EQ. [1_I4B, 4_I4B, 5_I4B, 2_I4B, 3_I4B, 6_I4B]), &
        "permuted idx2(2:5)")
CALL OK(idx2(1) .EQ. 1_I4B .AND. idx2(6) .EQ. 6_I4B, &
        "outside is untouched")

END PROGRAM main
