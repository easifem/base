!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: ArgPartition for real vectors

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE PartitionUtility, ONLY: ArgPartition
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP) :: t(5), t2(4)
INTEGER(I4B) :: idx(5), idx2(4), i, k

! pivot is t(idx(1)) = 0.5
t = [0.5_DFP, 0.25_DFP, 0.75_DFP, 0.125_DFP, 1.0_DFP]
idx = [(k, k=1, 5)]
i = 0_I4B
CALL ArgPartition(t, idx, 1_I4B, 5_I4B, i)
CALL OK(i .EQ. 3_I4B, "pivot position")
CALL OK(idx(i) .EQ. 1_I4B, "pivot index")
CALL OK(ALL(t(idx(1:i - 1)) .LE. t(idx(i))), "left part <= pivot")
CALL OK(ALL(t(idx(i + 1:5)) .GT. t(idx(i))), "right part > pivot")
CALL OK(ALL(idx .EQ. [4_I4B, 2_I4B, 1_I4B, 3_I4B, 5_I4B]), &
        "permuted indices")
CALL OK(ALL([(ANY(idx .EQ. k), k=1, 5)]), "idx is a permutation")

! equal values in t(idx(1:3)), idx2(4) is untouched
t2 = [2.0_DFP, 2.0_DFP, 2.0_DFP, 9.0_DFP]
idx2 = [(k, k=1, 4)]
CALL ArgPartition(t2, idx2, 1_I4B, 3_I4B, i)
CALL OK(i .EQ. 3_I4B, "equal values")
CALL OK(ALL(idx2 .EQ. [3_I4B, 2_I4B, 1_I4B, 4_I4B]), &
        "permuted idx2(1:3)")

END PROGRAM main
