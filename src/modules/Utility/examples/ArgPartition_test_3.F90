!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: ArgPartition when every entry is <= pivot and right = SIZE
!
! Description:
!   Regression test for bug H-04 in bug-reports/Utility_bug_report.md.
!   The scan `DO WHILE (lo <= hi .AND. this(idx(lo)) <= pivot)` relies
!   on short-circuit evaluation, which Fortran does not guarantee.
!   When every entry is <= pivot, lo reaches right + 1 and
!   idx(right + 1) may be read. With right = SIZE(idx) and bounds
!   checking enabled this stops the program.
!
!   All arrays below have exactly the size of the partitioned range.

PROGRAM main
USE GlobalData, ONLY: I4B
USE PartitionUtility, ONLY: ArgPartition
USE SortUtility, ONLY: ArgIntroSort
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B) :: a(5), c(5), idx(5), s(30), arg(30), ip, ii

! pivot a(idx(1)) = 9 is the largest entry
a = [9_I4B, 3_I4B, 5_I4B, 1_I4B, 7_I4B]
idx = [(ii, ii=1, 5)]
CALL ArgPartition(a, idx, 1_I4B, 5_I4B, ip)
CALL OK(ip .EQ. 5_I4B, "largest pivot: pivot index")
CALL OK(ALL(idx .EQ. [5_I4B, 2_I4B, 3_I4B, 4_I4B, 1_I4B]), &
        "largest pivot: idx")
CALL OK(ALL(a .EQ. [9_I4B, 3_I4B, 5_I4B, 1_I4B, 7_I4B]), &
        "largest pivot: array is unchanged")

! constant vector
c = 4_I4B
idx = [(ii, ii=1, 5)]
CALL ArgPartition(c, idx, 1_I4B, 5_I4B, ip)
CALL OK(ip .EQ. 5_I4B, "constant: pivot index")
CALL OK(ALL([(COUNT(idx .EQ. ii) .EQ. 1, ii=1, 5)]), &
        "constant: idx is a permutation")

! ArgIntroSort calls ArgPartition on the whole vector
s = 2_I4B
s(1:30:3) = 1_I4B
arg = [(ii, ii=1, 30)]
CALL ArgIntroSort(s, arg)
CALL OK(ALL(s(arg(1:29)) .LE. s(arg(2:30))), &
        "ArgIntroSort: few distinct values")
CALL OK(ALL([(COUNT(arg .EQ. ii) .EQ. 1, ii=1, 30)]), &
        "ArgIntroSort: arg is a permutation")

s = 3_I4B
arg = [(ii, ii=1, 30)]
CALL ArgIntroSort(s, arg)
CALL OK(ALL([(COUNT(arg .EQ. ii) .EQ. 1, ii=1, 30)]), &
        "ArgIntroSort: constant vector")

END PROGRAM main
