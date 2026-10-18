!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Partition when every entry is <= pivot and right = SIZE
!
! Description:
!   Regression test for bug H-04 in bug-reports/Utility_bug_report.md.
!   The scan `DO WHILE (lo <= hi .AND. this(lo) <= pivot)` relies on
!   short-circuit evaluation, which Fortran does not guarantee. When
!   every entry of this(left:right) is <= pivot, lo reaches right + 1
!   and this(right + 1) may be read. With right = SIZE(this) and
!   bounds checking enabled this stops the program.
!
!   All arrays below have exactly the size of the partitioned range.

PROGRAM main
USE GlobalData, ONLY: I4B
USE PartitionUtility, ONLY: Partition
USE SortUtility, ONLY: IntroSort
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B) :: a(5), c(5), s(30), ip, ii

! pivot a(1) = 9 is the largest entry
a = [9_I4B, 3_I4B, 5_I4B, 1_I4B, 7_I4B]
CALL Partition(a, 1_I4B, 5_I4B, ip)
CALL OK(ip .EQ. 5_I4B, "largest pivot: pivot index")
CALL OK(ALL(a .EQ. [7_I4B, 3_I4B, 5_I4B, 1_I4B, 9_I4B]), &
        "largest pivot: partitioned vector")

! constant vector
c = 4_I4B
CALL Partition(c, 1_I4B, 5_I4B, ip)
CALL OK(ip .EQ. 5_I4B, "constant: pivot index")
CALL OK(ALL(c .EQ. 4_I4B), "constant: vector is unchanged")

! IntroSort calls Partition on the whole vector
s = 2_I4B
s(1:30:3) = 1_I4B
CALL IntroSort(s)
CALL OK(ALL(s(1:29) .LE. s(2:30)), "IntroSort: few distinct values")
CALL OK(COUNT(s .EQ. 1_I4B) .EQ. 10_I4B, "IntroSort: values are kept")

s = 3_I4B
CALL IntroSort(s)
CALL OK(ALL(s .EQ. 3_I4B), "IntroSort: constant vector")

s = [(31_I4B - ii, ii=1, 30)]
CALL IntroSort(s)
CALL OK(ALL(s .EQ. [(ii, ii=1, 30)]), "IntroSort: reversed vector")

END PROGRAM main
