!> author: Vikas Sharma, Ph. D.
! date: 2026-10-19
! summary: QuickSort on large sorted, reverse sorted and constant input
!
! Description:
!   Regression test for bug M-09 in bug-reports/Utility_bug_report.md.
!   QuickSort used the last entry as pivot, so sorted, reverse sorted
!   and constant vectors took O(n**2) time and n levels of recursion;
!   for n = 100000 that is slow and may overflow the stack. QuickSort
!   now uses IntroSort (one vector) or ArgIntroSort (several vectors),
!   which is O(n log n) with O(log n) depth for every input.

PROGRAM main
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: INT64
USE SortUtility, ONLY: QuickSort
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B), PARAMETER :: n = 100000
INTEGER(I4B) :: a(n), key(n), row(n), ii
REAL(DFP) :: r(n)

! one vector, already sorted
a = [(ii, ii=1, n)]
CALL QuickSort(a, 1_I4B, n)
CALL OK(ALL(a .EQ. [(ii, ii=1, n)]), "sorted input")

! one vector, reverse sorted
a = [(n + 1 - ii, ii=1, n)]
CALL QuickSort(a, 1_I4B, n)
CALL OK(ALL(a .EQ. [(ii, ii=1, n)]), "reverse sorted input")

! one vector, constant
a = 7_I4B
CALL QuickSort(a, 1_I4B, n)
CALL OK(ALL(a .EQ. 7_I4B), "constant input")

! only a(low:high) is sorted
a = [(n + 1 - ii, ii=1, n)]
CALL QuickSort(a, 11_I4B, n - 10_I4B)
CALL OK(ALL(a(1:10) .EQ. [(n + 1 - ii, ii=1, 10)]), &
        "entries before low are not changed")
CALL OK(ALL(a(n - 9:n) .EQ. [(n + 1 - ii, ii=n - 9, n)]), &
        "entries after high are not changed")
CALL OK(ALL(a(11:n - 11) .LE. a(12:n - 10)), "a(low:high) is sorted")

! two vectors, reverse sorted key: the rows stay together
key = [(n + 1 - ii, ii=1, n)]
row = [(ii, ii=1, n)]
CALL QuickSort(key, row, 1_I4B, n)
CALL OK(ALL(key .EQ. [(ii, ii=1, n)]), "two vectors: key is sorted")
CALL OK(ALL(row .EQ. [(n + 1 - ii, ii=1, n)]), &
        "two vectors: rows stay together")

! two vectors, constant key: the rows are a permutation
key = 3_I4B
row = [(ii, ii=1, n)]
CALL QuickSort(key, row, 1_I4B, n)
CALL OK(ALL(key .EQ. 3_I4B), "two vectors, constant key")
CALL OK(SUM(INT(row, INT64)) .EQ. INT(n, INT64) * (n + 1) / 2, &
        "two vectors, constant key: rows are kept")

! integer key with a real second vector, already sorted
key = [(ii, ii=1, n)]
r = [(REAL(ii, DFP), ii=1, n)]
CALL QuickSort(key, r, 1_I4B, n)
CALL OK(ALL(NINT(r) .EQ. key), "integer key with real vector")

END PROGRAM main
