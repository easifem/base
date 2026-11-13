!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: ArgIntroSort on input that reaches the heap sort fallback
!
! Description:
!   Regression test for bug H-01 in
!   bug-reports/Utility_bug_report.md. The heap sort fallback, used
!   when the recursion depth is used up, overwrote arg(left:right)
!   with 1..N.
!
!   - Sorted input: before the fix the pivot was the smallest of
!     three samples, so sorted input reached the fallback.
!   - Organ-pipe input (rising, then falling): reaches the fallback
!     even with the median pivot.
!
!   Inputs with distinct values or few repeats are used on purpose:
!   they avoid the out-of-bounds read of bug H-04.

PROGRAM main
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: DFP
USE SortUtility, ONLY: ArgIntroSort
USE SortUtility, ONLY: ArgSort
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B), PARAMETER :: n = 50, m = 40, k = 100
INTEGER(I4B) :: a(n), arg(n)
INTEGER(I4B) :: p(2 * k), argp(2 * k)
REAL(DFP) :: r(m)
INTEGER(I4B) :: argr(m)
INTEGER(I4B) :: ii

! integer vector, already sorted
a = [(ii, ii=1, n)]
arg = [(ii, ii=1, n)]
CALL ArgIntroSort(a, arg)
CALL OK(ALL([(COUNT(arg .EQ. ii) .EQ. 1, ii=1, n)]), &
        "sorted I4B: arg is a permutation")
CALL OK(ALL(a(arg(1:n - 1)) .LE. a(arg(2:n))), &
        "sorted I4B: a(arg) is sorted")
CALL OK(ALL(arg .EQ. [(ii, ii=1, n)]), "sorted I4B: arg is 1..n")

! real vector, already sorted
r = [(0.5_DFP * REAL(ii, DFP), ii=1, m)]
argr = [(ii, ii=1, m)]
CALL ArgIntroSort(r, argr)
CALL OK(ALL([(COUNT(argr .EQ. ii) .EQ. 1, ii=1, m)]), &
        "sorted DFP: arg is a permutation")
CALL OK(ALL(r(argr(1:m - 1)) .LE. r(argr(2:m))), &
        "sorted DFP: r(arg) is sorted")

! organ-pipe vector [1, ..., k, k, ..., 1]
p = [[(ii, ii=1, k)], [(ii, ii=k, 1, -1)]]
argp = [(ii, ii=1, 2 * k)]
CALL ArgIntroSort(p, argp)
CALL OK(ALL([(COUNT(argp .EQ. ii) .EQ. 1, ii=1, 2 * k)]), &
        "organ-pipe: arg is a permutation")
CALL OK(ALL(p(argp(1:2 * k - 1)) .LE. p(argp(2:2 * k))), &
        "organ-pipe: p(arg) is sorted")

! ArgSort uses ArgIntroSort by default
arg = ArgSort(a)
CALL OK(ALL([(COUNT(arg .EQ. ii) .EQ. 1, ii=1, n)]), &
        "ArgSort default: arg is a permutation")
CALL OK(ALL(arg .EQ. [(ii, ii=1, n)]), "ArgSort default: arg is 1..n")

END PROGRAM main
