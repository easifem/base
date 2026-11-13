!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: a .in. b for integer vectors (subset test)
!
! Description:
!   Includes the regression test for bug H-08 in
!   bug-reports/Utility_bug_report.md: the result was .FALSE.
!   whenever SIZE(a) > SIZE(b), even if every entry of a is in b.

PROGRAM main
USE GlobalData, ONLY: I4B
USE IntegerUtility, ONLY: OPERATOR(.in.)
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B) :: a(2), b(4)

a = [12_I4B, 9_I4B]
b = [9_I4B, 10_I4B, 11_I4B, 12_I4B]

! every entry of a is in b
CALL OK(a .in. b, "a is a subset of b")

! b has entries which are not in a
CALL OK(.NOT. (b .in. a), "b is not a subset of a")

! one entry is missing
CALL OK(.NOT. ([9_I4B, 13_I4B] .in. b), "one entry is missing")

! the order does not matter
CALL OK([12_I4B, 10_I4B, 9_I4B] .in. b, "order does not matter")

! a set is a subset of itself
CALL OK(b .in. b, "b is a subset of b")

! a may have more entries than b if some entries are repeated
CALL OK([9_I4B, 9_I4B, 12_I4B, 9_I4B, 12_I4B] .in. [9_I4B, 12_I4B], &
        "repeated entries, SIZE(a) > SIZE(b)")
CALL OK(.NOT. ([9_I4B, 9_I4B, 13_I4B] .in. [9_I4B, 12_I4B]), &
        "repeated entries with one missing")

END PROGRAM main
