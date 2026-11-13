!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Median of three entries of a real vector

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE MedianUtility, ONLY: Median
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP) :: perms(3, 6), a(3), b(5), sorted(3)
INTEGER(I4B) :: ii
LOGICAL(LGT) :: isok

sorted = [-1.5_DFP, 0.5_DFP, 2.5_DFP]

! the median ends up in the middle position
a = [2.5_DFP, -1.5_DFP, 0.5_DFP]
CALL Median(a, 1_I4B, 2_I4B, 3_I4B)
CALL OK(ALL(ABS(a - sorted) .LT. tol), "median of [2.5, -1.5, 0.5]")
CALL OK(ABS(a(2) - 0.5_DFP) .LT. tol, "median is in this(mid)")

! all six orderings of three distinct values are sorted
perms(:, 1) = [sorted(1), sorted(2), sorted(3)]
perms(:, 2) = [sorted(1), sorted(3), sorted(2)]
perms(:, 3) = [sorted(2), sorted(1), sorted(3)]
perms(:, 4) = [sorted(2), sorted(3), sorted(1)]
perms(:, 5) = [sorted(3), sorted(1), sorted(2)]
perms(:, 6) = [sorted(3), sorted(2), sorted(1)]
isok = .TRUE.
DO ii = 1, 6
  a = perms(:, ii)
  CALL Median(a, 1_I4B, 2_I4B, 3_I4B)
  isok = isok .AND. ALL(ABS(a - sorted) .LT. tol)
END DO
CALL OK(isok, "all six orderings")

! equal values
a = [2.0_DFP, 1.0_DFP, 2.0_DFP]
CALL Median(a, 1_I4B, 2_I4B, 3_I4B)
CALL OK(ALL(ABS(a - [1.0_DFP, 2.0_DFP, 2.0_DFP]) .LT. tol), &
        "two equal values")

! non-adjacent positions, other entries are not changed
b = [3.0_DFP, 9.9_DFP, 1.0_DFP, 9.8_DFP, 2.0_DFP]
CALL Median(b, 1_I4B, 3_I4B, 5_I4B)
CALL OK(ALL(ABS(b - [1.0_DFP, 9.9_DFP, 2.0_DFP, 9.8_DFP, 3.0_DFP]) &
            .LT. tol), "non-adjacent positions")

END PROGRAM main
