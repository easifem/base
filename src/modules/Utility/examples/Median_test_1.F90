!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Median of three entries of an integer vector

PROGRAM main
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE MedianUtility, ONLY: Median
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B) :: perms(3, 6), a(3), b(5), ii
LOGICAL(LGT) :: isok

! the median ends up in the middle position
a = [3_I4B, 1_I4B, 2_I4B]
CALL Median(a, 1_I4B, 2_I4B, 3_I4B)
CALL OK(ALL(a .EQ. [1_I4B, 2_I4B, 3_I4B]), "median of [3, 1, 2]")
CALL OK(a(2) .EQ. 2_I4B, "median is in this(mid)")

! all six orderings of three distinct values are sorted
perms = RESHAPE([1, 2, 3, 1, 3, 2, 2, 1, 3, 2, 3, 1, 3, 1, 2, 3, 2, 1], &
                [3, 6])
isok = .TRUE.
DO ii = 1, 6
  a = perms(:, ii)
  CALL Median(a, 1_I4B, 2_I4B, 3_I4B)
  isok = isok .AND. ALL(a .EQ. [1_I4B, 2_I4B, 3_I4B])
END DO
CALL OK(isok, "all six orderings")

! equal values
a = [2_I4B, 1_I4B, 2_I4B]
CALL Median(a, 1_I4B, 2_I4B, 3_I4B)
CALL OK(ALL(a .EQ. [1_I4B, 2_I4B, 2_I4B]), "two equal values")

a = [5_I4B, 5_I4B, 5_I4B]
CALL Median(a, 1_I4B, 2_I4B, 3_I4B)
CALL OK(ALL(a .EQ. 5_I4B), "three equal values")

! non-adjacent positions, other entries are not changed
b = [30_I4B, 99_I4B, 10_I4B, 98_I4B, 20_I4B]
CALL Median(b, 1_I4B, 3_I4B, 5_I4B)
CALL OK(ALL(b .EQ. [10_I4B, 99_I4B, 20_I4B, 98_I4B, 30_I4B]), &
        "non-adjacent positions")

! the order follows the arguments, not the positions
b = [30_I4B, 99_I4B, 10_I4B, 98_I4B, 20_I4B]
CALL Median(b, 5_I4B, 3_I4B, 1_I4B)
CALL OK(ALL(b .EQ. [30_I4B, 99_I4B, 20_I4B, 98_I4B, 10_I4B]), &
        "left > mid > right")

END PROGRAM main
