!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: ArgMedian of three entries of an integer vector

PROGRAM main
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE MedianUtility, ONLY: ArgMedian
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B) :: perms(3, 6), a(3), a0(3), b(5), indx(3), jndx(5), ii
LOGICAL(LGT) :: isok

! indx is permuted, the values are not changed
a = [30_I4B, 10_I4B, 20_I4B]
a0 = a
indx = [1_I4B, 2_I4B, 3_I4B]
CALL ArgMedian(a, indx, 1_I4B, 2_I4B, 3_I4B)
CALL OK(ALL(indx .EQ. [2_I4B, 3_I4B, 1_I4B]), "indx of [30, 10, 20]")
CALL OK(a(indx(2)) .EQ. 20_I4B, "median is this(indx(mid))")
CALL OK(ALL(a .EQ. a0), "this is not changed")

! all six orderings of three distinct values
perms = RESHAPE([1, 2, 3, 1, 3, 2, 2, 1, 3, 2, 3, 1, 3, 1, 2, 3, 2, 1], &
                [3, 6])
isok = .TRUE.
DO ii = 1, 6
  a = perms(:, ii)
  indx = [1_I4B, 2_I4B, 3_I4B]
  CALL ArgMedian(a, indx, 1_I4B, 2_I4B, 3_I4B)
  isok = isok .AND. ALL(a(indx) .EQ. [1_I4B, 2_I4B, 3_I4B])
  isok = isok .AND. ALL(a .EQ. perms(:, ii))
END DO
CALL OK(isok, "all six orderings")

! equal values
a = [2_I4B, 1_I4B, 2_I4B]
indx = [1_I4B, 2_I4B, 3_I4B]
CALL ArgMedian(a, indx, 1_I4B, 2_I4B, 3_I4B)
CALL OK(ALL(indx .EQ. [2_I4B, 1_I4B, 3_I4B]), "two equal values")

! non-adjacent positions, other entries of indx are not changed
b = [50_I4B, 40_I4B, 30_I4B, 20_I4B, 10_I4B]
jndx = [3_I4B, 1_I4B, 4_I4B, 2_I4B, 5_I4B]
CALL ArgMedian(b, jndx, 1_I4B, 3_I4B, 5_I4B)
CALL OK(ALL(jndx .EQ. [5_I4B, 1_I4B, 4_I4B, 2_I4B, 3_I4B]), &
        "non-adjacent positions")

END PROGRAM main
