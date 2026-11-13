!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: ArgMedian of three entries of a real vector

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE MedianUtility, ONLY: ArgMedian
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP) :: perms(3, 6), a(3), a0(3), b(5), sorted(3)
INTEGER(I4B) :: indx(3), jndx(5), ii
LOGICAL(LGT) :: isok

sorted = [-1.5_DFP, 0.5_DFP, 2.5_DFP]

! indx is permuted, the values are not changed
a = [2.5_DFP, -1.5_DFP, 0.5_DFP]
a0 = a
indx = [1_I4B, 2_I4B, 3_I4B]
CALL ArgMedian(a, indx, 1_I4B, 2_I4B, 3_I4B)
CALL OK(ALL(indx .EQ. [2_I4B, 3_I4B, 1_I4B]), "indx of [2.5, -1.5, 0.5]")
CALL OK(ABS(a(indx(2)) - 0.5_DFP) .LT. tol, "median is this(indx(mid))")
CALL OK(ALL(ABS(a - a0) .LT. tol), "this is not changed")

! all six orderings of three distinct values
perms(:, 1) = [sorted(1), sorted(2), sorted(3)]
perms(:, 2) = [sorted(1), sorted(3), sorted(2)]
perms(:, 3) = [sorted(2), sorted(1), sorted(3)]
perms(:, 4) = [sorted(2), sorted(3), sorted(1)]
perms(:, 5) = [sorted(3), sorted(1), sorted(2)]
perms(:, 6) = [sorted(3), sorted(2), sorted(1)]
isok = .TRUE.
DO ii = 1, 6
  a = perms(:, ii)
  indx = [1_I4B, 2_I4B, 3_I4B]
  CALL ArgMedian(a, indx, 1_I4B, 2_I4B, 3_I4B)
  isok = isok .AND. ALL(ABS(a(indx) - sorted) .LT. tol)
END DO
CALL OK(isok, "all six orderings")

! non-adjacent positions, other entries of indx are not changed
b = [5.0_DFP, 4.0_DFP, 3.0_DFP, 2.0_DFP, 1.0_DFP]
jndx = [3_I4B, 1_I4B, 4_I4B, 2_I4B, 5_I4B]
CALL ArgMedian(b, jndx, 1_I4B, 3_I4B, 5_I4B)
CALL OK(ALL(jndx .EQ. [5_I4B, 1_I4B, 4_I4B, 2_I4B, 3_I4B]), &
        "non-adjacent positions")

END PROGRAM main
