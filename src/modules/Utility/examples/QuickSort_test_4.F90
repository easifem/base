!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: QuickSort for 2 vectors (integer, real)

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE SortUtility, ONLY: QuickSort
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B) :: v1(6)
REAL(DFP) :: v2(6)
INTEGER(I4B) :: ii

! vect1 is the key, the other vectors follow it
v1 = [4_I4B, 2_I4B, 5_I4B, &
      2_I4B, 1_I4B, 3_I4B]
v2 = [(1.0_DFP * ii, ii=1, 6)]

CALL QuickSort(v1, v2, 1_I4B, 6_I4B)

! vect1 is sorted in ascending order
CALL OK(ALL(v1 .EQ. [1_I4B, 2_I4B, 2_I4B, &
                     3_I4B, 4_I4B, 5_I4B]), "vect1 is sorted")

! v2 holds the original row number of each entry of v1
CALL OK(ALL(v1 .EQ. KEY0(NINT(v2))), "rows stay together")
CALL OK(ALL([(COUNT(NINT(v2) .EQ. ii) .EQ. 1, ii=1, 6)]), &
        "vect2 is a permutation")

CONTAINS

PURE FUNCTION KEY0(i) RESULT(ans)
  INTEGER(I4B), INTENT(IN) :: i(:)
  INTEGER(I4B) :: ans(SIZE(i))
  INTEGER(I4B) :: k(6)
  k = [4_I4B, 2_I4B, 5_I4B, &
       2_I4B, 1_I4B, 3_I4B]
  ans = k(i)
END FUNCTION KEY0

END PROGRAM main
