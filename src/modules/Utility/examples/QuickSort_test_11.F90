!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: QuickSort for 4 vectors (real, real, real, real)

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE SortUtility, ONLY: QuickSort
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP) :: v1(6)
REAL(DFP) :: v2(6)
REAL(DFP) :: v3(6)
REAL(DFP) :: v4(6)
INTEGER(I4B) :: ii

! vect1 is the key, the other vectors follow it
v1 = [4.0_DFP, 2.0_DFP, 5.0_DFP, &
      2.0_DFP, 1.0_DFP, 3.0_DFP]
v2 = [(1.0_DFP * ii, ii=1, 6)]
v3 = [(10.0_DFP * ii, ii=1, 6)]
v4 = [(100.0_DFP * ii, ii=1, 6)]

CALL QuickSort(v1, v2, v3, v4, 1_I4B, 6_I4B)

! vect1 is sorted in ascending order
CALL OK(ALL(v1 .EQ. [1.0_DFP, 2.0_DFP, 2.0_DFP, &
                     3.0_DFP, 4.0_DFP, 5.0_DFP]), "vect1 is sorted")

! v2 holds the original row number of each entry of v1
CALL OK(ALL(v1 .EQ. KEY0(NINT(v2))), "rows stay together")
CALL OK(ALL([(COUNT(NINT(v2) .EQ. ii) .EQ. 1, ii=1, 6)]), &
        "vect2 is a permutation")
CALL OK(ALL(v3 .EQ. 10.0_DFP * v2), "vect3 follows vect1")
CALL OK(ALL(v4 .EQ. 100.0_DFP * v2), "vect4 follows vect1")

CONTAINS

PURE FUNCTION KEY0(i) RESULT(ans)
  INTEGER(I4B), INTENT(IN) :: i(:)
  REAL(DFP) :: ans(SIZE(i))
  REAL(DFP) :: k(6)
  k = [4.0_DFP, 2.0_DFP, 5.0_DFP, &
       2.0_DFP, 1.0_DFP, 3.0_DFP]
  ans = k(i)
END FUNCTION KEY0

END PROGRAM main
