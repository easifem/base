!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: ArgHeapSort for an integer vector

PROGRAM main
USE GlobalData, ONLY: I4B
USE SortUtility, ONLY: ArgHeapSort
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B) :: a(10), a0(10), arg(10)
INTEGER(I4B) :: b(40), argb(40), e(0), arge(0)
INTEGER(I4B) :: ii

a = [5_I4B, 3_I4B, 9_I4B, 1_I4B, 7_I4B, 2_I4B, 8_I4B, 6_I4B, &
     10_I4B, 4_I4B]
a0 = a

! arg is set by the routine, its value on entry is not used
arg = 0_I4B
CALL ArgHeapSort(a, arg)
CALL OK(ALL(arg .EQ. [4_I4B, 6_I4B, 2_I4B, 10_I4B, 1_I4B, 8_I4B, &
                      5_I4B, 7_I4B, 3_I4B, 9_I4B]), "arg")
CALL OK(ALL(a(arg) .EQ. [(ii, ii=1, 10)]), "a(arg) is sorted")
CALL OK(ALL(a .EQ. a0), "array is not changed")

! duplicates
b = [(MOD(7_I4B * ii, 5_I4B), ii=1, 40)]
CALL ArgHeapSort(b, argb)
CALL OK(ALL(b(argb(1:39)) .LE. b(argb(2:40))), "duplicates")
CALL OK(ALL([(COUNT(argb .EQ. ii) .EQ. 1, ii=1, 40)]), &
        "arg is a permutation")

! empty vector
CALL ArgHeapSort(e, arge)
CALL OK(SIZE(arge) .EQ. 0_I4B, "empty vector")

END PROGRAM main
