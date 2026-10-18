!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: ArgIntroSort for a real vector

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE SortUtility, ONLY: ArgIntroSort
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP) :: a(7), a0(7), b(40)
INTEGER(I4B) :: arg(7), argb(40)
INTEGER(I4B) :: ii

a = [3.5_DFP, -1.0_DFP, 2.25_DFP, 0.0_DFP, 7.75_DFP, -4.5_DFP, 1.5_DFP]
a0 = a
arg = [(ii, ii=1, 7)]
CALL ArgIntroSort(a, arg)
CALL OK(ALL(arg .EQ. [6_I4B, 2_I4B, 4_I4B, 7_I4B, 3_I4B, 1_I4B, &
                      5_I4B]), "arg")
CALL OK(ALL(a(arg(1:6)) .LE. a(arg(2:7))), "a(arg) is sorted")
CALL OK(ALL(a .EQ. a0), "array is not changed")

! long vector, a permutation of 0.5 * (1..40)
b = [(0.5_DFP * MOD(37_I4B * ii, 41_I4B), ii=1, 40)]
argb = [(ii, ii=1, 40)]
CALL ArgIntroSort(b, argb)
CALL OK(ALL(b(argb) .EQ. [(0.5_DFP * ii, ii=1, 40)]), "long vector")
CALL OK(ALL([(COUNT(argb .EQ. ii) .EQ. 1, ii=1, 40)]), &
        "arg is a permutation")

END PROGRAM main
