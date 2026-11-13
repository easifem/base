!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: ArgHeapSort for a real vector

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE SortUtility, ONLY: ArgHeapSort
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP) :: a(7)
INTEGER(I4B) :: arg(7)

a = [3.5_DFP, -1.0_DFP, 2.25_DFP, 0.0_DFP, 7.75_DFP, -4.5_DFP, 1.5_DFP]
CALL ArgHeapSort(a, arg)
CALL OK(ALL(arg .EQ. [6_I4B, 2_I4B, 4_I4B, 7_I4B, 3_I4B, 1_I4B, &
                      5_I4B]), "arg")
CALL OK(ALL(a(arg) .EQ. [-4.5_DFP, -1.0_DFP, 0.0_DFP, 1.5_DFP, &
                         2.25_DFP, 3.5_DFP, 7.75_DFP]), "a(arg) is sorted")

END PROGRAM main
