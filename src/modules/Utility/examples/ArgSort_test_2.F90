!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: ArgSort for a real vector

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE SortUtility, ONLY: ArgSort
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP) :: x(7)
INTEGER(I4B) :: arg(7)

x = [3.5_DFP, -1.0_DFP, 2.25_DFP, 0.0_DFP, 7.75_DFP, -4.5_DFP, 1.5_DFP]
arg = [6_I4B, 2_I4B, 4_I4B, 7_I4B, 3_I4B, 1_I4B, 5_I4B]

CALL OK(ALL(ArgSort(x) .EQ. arg), "default")
CALL OK(ALL(ArgSort(x, "HeapSort") .EQ. arg), "HeapSort")
CALL OK(ALL(ArgSort(x, "QuickSort") .EQ. arg), "QuickSort")
CALL OK(ALL(ArgSort(x, "Insertion") .EQ. arg), "Insertion")
CALL OK(ALL(x(ArgSort(x)) .EQ. [-4.5_DFP, -1.0_DFP, 0.0_DFP, 1.5_DFP, &
                                2.25_DFP, 3.5_DFP, 7.75_DFP]), &
        "x(ArgSort(x)) is sorted")

END PROGRAM main
