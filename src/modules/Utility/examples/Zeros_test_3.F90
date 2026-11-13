!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Zeros for an integer matrix

PROGRAM main
USE GlobalData, ONLY: I4B
USE ZerosUtility, ONLY: Zeros
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B), ALLOCATABLE :: a(:, :)

a = Zeros(2_I4B, 3_I4B, 1_I4B)
CALL OK(ALL(SHAPE(a) .EQ. [2_I4B, 3_I4B]), "shape of the result")
CALL OK(ALL(a .EQ. 0_I4B), "all entries are zero")
CALL OK(KIND(Zeros(2_I4B, 3_I4B, 0_I4B)) .EQ. I4B, "kind of the result")

! zero size in one dimension
a = Zeros(2_I4B, 0_I4B, 1_I4B)
CALL OK(SIZE(a) .EQ. 0_I4B, "zero size")

END PROGRAM main
