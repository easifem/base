!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Ones for an integer matrix

PROGRAM main
USE GlobalData, ONLY: I4B
USE OnesUtility, ONLY: Ones
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B), ALLOCATABLE :: a(:, :)

a = Ones(2_I4B, 3_I4B, 1_I4B)
CALL OK(ALL(SHAPE(a) .EQ. [2_I4B, 3_I4B]), "shape of the result")
CALL OK(ALL(a .EQ. 1_I4B), "all entries are one")
CALL OK(KIND(Ones(2_I4B, 3_I4B, 0_I4B)) .EQ. I4B, "kind of the result")

! zero size in one dimension
a = Ones(2_I4B, 0_I4B, 1_I4B)
CALL OK(SIZE(a) .EQ. 0_I4B, "zero size")

END PROGRAM main
