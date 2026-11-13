!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Reallocate for an integer matrix

PROGRAM main
USE GlobalData, ONLY: I4B
USE ReallocateUtility, ONLY: Reallocate
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B), ALLOCATABLE :: a(:, :)

! not allocated: allocate and set to zero
CALL Reallocate(a, 2_I4B, 3_I4B)
CALL OK(ALL(SHAPE(a) .EQ. [2_I4B, 3_I4B]), "shape is (2, 3)")
CALL OK(ALL(a .EQ. 0_I4B), "all entries are zero")

! a different shape without isExpand
a = 5_I4B
CALL Reallocate(a, 3_I4B, 2_I4B)
CALL OK(ALL(SHAPE(a) .EQ. [3_I4B, 2_I4B]), "shape is (3, 2)")
CALL OK(ALL(a .EQ. 0_I4B), "entries are zero")

! isExpand with expandFactor
CALL Reallocate(a, 4_I4B, 2_I4B, isExpand=.TRUE., expandFactor=2_I4B)
CALL OK(ALL(SHAPE(a) .EQ. [8_I4B, 2_I4B]), "shape is (8, 2)")

! the shape can also be given as a vector
CALL Reallocate(a, [1_I4B, 4_I4B])
CALL OK(ALL(SHAPE(a) .EQ. [1_I4B, 4_I4B]), "shape from s")

END PROGRAM main
