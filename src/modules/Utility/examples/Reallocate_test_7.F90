!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Reallocate for an integer rank-3 array

PROGRAM main
USE GlobalData, ONLY: I4B
USE ReallocateUtility, ONLY: Reallocate
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B), ALLOCATABLE :: a(:, :, :)

! not allocated: allocate and set to zero
CALL Reallocate(a, 2_I4B, 2_I4B, 2_I4B)
CALL OK(ALL(SHAPE(a) .EQ. [2_I4B, 2_I4B, 2_I4B]), "shape is (2, 2, 2)")
CALL OK(ALL(a .EQ. 0_I4B), "all entries are zero")

! a different shape without isExpand
a = 3_I4B
CALL Reallocate(a, 1_I4B, 2_I4B, 3_I4B)
CALL OK(ALL(SHAPE(a) .EQ. [1_I4B, 2_I4B, 3_I4B]), "shape is (1, 2, 3)")
CALL OK(ALL(a .EQ. 0_I4B), "entries are zero")

! isExpand with expandFactor
CALL Reallocate(a, 2_I4B, 2_I4B, 3_I4B, isExpand=.TRUE., &
                expandFactor=3_I4B)
CALL OK(ALL(SHAPE(a) .EQ. [6_I4B, 2_I4B, 3_I4B]), "shape is (6, 2, 3)")

! the shape can also be given as a vector
CALL Reallocate(a, [2_I4B, 1_I4B, 1_I4B])
CALL OK(ALL(SHAPE(a) .EQ. [2_I4B, 1_I4B, 1_I4B]), "shape from s")

END PROGRAM main
