!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Reallocate for a integer rank-6 array

PROGRAM main
USE GlobalData, ONLY: I4B
USE ReallocateUtility, ONLY: Reallocate
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B), ALLOCATABLE :: a(:, :, :, :, :, :)
INTEGER(I4B) :: s1(6), s2(6)

s1 = [2_I4B, 2_I4B, 2_I4B, 2_I4B, 2_I4B, 2_I4B]
s2 = [1_I4B, 1_I4B, 1_I4B, 1_I4B, 1_I4B, 3_I4B]

! not allocated: allocate and set to zero
CALL Reallocate(a, 2_I4B, 2_I4B, 2_I4B, 2_I4B, 2_I4B, 2_I4B)
CALL OK(ALL(SHAPE(a) .EQ. s1), "shape is (2, 2, 2, 2, 2, 2)")
CALL OK(ALL(a .EQ. 0_I4B), "all entries are zero")

! same shape: the old values are not kept
a = 1_I4B
CALL Reallocate(a, 2_I4B, 2_I4B, 2_I4B, 2_I4B, 2_I4B, 2_I4B)
CALL OK(ALL(a .EQ. 0_I4B), "old values are reset")

! isExpand is ignored for this rank, so the array shrinks
CALL Reallocate(a, 1_I4B, 1_I4B, 1_I4B, 1_I4B, 1_I4B, 3_I4B, &
                isExpand=.TRUE.)
CALL OK(ALL(SHAPE(a) .EQ. s2), "shape is (1, 1, 1, 1, 1, 3)")

! the shape can also be given as a vector
CALL Reallocate(a, s1)
CALL OK(ALL(SHAPE(a) .EQ. s1), "shape from s")
CALL OK(ALL(a .EQ. 0_I4B), "entries are zero")

END PROGRAM main
