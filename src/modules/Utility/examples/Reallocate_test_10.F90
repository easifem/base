!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Reallocate for a real rank-5 array

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE ReallocateUtility, ONLY: Reallocate
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), ALLOCATABLE :: a(:, :, :, :, :)
INTEGER(I4B) :: s1(5), s2(5)

s1 = [2_I4B, 2_I4B, 2_I4B, 2_I4B, 2_I4B]
s2 = [1_I4B, 1_I4B, 1_I4B, 1_I4B, 3_I4B]

! not allocated: allocate and set to zero
CALL Reallocate(a, 2_I4B, 2_I4B, 2_I4B, 2_I4B, 2_I4B)
CALL OK(ALL(SHAPE(a) .EQ. s1), "shape is (2, 2, 2, 2, 2)")
CALL OK(ALL(a .EQ. 0.0_DFP), "all entries are zero")

! same shape: the old values are not kept
a = 1.0_DFP
CALL Reallocate(a, 2_I4B, 2_I4B, 2_I4B, 2_I4B, 2_I4B)
CALL OK(ALL(a .EQ. 0.0_DFP), "old values are reset")

! isExpand is ignored for this rank, so the array shrinks
CALL Reallocate(a, 1_I4B, 1_I4B, 1_I4B, 1_I4B, 3_I4B, &
                isExpand=.TRUE.)
CALL OK(ALL(SHAPE(a) .EQ. s2), "shape is (1, 1, 1, 1, 3)")

! the shape can also be given as a vector
CALL Reallocate(a, s1)
CALL OK(ALL(SHAPE(a) .EQ. s1), "shape from s")
CALL OK(ALL(a .EQ. 0.0_DFP), "entries are zero")

END PROGRAM main
