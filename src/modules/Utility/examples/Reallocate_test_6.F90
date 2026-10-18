!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Reallocate for a real rank-3 array

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE ReallocateUtility, ONLY: Reallocate
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), ALLOCATABLE :: a(:, :, :)

! not allocated: allocate and set to zero
CALL Reallocate(a, 2_I4B, 3_I4B, 4_I4B)
CALL OK(ALL(SHAPE(a) .EQ. [2_I4B, 3_I4B, 4_I4B]), "shape is (2, 3, 4)")
CALL OK(ALL(a .EQ. 0.0_DFP), "all entries are zero")

! isExpand: grow the last dimension
CALL Reallocate(a, 2_I4B, 3_I4B, 8_I4B, isExpand=.TRUE., &
                expandFactor=1_I4B)
CALL OK(ALL(SHAPE(a) .EQ. [2_I4B, 3_I4B, 8_I4B]), "shape is (2, 3, 8)")

! isExpand: a large enough array is kept
CALL Reallocate(a, 2_I4B, 3_I4B, 4_I4B, isExpand=.TRUE., &
                expandFactor=2_I4B)
CALL OK(ALL(SHAPE(a) .EQ. [2_I4B, 3_I4B, 8_I4B]), "shape is kept")

! expandFactor: only the dimensions that are too small grow
CALL Reallocate(a, 4_I4B, 6_I4B, 4_I4B, isExpand=.TRUE., &
                expandFactor=2_I4B)
CALL OK(ALL(SHAPE(a) .EQ. [8_I4B, 12_I4B, 8_I4B]), &
        "shape is (8, 12, 8)")

! the shape can also be given as a vector
a = 1.0_DFP
CALL Reallocate(a, [1_I4B, 2_I4B, 3_I4B])
CALL OK(ALL(SHAPE(a) .EQ. [1_I4B, 2_I4B, 3_I4B]), "shape from s")
CALL OK(ALL(a .EQ. 0.0_DFP), "entries are zero")

END PROGRAM main
