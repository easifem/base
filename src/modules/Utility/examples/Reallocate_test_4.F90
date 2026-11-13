!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Reallocate for a real matrix

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE ReallocateUtility, ONLY: Reallocate
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), ALLOCATABLE :: a(:, :)

! not allocated: allocate and set to zero
CALL Reallocate(a, 10_I4B, 4_I4B)
CALL OK(ALL(SHAPE(a) .EQ. [10_I4B, 4_I4B]), "shape is (10, 4)")
CALL OK(ALL(a .EQ. 0.0_DFP), "all entries are zero")

! isExpand: grow the rows
CALL Reallocate(a, 20_I4B, 4_I4B, isExpand=.TRUE., expandFactor=1_I4B)
CALL OK(ALL(SHAPE(a) .EQ. [20_I4B, 4_I4B]), "shape is (20, 4)")

! isExpand: a larger matrix is kept
CALL Reallocate(a, 10_I4B, 4_I4B, isExpand=.TRUE., expandFactor=1_I4B)
CALL OK(ALL(SHAPE(a) .EQ. [20_I4B, 4_I4B]), "shape is still (20, 4)")

! expandFactor: only a dimension that is too small grows
CALL Reallocate(a, 30_I4B, 4_I4B, isExpand=.TRUE., expandFactor=2_I4B)
CALL OK(ALL(SHAPE(a) .EQ. [60_I4B, 4_I4B]), "shape is (60, 4)")

CALL Reallocate(a, 30_I4B, 8_I4B, isExpand=.TRUE., expandFactor=2_I4B)
CALL OK(ALL(SHAPE(a) .EQ. [60_I4B, 16_I4B]), "shape is (60, 16)")

! isExpand: only the leading block a(1:row, 1:col) is reset
a = 1.0_DFP
CALL Reallocate(a, 2_I4B, 3_I4B, isExpand=.TRUE.)
CALL OK(ALL(SHAPE(a) .EQ. [60_I4B, 16_I4B]), "shape is kept")
CALL OK(ALL(a(1:2, 1:3) .EQ. 0.0_DFP), "leading block is reset")
CALL OK(a(3, 1) .EQ. 1.0_DFP, "a(3, 1) is not touched")

! the shape can also be given as a vector
CALL Reallocate(a, [2_I4B, 3_I4B])
CALL OK(ALL(SHAPE(a) .EQ. [2_I4B, 3_I4B]), "shape from s")
CALL OK(ALL(a .EQ. 0.0_DFP), "entries are zero")

END PROGRAM main
