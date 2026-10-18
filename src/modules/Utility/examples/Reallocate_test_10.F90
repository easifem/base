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
s2 = [2_I4B, 2_I4B, 2_I4B, 2_I4B, 3_I4B]

! not allocated: allocate and set to zero
CALL Reallocate(a, 2_I4B, 2_I4B, 2_I4B, 2_I4B, 2_I4B)
CALL OK(ALL(SHAPE(a) .EQ. s1), "shape is (2, 2, 2, 2, 2)")
CALL OK(ALL(a .EQ. 0.0_DFP), "all entries are zero")

! same shape: the old values are not kept
a = 1.0_DFP
CALL Reallocate(a, 2_I4B, 2_I4B, 2_I4B, 2_I4B, 2_I4B)
CALL OK(ALL(a .EQ. 0.0_DFP), "old values are reset")

! isExpand: dimensions that are large enough are kept, a
! dimension that is too small grows to the requested size
CALL Reallocate(a, 1_I4B, 1_I4B, 1_I4B, 1_I4B, 3_I4B, &
                isExpand=.TRUE.)
CALL OK(ALL(SHAPE(a) .EQ. s2), "isExpand: shape is (2, 2, 2, 2, 3)")

! expandFactor: a dimension that is too small grows to
! n * expandFactor, the others are kept
CALL Reallocate(a, 1_I4B, 1_I4B, 1_I4B, 1_I4B, 4_I4B, &
                isExpand=.TRUE., expandFactor=2_I4B)
CALL OK(SIZE(a, 5) .EQ. 8_I4B, "expandFactor: last extent is 8")
CALL OK(ALL(SHAPE(a) .GE. 2_I4B), "expandFactor: others are kept")

! the shape can also be given as a vector
CALL Reallocate(a, s1)
CALL OK(ALL(SHAPE(a) .EQ. s1), "shape from s")
CALL OK(ALL(a .EQ. 0.0_DFP), "entries are zero")

END PROGRAM main
