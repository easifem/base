!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Reallocate for an integer vector

PROGRAM main
USE GlobalData, ONLY: I4B
USE ReallocateUtility, ONLY: Reallocate
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B), ALLOCATABLE :: a(:)

! not allocated: allocate and set to zero
CALL Reallocate(a, 3_I4B)
CALL OK(SIZE(a) .EQ. 3_I4B, "size is 3")
CALL OK(ALL(a .EQ. 0_I4B), "all entries are zero")

! a different size without isExpand
a = 7_I4B
CALL Reallocate(a, 2_I4B)
CALL OK(SIZE(a) .EQ. 2_I4B, "size is 2")
CALL OK(ALL(a .EQ. 0_I4B), "entries are zero")

! not allocated: expandFactor is not used, as for rank 2 and 3
DEALLOCATE (a)
CALL Reallocate(a, 3_I4B, isExpand=.TRUE., expandFactor=2_I4B)
CALL OK(SIZE(a) .EQ. 3_I4B, "new vector has size 3")
CALL OK(ALL(a .EQ. 0_I4B), "new vector is zero")
DEALLOCATE (a)
CALL Reallocate(a, 3_I4B, expandFactor=2_I4B)
CALL OK(SIZE(a) .EQ. 3_I4B, "expandFactor alone: size 3")

! allocated and too small: the new size is row * expandFactor
CALL Reallocate(a, 4_I4B, isExpand=.TRUE., expandFactor=2_I4B)
CALL OK(SIZE(a) .EQ. 8_I4B, "grows to 4 * 2")

! the size can also be given as a vector
CALL Reallocate(a, [5_I4B])
CALL OK(SIZE(a) .EQ. 5_I4B, "size from s(1)")

END PROGRAM main
