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

! not allocated with isExpand and expandFactor
DEALLOCATE (a)
CALL Reallocate(a, 3_I4B, isExpand=.TRUE., expandFactor=2_I4B)
CALL OK(SIZE(a) .EQ. 6_I4B, "size is 3 * 2")
CALL OK(ALL(a(1:3) .EQ. 0_I4B), "a(1:3) is zero")

! the size can also be given as a vector
CALL Reallocate(a, [5_I4B])
CALL OK(SIZE(a) .EQ. 5_I4B, "size from s(1)")

END PROGRAM main
