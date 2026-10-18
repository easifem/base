!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Reallocate for a real vector

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE ReallocateUtility, ONLY: Reallocate
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), ALLOCATABLE :: a(:)

! not allocated: allocate and set to zero
CALL Reallocate(a, 10_I4B)
CALL OK(SIZE(a) .EQ. 10_I4B, "size is 10")
CALL OK(ALL(a .EQ. 0.0_DFP), "all entries are zero")

! same size: the old values are not kept
a = 1.0_DFP
CALL Reallocate(a, 10_I4B)
CALL OK(ALL(a .EQ. 0.0_DFP), "old values are reset")

! without isExpand the vector can shrink
CALL Reallocate(a, 5_I4B)
CALL OK(SIZE(a) .EQ. 5_I4B, "size is 5")

! isExpand: grow when the vector is too small
CALL Reallocate(a, 20_I4B, isExpand=.TRUE., expandFactor=1_I4B)
CALL OK(SIZE(a) .EQ. 20_I4B, "isExpand grows to 20")

! isExpand: keep a larger vector, only a(1:row) is reset
a = 1.0_DFP
CALL Reallocate(a, 10_I4B, isExpand=.TRUE.)
CALL OK(SIZE(a) .EQ. 20_I4B, "isExpand keeps size 20")
CALL OK(ALL(a(1:10) .EQ. 0.0_DFP), "a(1:10) is reset")
CALL OK(ALL(a(11:20) .EQ. 1.0_DFP), "a(11:20) is not touched")

! expandFactor: the new size is row * expandFactor
CALL Reallocate(a, 30_I4B, isExpand=.TRUE., expandFactor=2_I4B)
CALL OK(SIZE(a) .EQ. 60_I4B, "size is 30 * 2")

! the size can also be given as a vector
CALL Reallocate(a, [4_I4B])
CALL OK(SIZE(a) .EQ. 4_I4B, "size from s(1)")

! a vector of size zero
CALL Reallocate(a, 0_I4B)
CALL OK(ALLOCATED(a), "allocated with size 0")
CALL OK(SIZE(a) .EQ. 0_I4B, "size is 0")

END PROGRAM main
