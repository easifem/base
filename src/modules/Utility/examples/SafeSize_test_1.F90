!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: SafeSize for an allocatable integer vector

PROGRAM main
USE GlobalData, ONLY: I4B
USE SafeSizeUtility, ONLY: SafeSize
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B), ALLOCATABLE :: a(:)

! not allocated
CALL OK(SafeSize(a) .EQ. 0_I4B, "not allocated")

! allocated with size 0
ALLOCATE (a(0))
CALL OK(SafeSize(a) .EQ. 0_I4B, "allocated with size 0")
DEALLOCATE (a)

! allocated with size 5
ALLOCATE (a(5))
a = 1_I4B
CALL OK(SafeSize(a) .EQ. 5_I4B, "allocated with size 5")
CALL OK(SafeSize(a) .EQ. SIZE(a), "same as SIZE when allocated")

! after deallocation
DEALLOCATE (a)
CALL OK(SafeSize(a) .EQ. 0_I4B, "after deallocation")

END PROGRAM main
