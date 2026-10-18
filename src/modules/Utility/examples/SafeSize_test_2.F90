!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: SafeSize for an allocatable real vector

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE SafeSizeUtility, ONLY: SafeSize
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), ALLOCATABLE :: a(:)

! not allocated
CALL OK(SafeSize(a) .EQ. 0_I4B, "not allocated")

! allocated with size 0
ALLOCATE (a(0))
CALL OK(SafeSize(a) .EQ. 0_I4B, "allocated with size 0")
DEALLOCATE (a)

! allocated with size 3, lower bound is not 1
ALLOCATE (a(-1:1))
a = 0.5_DFP
CALL OK(SafeSize(a) .EQ. 3_I4B, "allocated with size 3")

! automatic reallocation on assignment
a = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP]
CALL OK(SafeSize(a) .EQ. 4_I4B, "size after reallocation")

DEALLOCATE (a)
CALL OK(SafeSize(a) .EQ. 0_I4B, "after deallocation")

END PROGRAM main
