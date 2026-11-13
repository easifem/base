!> author: Vikas Sharma, Ph. D.
! date: 2026-10-19
! summary: SafeSize of an allocatable logical vector

PROGRAM main
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE SafeSizeUtility, ONLY: SafeSize
USE Test_Method, ONLY: OK
IMPLICIT NONE

LOGICAL(LGT), ALLOCATABLE :: a(:)

! not allocated
CALL OK(SafeSize(a) .EQ. 0_I4B, "not allocated")

! allocated with size 3
ALLOCATE (a(3))
a = .TRUE.
CALL OK(SafeSize(a) .EQ. 3_I4B, "allocated with size 3")

END PROGRAM main
