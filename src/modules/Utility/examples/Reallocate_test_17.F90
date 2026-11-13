!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Reallocate up to six real vectors in one call

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE ReallocateUtility, ONLY: Reallocate
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), ALLOCATABLE :: v1(:), v2(:), v3(:)

! two vectors
CALL Reallocate(v1, 2_I4B, v2, 5_I4B)
CALL OK(SIZE(v1) .EQ. 2_I4B, "size of v1 is 2")
CALL OK(SIZE(v2) .EQ. 5_I4B, "size of v2 is 5")
CALL OK(ALL(v2 .EQ. 0.0_DFP), "v2 is zero")

! three vectors, the old values are not kept
v1 = 1.0_DFP
CALL Reallocate(v1, 2_I4B, v2, 1_I4B, v3, 4_I4B)
CALL OK(ALL(v1 .EQ. 0.0_DFP), "v1 is reset")
CALL OK(SIZE(v2) .EQ. 1_I4B, "size of v2 is 1")
CALL OK(SIZE(v3) .EQ. 4_I4B, "size of v3 is 4")

END PROGRAM main
