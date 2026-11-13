!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Reallocate up to six integer vectors in one call

PROGRAM main
USE GlobalData, ONLY: I4B
USE ReallocateUtility, ONLY: Reallocate
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B), ALLOCATABLE :: v1(:), v2(:), v3(:), v4(:), v5(:), v6(:)

! two vectors
CALL Reallocate(v1, 3_I4B, v2, 4_I4B)
CALL OK(SIZE(v1) .EQ. 3_I4B, "size of v1 is 3")
CALL OK(SIZE(v2) .EQ. 4_I4B, "size of v2 is 4")
CALL OK(ALL(v1 .EQ. 0_I4B) .AND. ALL(v2 .EQ. 0_I4B), "entries are zero")
CALL OK(.NOT. ALLOCATED(v3), "v3 is not touched")

! the old values are not kept
v1 = 7_I4B
v2 = 7_I4B
CALL Reallocate(v1, 3_I4B, v2, 2_I4B)
CALL OK(ALL(v1 .EQ. 0_I4B), "v1 is reset")
CALL OK(SIZE(v2) .EQ. 2_I4B, "size of v2 is 2")

! six vectors
CALL Reallocate(v1, 1_I4B, v2, 2_I4B, v3, 3_I4B, v4, 4_I4B, &
                v5, 5_I4B, v6, 6_I4B)
CALL OK(SIZE(v1) .EQ. 1_I4B, "size of v1 is 1")
CALL OK(SIZE(v3) .EQ. 3_I4B, "size of v3 is 3")
CALL OK(SIZE(v6) .EQ. 6_I4B, "size of v6 is 6")
CALL OK(ALL(v6 .EQ. 0_I4B), "v6 is zero")

END PROGRAM main
