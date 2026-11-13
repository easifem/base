!> author: Shion Shimizu
! date: 2026-11-13
! summary: Reallocate a logical rank-3 array

PROGRAM main
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE ReallocateUtility, ONLY: Reallocate
USE Test_Method, ONLY: OK
IMPLICIT NONE

! Test 1: not allocated, allocate (i1, i2, i3) and set to .FALSE.
BLOCK
  LOGICAL(LGT), ALLOCATABLE :: m(:, :, :)

  CALL Reallocate(m, 2_I4B, 3_I4B, 4_I4B)
  CALL OK(ALL(SHAPE(m) .EQ. [2_I4B, 3_I4B, 4_I4B]), &
          "shape is (2, 3, 4)")
  CALL OK(.NOT. ANY(m), "all entries are .FALSE.")
END BLOCK

! Test 2: same shape resets, another shape reallocates
BLOCK
  LOGICAL(LGT), ALLOCATABLE :: m(:, :, :)

  CALL Reallocate(m, 2_I4B, 3_I4B, 4_I4B)
  m = .TRUE.
  CALL Reallocate(m, 2_I4B, 3_I4B, 4_I4B)
  CALL OK(.NOT. ANY(m), "same shape: old values are reset")
  CALL Reallocate(m, 1_I4B, 1_I4B, 2_I4B)
  CALL OK(ALL(SHAPE(m) .EQ. [1_I4B, 1_I4B, 2_I4B]), &
          "new shape is (1, 1, 2)")
END BLOCK

! Test 3: isExpand keeps a larger array, only the block is reset
BLOCK
  LOGICAL(LGT), ALLOCATABLE :: m(:, :, :)

  CALL Reallocate(m, 3_I4B, 3_I4B, 3_I4B)
  m = .TRUE.
  CALL Reallocate(m, 2_I4B, 2_I4B, 2_I4B, isExpand=.TRUE.)
  CALL OK(ALL(SHAPE(m) .EQ. [3_I4B, 3_I4B, 3_I4B]), &
          "isExpand keeps (3, 3, 3)")
  CALL OK(.NOT. ANY(m(1:2, 1:2, 1:2)), "m(1:2, 1:2, 1:2) is reset")
  CALL OK(ALL(m(3, :, :)) .AND. ALL(m(:, :, 3)), "other entries kept")
END BLOCK

! Test 4: isExpand grows a too small extent by expandFactor
BLOCK
  LOGICAL(LGT), ALLOCATABLE :: m(:, :, :)

  CALL Reallocate(m, 2_I4B, 2_I4B, 2_I4B)
  CALL Reallocate(m, 2_I4B, 3_I4B, 1_I4B, isExpand=.TRUE., &
                  expandFactor=2_I4B)
  CALL OK(ALL(SHAPE(m) .EQ. [2_I4B, 6_I4B, 2_I4B]), &
          "shape is (2, 3 * 2, 2)")
  CALL OK(.NOT. ANY(m), "new array is .FALSE. everywhere")
END BLOCK

! Test 5: shape given as a vector
BLOCK
  LOGICAL(LGT), ALLOCATABLE :: m(:, :, :)

  CALL Reallocate(m, [4_I4B, 1_I4B, 2_I4B])
  CALL OK(ALL(SHAPE(m) .EQ. [4_I4B, 1_I4B, 2_I4B]), "shape from s")
  CALL OK(.NOT. ANY(m), "shape from s: all .FALSE.")
END BLOCK

END PROGRAM main
