!> author: Vikas Sharma, Ph. D.
! date: 2026-11-05
! summary: GetNNZ(obj1, obj2, op, isSorted) for the sum of two patterns
!
! Description:
!   The result is the size of the union of both patterns, row by row.
!   Covers op "+" and "-", sorted and unsorted columns, shared and
!   disjoint entries, and empty rows.

PROGRAM main
USE BaseType, ONLY: CSRSparsity_
USE CSRSparsity_Method, ONLY: DEALLOCATE
USE CSRSparsity_Method, ONLY: GetNNZ
USE CSRSparsity_Method, ONLY: Initiate
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE Test_Method, ONLY: OK
IMPLICIT NONE

LOGICAL(LGT), PARAMETER :: yes = .TRUE.

! Test 1: sorted columns, one shared entry
BLOCK
  TYPE(CSRSparsity_) :: a, b
  CALL Initiate(obj=a, IA=[1_I4B, 3_I4B, 4_I4B], &
                JA=[1_I4B, 2_I4B, 2_I4B])
  CALL Initiate(obj=b, IA=[1_I4B, 2_I4B, 4_I4B], &
                JA=[2_I4B, 1_I4B, 2_I4B])
  CALL OK(GetNNZ(obj1=a, obj2=b, op="+") .EQ. 4_I4B, "sorted: +")
  CALL OK(GetNNZ(obj1=a, obj2=b, op="-") .EQ. 4_I4B, "sorted: -")
  CALL OK(GetNNZ(obj1=a, obj2=b, op="+", isSorted=yes) .EQ. 4_I4B, &
          "sorted: + with isSorted")
  CALL OK(GetNNZ(obj1=a, obj2=b, op="-", isSorted=yes) .EQ. 4_I4B, &
          "sorted: - with isSorted")
  CALL DEALLOCATE (a)
  CALL DEALLOCATE (b)
END BLOCK

! Test 2: unsorted columns need the default isSorted
BLOCK
  TYPE(CSRSparsity_) :: a, b
  CALL Initiate(obj=a, IA=[1_I4B, 4_I4B], &
                JA=[3_I4B, 1_I4B, 2_I4B])
  CALL Initiate(obj=b, IA=[1_I4B, 3_I4B], JA=[2_I4B, 3_I4B])
  CALL OK(GetNNZ(obj1=a, obj2=b, op="+") .EQ. 3_I4B, "unsorted: +")
  CALL DEALLOCATE (a)
  CALL DEALLOCATE (b)
END BLOCK

! Test 3: disjoint patterns with empty rows
BLOCK
  TYPE(CSRSparsity_) :: a, b
  CALL Initiate(obj=a, IA=[1_I4B, 1_I4B, 3_I4B], JA=[1_I4B, 2_I4B])
  CALL Initiate(obj=b, IA=[1_I4B, 2_I4B, 2_I4B], JA=[2_I4B])
  CALL OK(GetNNZ(obj1=a, obj2=b, op="+") .EQ. 3_I4B, "disjoint")
  CALL OK(GetNNZ(obj1=a, obj2=b, op="+", isSorted=yes) .EQ. 3_I4B, &
          "disjoint with isSorted")
  CALL DEALLOCATE (a)
  CALL DEALLOCATE (b)
END BLOCK

END PROGRAM main
