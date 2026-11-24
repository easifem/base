!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: Length of data in a FEVariable by GetLen and .len.

PROGRAM main
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: Matrix => TypeFEVariableMatrix
USE BaseType, ONLY: Space => TypeFEVariableSpace
USE FEVariable_Method, ONLY: GetLen
USE FEVariable_Method, ONLY: NodalVariable
USE FEVariable_Method, ONLY: OPERATOR(.len.)
USE FEVariable_Method, ONLY: SetLen
USE Test_Method, ONLY: OK
IMPLICIT NONE

! Test 1: len is the product of the shape
BLOCK
  TYPE(FEVariable_) :: obj

  obj = NodalVariable(dim1=2, dim2=2, dim3=3, rank=Matrix, vartype=Space)
  CALL OK(GetLen(obj) .EQ. 12, "GetLen = 12")
  CALL OK((.len.obj) .EQ. 12, ".len. = 12")
END BLOCK

! Test 2: len follows SetLen, and is zero for an empty object
BLOCK
  TYPE(FEVariable_) :: obj, empty

  obj = NodalVariable(dim1=2, dim2=2, dim3=3, rank=Matrix, vartype=Space)
  CALL SetLen(obj, 5)
  CALL OK(GetLen(obj) .EQ. 5, "GetLen = 5 after SetLen")
  CALL OK(GetLen(empty) .EQ. 0, "GetLen = 0 for empty object")
END BLOCK

END PROGRAM main
