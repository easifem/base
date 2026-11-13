!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: Convert a rank name (scalar, vector, matrix) to its integer

PROGRAM main
USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
USE FEVariable_Method, ONLY: FEVariable_ToInteger
USE Test_Method, ONLY: OK
IMPLICIT NONE

! Test 1: names in any case, only the first letter is used
BLOCK
  CALL OK(FEVariable_ToInteger("Scalar") .EQ. fevaropt%scalar, "Scalar")
  CALL OK(FEVariable_ToInteger("VECTOR") .EQ. fevaropt%vector, "VECTOR")
  CALL OK(FEVariable_ToInteger("matrix") .EQ. fevaropt%matrix, "matrix")
  CALL OK(FEVariable_ToInteger("v") .EQ. fevaropt%vector, "v")
END BLOCK

! Test 2: unknown name gives scalar
BLOCK
  CALL OK(FEVariable_ToInteger("tensor") .EQ. fevaropt%scalar, &
          "unknown: scalar")
END BLOCK

END PROGRAM main
