!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: Convert a rank (scalar, vector, matrix) to its name

PROGRAM main
USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
USE FEVariable_Method, ONLY: FEVariable_ToChar
USE Test_Method, ONLY: OK
IMPLICIT NONE

! Test 1: names of scalar, vector and matrix
BLOCK
  CALL OK(FEVariable_ToChar(fevaropt%scalar) .EQ. "Scalar", "Scalar")
  CALL OK(FEVariable_ToChar(fevaropt%vector) .EQ. "Vector", "Vector")
  CALL OK(FEVariable_ToChar(fevaropt%matrix) .EQ. "Matrix", "Matrix")
END BLOCK

! Test 2: upper case names
BLOCK
  CALL OK(FEVariable_ToChar(fevaropt%vector, isUpper=.TRUE.) .EQ. &
          "VECTOR", "VECTOR")
  CALL OK(FEVariable_ToChar(fevaropt%matrix, isUpper=.FALSE.) .EQ. &
          "Matrix", "isUpper false: Matrix")
END BLOCK

! Test 3: unknown rank gives Scalar
BLOCK
  CALL OK(FEVariable_ToChar(-1) .EQ. "Scalar", "unknown: Scalar")
END BLOCK

END PROGRAM main
