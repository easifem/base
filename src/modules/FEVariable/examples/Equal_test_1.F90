!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: Compare two FEVariables by == and /=

PROGRAM main
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: Scalar => TypeFEVariableScalar
USE BaseType, ONLY: Space => TypeFEVariableSpace
USE BaseType, ONLY: Time => TypeFEVariableTime
USE FEVariable_Method, ONLY: NodalVariable
USE FEVariable_Method, ONLY: OPERATOR(.EQ.)
USE FEVariable_Method, ONLY: OPERATOR(.NE.)
USE FEVariable_Method, ONLY: QuadratureVariable
USE FEVariable_Method, ONLY: SetLen
USE GlobalData, ONLY: DFP
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP) :: a(3)

a = [1.0_DFP, 2.0_DFP, 3.0_DFP]

! Test 1: same data are equal
BLOCK
  TYPE(FEVariable_) :: obj1, obj2

  obj1 = QuadratureVariable(a, Scalar, Space)
  obj2 = obj1
  CALL OK(obj1 .EQ. obj2, "same: ==")
  CALL OK(.NOT. (obj1 .NE. obj2), "same: not /=")
END BLOCK

! Test 2: different values, defineOn, varType or len are not equal
BLOCK
  TYPE(FEVariable_) :: obj1, obj2

  obj1 = QuadratureVariable(a, Scalar, Space)
  obj2 = QuadratureVariable(a + 1.0_DFP, Scalar, Space)
  CALL OK(obj1 .NE. obj2, "values differ: /=")
  obj2 = NodalVariable(a, Scalar, Space)
  CALL OK(obj1 .NE. obj2, "defineOn differs: /=")
  obj2 = QuadratureVariable(a, Scalar, Time)
  CALL OK(obj1 .NE. obj2, "varType differs: /=")
  obj2 = QuadratureVariable(a(1:2), Scalar, Space)
  CALL OK(.NOT. (obj1 .EQ. obj2), "len differs: not ==")
  CALL OK(obj1 .NE. obj2, "len differs: /=")
END BLOCK

! Test 3: same data with a larger capacity are equal
BLOCK
  TYPE(FEVariable_) :: obj1, obj2

  obj1 = QuadratureVariable(a, Scalar, Space)
  obj2 = QuadratureVariable([a, 7.0_DFP], Scalar, Space)
  CALL SetLen(obj2, 3)
  obj2%s(1) = 3
  obj2%val(1:3) = a
  CALL OK(obj1 .EQ. obj2, "larger capacity: ==")
  CALL OK(.NOT. (obj1 .NE. obj2), "larger capacity: not /=")
END BLOCK

END PROGRAM main
