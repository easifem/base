!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: == and /= when rank or shape differ but values are the same

PROGRAM main
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: Scalar => TypeFEVariableScalar
USE BaseType, ONLY: Space => TypeFEVariableSpace
USE BaseType, ONLY: SpaceTime => TypeFEVariableSpaceTime
USE BaseType, ONLY: Vector => TypeFEVariableVector
USE FEVariable_Method, ONLY: OPERATOR(.EQ.)
USE FEVariable_Method, ONLY: OPERATOR(.NE.)
USE FEVariable_Method, ONLY: QuadratureVariable
USE GlobalData, ONLY: DFP
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP) :: a(4)

a = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP]

! Test 1: same values, different rank are not equal
BLOCK
  TYPE(FEVariable_) :: obj1, obj2

  obj1 = QuadratureVariable(a, Scalar, Space)
  obj2 = QuadratureVariable(RESHAPE(a, [2, 2]), Vector, Space)
  CALL OK(.NOT. (obj1 .EQ. obj2), "rank differs: not ==")
  CALL OK(obj1 .NE. obj2, "rank differs: /=")
END BLOCK

! Test 2: same values and rank, different shape are not equal
BLOCK
  TYPE(FEVariable_) :: obj1, obj2

  obj1 = QuadratureVariable(RESHAPE(a, [2, 2]), Vector, Space)
  obj2 = QuadratureVariable(RESHAPE(a, [4, 1]), Vector, Space)
  CALL OK(.NOT. (obj1 .EQ. obj2), "shape differs: not ==")
  CALL OK(obj1 .NE. obj2, "shape differs: /=")
END BLOCK

! Test 3: copies of a space-time scalar are equal
BLOCK
  TYPE(FEVariable_) :: obj1, obj2

  obj1 = QuadratureVariable(RESHAPE(a, [2, 2]), Scalar, SpaceTime)
  obj2 = obj1
  CALL OK(obj1 .EQ. obj2, "spaceTime copy: ==")
  CALL OK(.NOT. (obj1 .NE. obj2), "spaceTime copy: not /=")
END BLOCK

END PROGRAM main
