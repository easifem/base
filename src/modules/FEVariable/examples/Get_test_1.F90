!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: Get the values of a FEVariable as an allocated array

PROGRAM main
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: Constant => TypeFEVariableConstant
USE BaseType, ONLY: Matrix => TypeFEVariableMatrix
USE BaseType, ONLY: Scalar => TypeFEVariableScalar
USE BaseType, ONLY: Space => TypeFEVariableSpace
USE BaseType, ONLY: SpaceTime => TypeFEVariableSpaceTime
USE BaseType, ONLY: Time => TypeFEVariableTime
USE BaseType, ONLY: Vector => TypeFEVariableVector
USE FEVariable_Method, ONLY: Get
USE FEVariable_Method, ONLY: NodalVariable
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B) :: ii
REAL(DFP) :: v(24)

v = [(REAL(ii, DFP), ii=1, 24)]

! Test 1: scalar variables
BLOCK
  TYPE(FEVariable_) :: obj
  REAL(DFP), ALLOCATABLE :: r1(:), r2(:, :)

  obj = NodalVariable(3.0_DFP, Scalar, Constant)
  CALL OK(Get(obj, Scalar, Constant) .EQ. 3.0_DFP, "scalar constant")

  obj = NodalVariable(v(1:4), Scalar, Space)
  r1 = Get(obj, Scalar, Space)
  CALL OK(ALL(r1 .EQ. v(1:4)), "scalar space")

  obj = NodalVariable(v(1:2), Scalar, Time)
  r1 = Get(obj, Scalar, Time)
  CALL OK(ALL(r1 .EQ. v(1:2)), "scalar time")

  obj = NodalVariable(RESHAPE(v(1:6), [3, 2]), Scalar, SpaceTime)
  r2 = Get(obj, Scalar, SpaceTime)
  CALL OK(ALL(SHAPE(r2) .EQ. [3, 2]), "scalar spaceTime: shape")
  CALL OK(ALL(r2 .EQ. RESHAPE(v(1:6), [3, 2])), "scalar spaceTime")
END BLOCK

! Test 2: vector variables
BLOCK
  TYPE(FEVariable_) :: obj
  REAL(DFP), ALLOCATABLE :: r1(:), r2(:, :), r3(:, :, :)

  obj = NodalVariable(v(1:3), Vector, Constant)
  r1 = Get(obj, Vector, Constant)
  CALL OK(ALL(r1 .EQ. v(1:3)), "vector constant")

  obj = NodalVariable(RESHAPE(v(1:6), [2, 3]), Vector, Space)
  r2 = Get(obj, Vector, Space)
  CALL OK(ALL(r2 .EQ. RESHAPE(v(1:6), [2, 3])), "vector space")

  obj = NodalVariable(RESHAPE(v(1:6), [3, 2]), Vector, Time)
  r2 = Get(obj, Vector, Time)
  CALL OK(ALL(r2 .EQ. RESHAPE(v(1:6), [3, 2])), "vector time")

  obj = NodalVariable(RESHAPE(v(1:12), [3, 2, 2]), Vector, SpaceTime)
  r3 = Get(obj, Vector, SpaceTime)
  CALL OK(ALL(r3 .EQ. RESHAPE(v(1:12), [3, 2, 2])), "vector spaceTime")
END BLOCK

! Test 3: matrix variables
BLOCK
  TYPE(FEVariable_) :: obj
  REAL(DFP), ALLOCATABLE :: r2(:, :), r3(:, :, :), r4(:, :, :, :)

  obj = NodalVariable(RESHAPE(v(1:6), [2, 3]), Matrix, Constant)
  r2 = Get(obj, Matrix, Constant)
  CALL OK(ALL(r2 .EQ. RESHAPE(v(1:6), [2, 3])), "matrix constant")

  obj = NodalVariable(RESHAPE(v(1:12), [2, 2, 3]), Matrix, Space)
  r3 = Get(obj, Matrix, Space)
  CALL OK(ALL(r3 .EQ. RESHAPE(v(1:12), [2, 2, 3])), "matrix space")

  obj = NodalVariable(RESHAPE(v(1:8), [2, 2, 2]), Matrix, Time)
  r3 = Get(obj, Matrix, Time)
  CALL OK(ALL(r3 .EQ. RESHAPE(v(1:8), [2, 2, 2])), "matrix time")

  obj = NodalVariable(RESHAPE(v, [2, 2, 3, 2]), Matrix, SpaceTime)
  r4 = Get(obj, Matrix, SpaceTime)
  CALL OK(ALL(SHAPE(r4) .EQ. [2, 2, 3, 2]), "matrix spaceTime: shape")
  CALL OK(ALL(r4 .EQ. RESHAPE(v, [2, 2, 3, 2])), "matrix spaceTime")
END BLOCK

END PROGRAM main
