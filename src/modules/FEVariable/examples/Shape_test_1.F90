!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: Shape of a FEVariable

PROGRAM main
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: Constant => TypeFEVariableConstant
USE BaseType, ONLY: Matrix => TypeFEVariableMatrix
USE BaseType, ONLY: Scalar => TypeFEVariableScalar
USE BaseType, ONLY: Space => TypeFEVariableSpace
USE BaseType, ONLY: SpaceTime => TypeFEVariableSpaceTime
USE BaseType, ONLY: Vector => TypeFEVariableVector
USE FEVariable_Method, ONLY: QuadratureVariable
USE FEVariable_Method, ONLY: SHAPE
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
IMPLICIT NONE

! Test 1: scalar, constant
BLOCK
  TYPE(FEVariable_) :: obj
  INTEGER(I4B), ALLOCATABLE :: s(:)

  obj = QuadratureVariable(1.0_DFP, Scalar, Constant)
  s = SHAPE(obj)
  CALL OK(SIZE(s) .EQ. 1, "scalar constant: SIZE(shape) = 1")
  CALL OK(s(1) .EQ. 1, "scalar constant: shape = [1]")
END BLOCK

! Test 2: vector, space
BLOCK
  TYPE(FEVariable_) :: obj
  INTEGER(I4B), ALLOCATABLE :: s(:)

  obj = QuadratureVariable(nrow=3, ncol=4, rank=Vector, vartype=Space)
  s = SHAPE(obj)
  CALL OK(SIZE(s) .EQ. 2, "vector space: SIZE(shape) = 2")
  CALL OK(ALL(s .EQ. [3, 4]), "vector space: shape = [3, 4]")
END BLOCK

! Test 3: matrix, spaceTime
BLOCK
  TYPE(FEVariable_) :: obj
  INTEGER(I4B), ALLOCATABLE :: s(:)

  obj = QuadratureVariable(dim1=2, dim2=3, dim3=4, dim4=5, rank=Matrix, &
                           vartype=SpaceTime)
  s = SHAPE(obj)
  CALL OK(SIZE(s) .EQ. 4, "matrix spaceTime: SIZE(shape) = 4")
  CALL OK(ALL(s .EQ. [2, 3, 4, 5]), "matrix spaceTime: shape")
END BLOCK

END PROGRAM main
