!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: Size of a FEVariable with and without dim

PROGRAM main
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: Constant => TypeFEVariableConstant
USE BaseType, ONLY: Matrix => TypeFEVariableMatrix
USE BaseType, ONLY: Scalar => TypeFEVariableScalar
USE BaseType, ONLY: SpaceTime => TypeFEVariableSpaceTime
USE BaseType, ONLY: Space => TypeFEVariableSpace
USE FEVariable_Method, ONLY: NodalVariable
USE FEVariable_Method, ONLY: SIZE
USE GlobalData, ONLY: DFP
USE Test_Method, ONLY: OK
IMPLICIT NONE

! Test 1: scalar, space
BLOCK
  TYPE(FEVariable_) :: obj

  obj = NodalVariable(tsize=4, rank=Scalar, vartype=Space)
  CALL OK(SIZE(obj) .EQ. 4, "scalar space: SIZE = len = 4")
  CALL OK(SIZE(obj, 1) .EQ. 4, "scalar space: SIZE(dim=1) = 4")
END BLOCK

! Test 2: matrix, spaceTime gives len without dim, s(dim) with dim
BLOCK
  TYPE(FEVariable_) :: obj

  obj = NodalVariable(dim1=2, dim2=3, dim3=4, dim4=5, rank=Matrix, &
                      vartype=SpaceTime)
  CALL OK(SIZE(obj) .EQ. 120, "matrix spaceTime: SIZE = len = 120")
  CALL OK(SIZE(obj, 1) .EQ. 2, "matrix spaceTime: SIZE(dim=1) = 2")
  CALL OK(SIZE(obj, 2) .EQ. 3, "matrix spaceTime: SIZE(dim=2) = 3")
  CALL OK(SIZE(obj, 3) .EQ. 4, "matrix spaceTime: SIZE(dim=3) = 4")
  CALL OK(SIZE(obj, 4) .EQ. 5, "matrix spaceTime: SIZE(dim=4) = 5")
END BLOCK

! Test 3: scalar, constant
BLOCK
  TYPE(FEVariable_) :: obj

  obj = NodalVariable(2.0_DFP, Scalar, Constant)
  CALL OK(SIZE(obj) .EQ. 1, "scalar constant: SIZE = 1")
END BLOCK

END PROGRAM main
