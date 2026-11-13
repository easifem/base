!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: Total shape of a FEVariable and of the result of two variables

PROGRAM main
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: Constant => TypeFEVariableConstant
USE BaseType, ONLY: Matrix => TypeFEVariableMatrix
USE BaseType, ONLY: Scalar => TypeFEVariableScalar
USE BaseType, ONLY: Space => TypeFEVariableSpace
USE BaseType, ONLY: SpaceTime => TypeFEVariableSpaceTime
USE BaseType, ONLY: Vector => TypeFEVariableVector
USE FEVariable_Method, ONLY: GetTotalShape
USE FEVariable_Method, ONLY: NodalVariable
USE GlobalData, ONLY: DFP
USE Test_Method, ONLY: OK
IMPLICIT NONE

! Test 1: total shape of one variable
BLOCK
  TYPE(FEVariable_) :: obj

  obj = NodalVariable(1.0_DFP, Scalar, Constant)
  CALL OK(GetTotalShape(obj) .EQ. 1, "scalar constant: 1")
  obj = NodalVariable(nrow=3, ncol=2, rank=Scalar, vartype=SpaceTime)
  CALL OK(GetTotalShape(obj) .EQ. 2, "scalar spaceTime: 2")
  obj = NodalVariable(nrow=3, ncol=4, rank=Vector, vartype=Space)
  CALL OK(GetTotalShape(obj) .EQ. 2, "vector space: 2")
  obj = NodalVariable(dim1=2, dim2=2, dim3=3, dim4=2, rank=Matrix, &
                      vartype=SpaceTime)
  CALL OK(GetTotalShape(obj) .EQ. 4, "matrix spaceTime: 4")
END BLOCK

! Test 2: total shape of the result of a binary operation
BLOCK
  ! scalar space (tshape 1) and scalar time (tshape 1): spaceTime
  CALL OK(GetTotalShape(rankCase=00, varCase=12, tshape1=1, &
                        tshape2=1) .EQ. 2, "scalar space * scalar time")
  ! scalar constant (1) and vector space (2)
  CALL OK(GetTotalShape(rankCase=01, varCase=01, tshape1=1, &
                        tshape2=2) .EQ. 2, "scalar constant * vector space")
  ! vector space (2) and scalar constant (1)
  CALL OK(GetTotalShape(rankCase=10, varCase=10, tshape1=2, &
                        tshape2=1) .EQ. 2, "vector space * scalar constant")
  ! scalar space (1) and matrix constant (2)
  CALL OK(GetTotalShape(rankCase=02, varCase=10, tshape1=1, &
                        tshape2=2) .EQ. 3, "scalar space * matrix constant")
  ! vector space (2) and vector time (2): vector spaceTime
  CALL OK(GetTotalShape(rankCase=11, varCase=12, tshape1=2, &
                        tshape2=2) .EQ. 3, "vector space * vector time")
  ! matrix time (3) and vector space (2): matrix spaceTime
  CALL OK(GetTotalShape(rankCase=21, varCase=21, tshape1=3, &
                        tshape2=2) .EQ. 4, "matrix time * vector space")
END BLOCK

END PROGRAM main
