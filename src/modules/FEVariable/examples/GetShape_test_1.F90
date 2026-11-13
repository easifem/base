!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: Shape of a FEVariable and of the result of two variables

PROGRAM main
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: Matrix => TypeFEVariableMatrix
USE BaseType, ONLY: Space => TypeFEVariableSpace
USE BaseType, ONLY: SpaceTime => TypeFEVariableSpaceTime
USE BaseType, ONLY: Vector => TypeFEVariableVector
USE FEVariable_Method, ONLY: GetShape
USE FEVariable_Method, ONLY: QuadratureVariable
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
IMPLICIT NONE

! Test 1: shape of one variable without allocation
BLOCK
  TYPE(FEVariable_) :: obj
  INTEGER(I4B) :: s(4), tsize

  obj = QuadratureVariable(dim1=3, dim2=4, dim3=2, rank=Vector, &
                           vartype=SpaceTime)
  CALL GetShape(obj=obj, ans=s, tsize=tsize)
  CALL OK(tsize .EQ. 3, "vector spaceTime: tsize = 3")
  CALL OK(ALL(s(1:tsize) .EQ. [3, 4, 2]), "vector spaceTime: shape")

  obj = QuadratureVariable(dim1=2, dim2=2, dim3=5, rank=Matrix, &
                           vartype=Space)
  CALL GetShape(obj=obj, ans=s, tsize=tsize)
  CALL OK(tsize .EQ. 3, "matrix space: tsize = 3")
  CALL OK(ALL(s(1:tsize) .EQ. [2, 2, 5]), "matrix space: shape")
END BLOCK

! Test 2: shape of the result of a binary operation
BLOCK
  INTEGER(I4B) :: s(4), tsize

  ! scalar space [5] and scalar time [2]: [5, 2]
  CALL GetShape(rankCase=00, varCase=12, s1=[5], s2=[2], tshape1=1, &
                tshape2=1, ans=s, tsize=tsize)
  CALL OK(tsize .EQ. 2, "scalar space, scalar time: tsize")
  CALL OK(ALL(s(1:tsize) .EQ. [5, 2]), "scalar space, scalar time")

  ! scalar time [2] and scalar space [5]: [5, 2]
  CALL GetShape(rankCase=00, varCase=21, s1=[2], s2=[5], tshape1=1, &
                tshape2=1, ans=s, tsize=tsize)
  CALL OK(ALL(s(1:tsize) .EQ. [5, 2]), "scalar time, scalar space")

  ! scalar space [5] and vector constant [3]: [3, 5]
  CALL GetShape(rankCase=01, varCase=10, s1=[5], s2=[3], tshape1=1, &
                tshape2=1, ans=s, tsize=tsize)
  CALL OK(tsize .EQ. 2, "scalar space, vector constant: tsize")
  CALL OK(ALL(s(1:tsize) .EQ. [3, 5]), "scalar space, vector constant")

  ! vector space [3, 5] and vector time [3, 2]: [3, 5, 2]
  CALL GetShape(rankCase=11, varCase=12, s1=[3, 5], s2=[3, 2], &
                tshape1=2, tshape2=2, ans=s, tsize=tsize)
  CALL OK(tsize .EQ. 3, "vector space, vector time: tsize")
  CALL OK(ALL(s(1:tsize) .EQ. [3, 5, 2]), "vector space, vector time")

  ! scalar spaceTime [5, 2] and matrix constant [3, 3]: [3, 3, 5, 2]
  CALL GetShape(rankCase=02, varCase=30, s1=[5, 2], s2=[3, 3], &
                tshape1=2, tshape2=2, ans=s, tsize=tsize)
  CALL OK(tsize .EQ. 4, "scalar spaceTime, matrix constant: tsize")
  CALL OK(ALL(s(1:tsize) .EQ. [3, 3, 5, 2]), &
          "scalar spaceTime, matrix constant")

  ! matrix time [3, 3, 2] and vector space [3, 5]: [3, 3, 5, 2]
  CALL GetShape(rankCase=21, varCase=21, s1=[3, 3, 2], s2=[3, 5], &
                tshape1=3, tshape2=2, ans=s, tsize=tsize)
  CALL OK(tsize .EQ. 4, "matrix time, vector space: tsize")
  CALL OK(ALL(s(1:tsize) .EQ. [3, 3, 5, 2]), "matrix time, vector space")
END BLOCK

END PROGRAM main
