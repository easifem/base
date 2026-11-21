!> author: OpenAI
! date: 2026-11-21
! summary: Defined outputs for unsupported matrix interpolation

PROGRAM main
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: Matrix => TypeFEVariableMatrix
USE BaseType, ONLY: SpaceTime => TypeFEVariableSpaceTime
USE BaseType, ONLY: STElemShapeData_
USE BaseType, ONLY: Time => TypeFEVariableTime
USE ElemShapeData_Method, ONLY: GetInterpolation
USE ElemShapeData_Method, ONLY: GetInterpolation_
USE FEVariable_Method, ONLY: ASSIGNMENT(=)
USE FEVariable_Method, ONLY: NodalVariable
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
IMPLICIT NONE

TYPE(ElemShapeData_) :: obj
TYPE(STElemShapeData_) :: st(1)
TYPE(FEVariable_) :: timeVal, spaceTimeVal

obj%nns = 1
obj%nips = 1
obj%N = RESHAPE([1.0_DFP], [1, 1])
st(1)%nns = 1
st(1)%nips = 1
st(1)%nnt = 1
st(1)%N = obj%N
st(1)%T = [1.0_DFP]
timeVal = NodalVariable(RESHAPE([1.0_DFP], [1, 1, 1]), Matrix, Time)
spaceTimeVal = NodalVariable(RESHAPE([1.0_DFP], [1, 1, 1, 1]), &
                             Matrix, SpaceTime)

! Test 1: allocating wrappers return empty outputs
BLOCK
  REAL(DFP), ALLOCATABLE :: ans(:, :, :), ansST(:, :, :, :)

  CALL GetInterpolation(obj=obj, ans=ans, val=timeVal)
  CALL OK(ALL(SHAPE(ans) .EQ. [0, 0, 0]), "time: empty allocated matrix")
  CALL GetInterpolation(obj=obj, ans=ans, val=spaceTimeVal)
  CALL OK(ALL(SHAPE(ans) .EQ. [0, 0, 0]), &
          "space-time with spatial data: empty matrix")
  CALL GetInterpolation(obj=st, ans=ansST, val=timeVal)
  CALL OK(ALL(SHAPE(ansST) .EQ. [0, 0, 0, 1]), &
          "time: empty allocated time slices")
END BLOCK

! Test 2: unsupported combinations return zero sizes and preserve buffers
CALL CheckNoOp(timeVal)
CALL CheckNoOp(spaceTimeVal)

! Test 3: array wrappers propagate zero active matrix sizes
BLOCK
  REAL(DFP) :: ans(2, 2, 2, 1)
  INTEGER(I4B) :: d1, d2, d3, d4

  ans = 7.0_DFP
  CALL GetInterpolation_(obj=st, ans=ans, val=timeVal, &
                         dim1=d1, dim2=d2, dim3=d3, dim4=d4)
  CALL OK(ALL([d1, d2, d3, d4] .EQ. [0, 0, 0, 1]), &
          "time array: wrapper sizes")
  CALL OK(ALL(ans .EQ. 7.0_DFP), "time array: wrapper buffer")
  CALL GetInterpolation_(obj=st, ans=ans, val=timeVal, &
                         dim1=d1, dim2=d2, dim3=d3, dim4=d4, &
                         scale=2.0_DFP, addContribution=.FALSE.)
  CALL OK(ALL([d1, d2, d3, d4] .EQ. [0, 0, 0, 1]), &
          "time array: scaled sizes")
  CALL OK(ALL(ans .EQ. 7.0_DFP), "time array: scaled buffer")
END BLOCK

CONTAINS

SUBROUTINE CheckNoOp(val)
  TYPE(FEVariable_), INTENT(IN) :: val
  !! matrix FEVariable for an unsupported combination
  REAL(DFP) :: ans(2, 2, 2), point(2, 2)
  INTEGER(I4B) :: d1, d2, d3, mode

  ans = 7.0_DFP
  point = 7.0_DFP
  d1 = -1
  d2 = -1
  d3 = -1
  CALL GetInterpolation_(obj=obj, ans=ans, val=val, &
                         dim1=d1, dim2=d2, dim3=d3)
  CALL OK(ALL([d1, d2, d3] .EQ. 0), "unsupported matrix: wrapper sizes")
  CALL OK(ALL(ans .EQ. 7.0_DFP), "unsupported matrix: wrapper buffer")
  DO mode = 0, 1
    d1 = -1
    d2 = -1
    d3 = -1
    CALL GetInterpolation_(obj=obj, ans=ans, val=val, &
                           dim1=d1, dim2=d2, dim3=d3, scale=2.0_DFP, &
                           addContribution=(mode .EQ. 1))
    CALL OK(ALL([d1, d2, d3] .EQ. 0), "unsupported matrix: scaled sizes")
    CALL OK(ALL(ans .EQ. 7.0_DFP), "unsupported matrix: scaled buffer")
    d1 = -1
    d2 = -1
    CALL GetInterpolation_(obj=obj, ans=point, val=val, nrow=d1, ncol=d2, &
                           scale=2.0_DFP, addContribution=(mode .EQ. 1), &
                           spaceIndx=1, timeIndx=1)
    CALL OK(ALL([d1, d2] .EQ. 0), "unsupported matrix: point sizes")
    CALL OK(ALL(point .EQ. 7.0_DFP), "unsupported matrix: point buffer")
  END DO
END SUBROUTINE CheckNoOp
END PROGRAM main
