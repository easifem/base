!> author: OpenAI
! date: 2026-11-21
! summary: Defined scalar interpolation outputs for unsupported combinations

PROGRAM main
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: Scalar => TypeFEVariableScalar
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
timeVal = NodalVariable([1.0_DFP], Scalar, Time)
spaceTimeVal = NodalVariable(RESHAPE([1.0_DFP], [1, 1]), &
                             Scalar, SpaceTime)

! Test 1: allocating wrappers return empty results
BLOCK
  REAL(DFP), ALLOCATABLE :: ans(:), ansST(:, :)

  CALL GetInterpolation(obj=obj, ans=ans, val=timeVal)
  CALL OK(SIZE(ans) .EQ. 0, "time: empty allocated vector")
  CALL GetInterpolation(obj=obj, ans=ans, val=spaceTimeVal)
  CALL OK(SIZE(ans) .EQ. 0, "space-time with spatial data: empty vector")
  CALL GetInterpolation(obj=st, ans=ansST, val=timeVal)
  CALL OK(ALL(SHAPE(ansST) .EQ. [0, 1]), &
          "time: empty allocated time slices")
END BLOCK

! Test 2: unsupported combinations preserve output buffers
CALL CheckNoOp(timeVal)
CALL CheckNoOp(spaceTimeVal)

! Test 3: array wrappers propagate zero active row count
BLOCK
  REAL(DFP) :: ans(2, 1)
  INTEGER(I4B) :: nrow, ncol

  ans = 7.0_DFP
  CALL GetInterpolation_(obj=st, ans=ans, val=timeVal, &
                         nrow=nrow, ncol=ncol)
  CALL OK(nrow .EQ. 0 .AND. ncol .EQ. 1, "time array: wrapper sizes")
  CALL OK(ALL(ans .EQ. 7.0_DFP), "time array: wrapper buffer")
  CALL GetInterpolation_(obj=st, ans=ans, val=timeVal, &
                         nrow=nrow, ncol=ncol, scale=2.0_DFP, &
                         addContribution=.FALSE.)
  CALL OK(nrow .EQ. 0 .AND. ncol .EQ. 1, "time array: scaled sizes")
  CALL OK(ALL(ans .EQ. 7.0_DFP), "time array: scaled buffer")
END BLOCK

CONTAINS

SUBROUTINE CheckNoOp(val)
  TYPE(FEVariable_), INTENT(IN) :: val
  !! scalar FEVariable for an unsupported combination
  REAL(DFP) :: ans(2), point
  INTEGER(I4B) :: tsize, mode

  ans = 7.0_DFP
  point = 7.0_DFP
  tsize = -1
  CALL GetInterpolation_(obj=obj, ans=ans, val=val, tsize=tsize)
  CALL OK(tsize .EQ. 0, "unsupported scalar: wrapper size")
  CALL OK(ALL(ans .EQ. 7.0_DFP), "unsupported scalar: wrapper buffer")
  DO mode = 0, 1
    tsize = -1
    CALL GetInterpolation_(obj=obj, ans=ans, val=val, tsize=tsize, &
                           scale=2.0_DFP, addContribution=(mode .EQ. 1))
    CALL OK(tsize .EQ. 0, "unsupported scalar: scaled size")
    CALL OK(ALL(ans .EQ. 7.0_DFP), "unsupported scalar: scaled buffer")
    CALL GetInterpolation_(obj=obj, ans=point, val=val, &
                           scale=2.0_DFP, addContribution=(mode .EQ. 1), &
                           spaceIndx=1, timeIndx=1)
    CALL OK(point .EQ. 7.0_DFP, "unsupported scalar: point buffer")
  END DO
END SUBROUTINE CheckNoOp
END PROGRAM main
