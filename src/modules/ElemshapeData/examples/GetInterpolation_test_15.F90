!> author: OpenAI
! date: 2026-11-21
! summary: Defined outputs for unsupported vector interpolation

PROGRAM main
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: SpaceTime => TypeFEVariableSpaceTime
USE BaseType, ONLY: STElemShapeData_
USE BaseType, ONLY: Time => TypeFEVariableTime
USE BaseType, ONLY: Vector => TypeFEVariableVector
USE ElemShapeData_Method, ONLY: GetInterpolation
USE ElemShapeData_Method, ONLY: GetInterpolation_
USE FEVariable_Method, ONLY: NodalVariable
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
IMPLICIT NONE

TYPE(ElemShapeData_) :: obj
TYPE(STElemShapeData_) :: st(1)
TYPE(FEVariable_) :: timeVal, stVal

obj%nns = 1
obj%nips = 1
obj%N = RESHAPE([1.0_DFP], [1, 1])
st(1)%nns = 1
st(1)%nips = 1
st(1)%nnt = 1
st(1)%N = obj%N
st(1)%T = [1.0_DFP]
timeVal = NodalVariable([1.0_DFP, 2.0_DFP], Vector, Time, [2, 1])
stVal = NodalVariable([1.0_DFP, 2.0_DFP], Vector, SpaceTime, [2, 1, 1])

! Test 1: allocating calls return defined empty arrays
BLOCK
  REAL(DFP), ALLOCATABLE :: ans(:, :), ansST(:, :, :)

  CALL GetInterpolation(obj=obj, ans=ans, val=timeVal)
  CALL OK(ALL(SHAPE(ans) .EQ. [0, 0]), "time: empty allocated matrix")
  CALL GetInterpolation(obj=obj, ans=ans, val=stVal)
  CALL OK(ALL(SHAPE(ans) .EQ. [0, 0]), &
          "space-time with spatial data: empty allocated matrix")
  CALL GetInterpolation(obj=st, ans=ansST, val=timeVal)
  CALL OK(ALL(SHAPE(ansST) .EQ. [0, 0, 1]), &
          "time: empty allocated time slices")
END BLOCK

! Test 2: both unsupported combinations leave existing buffers untouched
CALL CheckNoOp(timeVal, "time")
CALL CheckNoOp(stVal, "space-time with spatial data")

! Test 3: time-point array wrappers propagate zero slice sizes
BLOCK
  REAL(DFP) :: ans(2, 1, 1)
  INTEGER(I4B) :: dim1, dim2, dim3

  ans = 7.0_DFP
  CALL GetInterpolation_(obj=st, ans=ans, val=timeVal, &
                         dim1=dim1, dim2=dim2, dim3=dim3)
  CALL OK(ALL([dim1, dim2, dim3] .EQ. [0, 0, 1]), &
          "time array: wrapper sizes")
  CALL OK(ALL(ans .EQ. 7.0_DFP), "time array: wrapper buffer")
  CALL GetInterpolation_(obj=st, ans=ans, val=timeVal, &
                         dim1=dim1, dim2=dim2, dim3=dim3, &
                         scale=2.0_DFP, addContribution=.TRUE.)
  CALL OK(ALL([dim1, dim2, dim3] .EQ. [0, 0, 1]), &
          "time array: scaled sizes")
  CALL OK(ALL(ans .EQ. 7.0_DFP), "time array: scaled buffer")
END BLOCK

CONTAINS

SUBROUTINE CheckNoOp(val, label)
  TYPE(FEVariable_), INTENT(IN) :: val
  !! unsupported vector variable
  CHARACTER(*), INTENT(IN) :: label
  !! prefix for test labels
  REAL(DFP) :: ans(2, 1), point(2)
  INTEGER(I4B) :: nrow, ncol, tsize, mode

  ans = 7.0_DFP
  point = 7.0_DFP
  nrow = -1
  ncol = -1
  CALL GetInterpolation_(obj=obj, ans=ans, val=val, &
                         nrow=nrow, ncol=ncol)
  CALL OK(nrow .EQ. 0 .AND. ncol .EQ. 0, label//": wrapper sizes")
  CALL OK(ALL(ans .EQ. 7.0_DFP), label//": wrapper buffer")
  DO mode = 0, 1
    nrow = -1
    ncol = -1
    tsize = -1
    CALL GetInterpolation_(obj=obj, ans=ans, val=val, &
                           nrow=nrow, ncol=ncol, scale=2.0_DFP, &
                           addContribution=(mode .EQ. 1))
    CALL OK(nrow .EQ. 0 .AND. ncol .EQ. 0, label//": scaled sizes")
    CALL OK(ALL(ans .EQ. 7.0_DFP), label//": scaled buffer")
    CALL GetInterpolation_(obj=obj, ans=point, val=val, tsize=tsize, &
                           scale=2.0_DFP, addContribution=(mode .EQ. 1), &
                           timeIndx=1, spaceIndx=1)
    CALL OK(tsize .EQ. 0, label//": point size")
    CALL OK(ALL(point .EQ. 7.0_DFP), label//": point buffer")
  END DO
END SUBROUTINE CheckNoOp
END PROGRAM main
