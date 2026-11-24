!> author: OpenAI
! date: 2026-11-21
! summary: Empty scalar space-time interpolation inputs

PROGRAM main
USE BaseType, ONLY: Constant => TypeFEVariableConstant
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: Scalar => TypeFEVariableScalar
USE BaseType, ONLY: STElemShapeData_
USE ElemShapeData_Method, ONLY: GetInterpolation
USE ElemShapeData_Method, ONLY: GetInterpolation_
USE FEVariable_Method, ONLY: ASSIGNMENT(=)
USE FEVariable_Method, ONLY: NodalVariable
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
IMPLICIT NONE

TYPE(STElemShapeData_) :: empty(0), obj
TYPE(FEVariable_) :: field
REAL(DFP) :: val(1, 1)
INTEGER(I4B) :: scenario, mode

val = 1.0_DFP
field = NodalVariable(1.0_DFP, Scalar, Constant)

! Test 1: allocating calls return a defined empty shape
BLOCK
  REAL(DFP), ALLOCATABLE :: ans(:, :)

  ALLOCATE (ans(2, 3))
  CALL GetInterpolation(obj=empty, ans=ans, val=val)
  CALL OK(ALL(SHAPE(ans) .EQ. [0, 0]), "empty raw: allocated shape")
  CALL GetInterpolation(obj=empty, ans=ans, val=field)
  CALL OK(ALL(SHAPE(ans) .EQ. [0, 0]), &
          "empty FEVariable: allocated shape")
END BLOCK

! Test 2: empty object arrays leave nonallocating buffers untouched
BLOCK
  REAL(DFP) :: ans(2, 3)
  INTEGER(I4B) :: nrow, ncol

  ans = 7.0_DFP
  CALL GetInterpolation_(obj=empty, ans=ans, val=val, &
                         nrow=nrow, ncol=ncol)
  CALL OK(nrow .EQ. 0 .AND. ncol .EQ. 0, "empty raw: wrapper sizes")
  CALL OK(ALL(ans .EQ. 7.0_DFP), "empty raw: wrapper buffer")
  CALL GetInterpolation_(obj=empty, ans=ans, val=field, &
                         nrow=nrow, ncol=ncol)
  CALL OK(nrow .EQ. 0 .AND. ncol .EQ. 0, &
          "empty FEVariable: wrapper sizes")
  CALL OK(ALL(ans .EQ. 7.0_DFP), "empty FEVariable: wrapper buffer")
  DO mode = 0, 1
    CALL GetInterpolation_(obj=empty, ans=ans, val=val, &
                           nrow=nrow, ncol=ncol, scale=2.0_DFP, &
                           addContribution=(mode .EQ. 1))
    CALL OK(nrow .EQ. 0 .AND. ncol .EQ. 0, "empty raw: scaled sizes")
    CALL OK(ALL(ans .EQ. 7.0_DFP), "empty raw: scaled buffer")
    CALL GetInterpolation_(obj=empty, ans=ans, val=field, &
                           nrow=nrow, ncol=ncol, scale=2.0_DFP, &
                           addContribution=(mode .EQ. 1))
    CALL OK(nrow .EQ. 0 .AND. ncol .EQ. 0, &
            "empty FEVariable: scaled sizes")
    CALL OK(ALL(ans .EQ. 7.0_DFP), "empty FEVariable: scaled buffer")
  END DO
END BLOCK

! Test 3: either input can limit the contributing time nodes to zero
obj%nips = 2
obj%nns = 1
DO scenario = 1, 2
  BLOCK
    REAL(DFP), ALLOCATABLE :: values(:, :), allocatedAns(:)
    REAL(DFP) :: ans(4)
    INTEGER(I4B) :: tsize

    IF (scenario .EQ. 1) THEN
      obj%nnt = 1
      ALLOCATE (values(1, 0))
    ELSE
      obj%nnt = 0
      ALLOCATE (values(1, 1))
      values = 1.0_DFP
    END IF
    CALL GetInterpolation(obj=obj, ans=allocatedAns, val=values)
    CALL OK(SIZE(allocatedAns) .EQ. 2, "no time nodes: allocated size")
    CALL OK(ALL(allocatedAns .EQ. 0.0_DFP), &
            "no time nodes: allocated result is zero")
    ans = 7.0_DFP
    CALL GetInterpolation_(obj=obj, ans=ans, val=values, tsize=tsize)
    CALL OK(tsize .EQ. 2, "no time nodes: wrapper active size")
    CALL OK(ALL(ans(1:2) .EQ. 0.0_DFP), "no time nodes: clear output")
    CALL OK(ALL(ans(3:4) .EQ. 7.0_DFP), "no time nodes: preserve tail")
    DO mode = 0, 1
      ans = 7.0_DFP
      CALL GetInterpolation_(obj=obj, ans=ans, val=values, tsize=tsize, &
                             scale=2.0_DFP, addContribution=(mode .EQ. 1))
      CALL OK(tsize .EQ. 2, "no time nodes: scaled active size")
      IF (mode .EQ. 0) THEN
        CALL OK(ALL(ans(1:2) .EQ. 0.0_DFP), &
                "no time nodes: replacement clears active output")
      ELSE
        CALL OK(ALL(ans(1:2) .EQ. 7.0_DFP), &
                "no time nodes: accumulation preserves active output")
      END IF
      CALL OK(ALL(ans(3:4) .EQ. 7.0_DFP), "no time nodes: scaled tail")
    END DO
  END BLOCK
END DO
END PROGRAM main
