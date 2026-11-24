!> author: OpenAI
! date: 2026-11-22
! summary: Local divergence FEVariable output for unsupported inputs

PROGRAM main
USE BaseType, ONLY: Constant => TypeFEVariableConstant
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
USE BaseType, ONLY: Matrix => TypeFEVariableMatrix
USE BaseType, ONLY: Scalar => TypeFEVariableScalar
USE BaseType, ONLY: SpaceTime => TypeFEVariableSpaceTime
USE BaseType, ONLY: Time => TypeFEVariableTime
USE BaseType, ONLY: Vector => TypeFEVariableVector
USE ElemShapeData_Method, ONLY: GetLocalDivergence
USE ElemShapeData_Method, ONLY: LocalDivergence
USE FEVariable_Method, ONLY: ASSIGNMENT(=)
USE FEVariable_Method, ONLY: NodalVariable
USE GlobalData, ONLY: DFP
USE Test_Method, ONLY: OK
IMPLICIT NONE

TYPE(ElemShapeData_) :: obj
TYPE(FEVariable_) :: vectorST, matrixST, vectorTime, matrixTime
TYPE(FEVariable_) :: lg, blank, ans

vectorST = NodalVariable(RESHAPE([1.0_DFP, 2.0_DFP], [1, 1, 2]), &
                         Vector, SpaceTime)
matrixST = NodalVariable(RESHAPE([1.0_DFP, 2.0_DFP], [1, 1, 1, 2]), &
                         Matrix, SpaceTime)
vectorTime = NodalVariable([1.0_DFP, 2.0_DFP], Vector, Time, [1, 2])
matrixTime = NodalVariable([1.0_DFP], Matrix, Time, [1, 1, 1])

! Test 1: a space-time val with a plain obj leaves lg unchanged
lg = NodalVariable(7.0_DFP, Scalar, Constant)
CALL GetLocalDivergence(obj=obj, lg=lg, val=vectorST)
CALL OK(Unchanged(lg), "space-time vector, plain obj: lg unchanged")
CALL GetLocalDivergence(obj=obj, lg=lg, val=matrixST)
CALL OK(Unchanged(lg), "space-time matrix, plain obj: lg unchanged")

! Test 2: a time-only val leaves lg unchanged
CALL GetLocalDivergence(obj=obj, lg=lg, val=vectorTime)
CALL OK(Unchanged(lg), "time vector: lg unchanged")
CALL GetLocalDivergence(obj=obj, lg=lg, val=matrixTime)
CALL OK(Unchanged(lg), "time matrix: lg unchanged")

! Test 3: the function form returns an uncomputed default FEVariable
ans = LocalDivergence(obj=obj, val=vectorST)
CALL OK(ans%len .EQ. blank%len, "function form, space-time vector")
ans = LocalDivergence(obj=obj, val=matrixTime)
CALL OK(ans%len .EQ. blank%len, "function form, time matrix")

CONTAINS

FUNCTION Unchanged(x) RESULT(ans)
  TYPE(FEVariable_), INTENT(IN) :: x
  !! output variable preset to the constant scalar 7
  LOGICAL :: ans
  !! true if x is still the preset value
  ans = x%len .EQ. 1 .AND. x%RANK .EQ. fevaropt%scalar .AND. &
        x%varType .EQ. fevaropt%constant .AND. x%val(1) .EQ. 7.0_DFP
END FUNCTION Unchanged
END PROGRAM main
