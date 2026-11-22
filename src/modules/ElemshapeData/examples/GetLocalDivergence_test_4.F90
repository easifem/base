!> author: OpenAI
! date: 2026-11-21
! summary: Local divergence at all time points with an empty obj array

PROGRAM main
USE BaseType, ONLY: Constant => TypeFEVariableConstant
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
USE BaseType, ONLY: Matrix => TypeFEVariableMatrix
USE BaseType, ONLY: Scalar => TypeFEVariableScalar
USE BaseType, ONLY: SpaceTime => TypeFEVariableSpaceTime
USE BaseType, ONLY: STElemShapeData_
USE BaseType, ONLY: Vector => TypeFEVariableVector
USE ElemShapeData_Method, ONLY: GetLocalDivergence
USE ElemShapeData_Method, ONLY: LocalDivergence
USE FEVariable_Method, ONLY: ASSIGNMENT(=)
USE FEVariable_Method, ONLY: NodalVariable
USE GlobalData, ONLY: DFP
USE Test_Method, ONLY: OK
IMPLICIT NONE

TYPE(STElemShapeData_) :: obj(0)
TYPE(FEVariable_) :: vectorVal, matrixVal, lg, blank, ans
REAL(DFP) :: v(1, 1, 1), m(1, 1, 1, 1)

v = 1.0_DFP
m = 1.0_DFP
vectorVal = NodalVariable(v, Vector, SpaceTime)
matrixVal = NodalVariable(m, Matrix, SpaceTime)

! Test 1: an empty obj leaves lg unchanged for a vector val
lg = NodalVariable(7.0_DFP, Scalar, Constant)
CALL GetLocalDivergence(obj=obj, lg=lg, val=vectorVal)
CALL OK(lg%len .EQ. 1 .AND. lg%RANK .EQ. fevaropt%scalar .AND. &
        lg%val(1) .EQ. 7.0_DFP, "empty obj, vector val: lg unchanged")

! Test 2: an empty obj leaves lg unchanged for a matrix val
lg = NodalVariable(7.0_DFP, Scalar, Constant)
CALL GetLocalDivergence(obj=obj, lg=lg, val=matrixVal)
CALL OK(lg%len .EQ. 1 .AND. lg%RANK .EQ. fevaropt%scalar .AND. &
        lg%val(1) .EQ. 7.0_DFP, "empty obj, matrix val: lg unchanged")

! Test 3: the function forms return an uncomputed default FEVariable
ans = LocalDivergence(obj=obj, val=vectorVal)
CALL OK(ans%len .EQ. blank%len, "empty obj, function form, vector val")
ans = LocalDivergence(obj=obj, val=matrixVal)
CALL OK(ans%len .EQ. blank%len, "empty obj, function form, matrix val")
END PROGRAM main
