!> author: OpenAI
! date: 2026-11-21
! summary: Matrix interpolation with an empty time-point array

PROGRAM main
USE BaseType, ONLY: Constant => TypeFEVariableConstant
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: Matrix => TypeFEVariableMatrix
USE BaseType, ONLY: STElemShapeData_
USE ElemShapeData_Method, ONLY: GetInterpolation
USE ElemShapeData_Method, ONLY: GetInterpolation_
USE FEVariable_Method, ONLY: ASSIGNMENT(=)
USE FEVariable_Method, ONLY: NodalVariable
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
IMPLICIT NONE

TYPE(STElemShapeData_) :: obj(0)
TYPE(FEVariable_) :: field
REAL(DFP) :: val(2, 3, 1, 1), matrixValues(2, 3)
INTEGER(I4B) :: mode

val = 1.0_DFP
matrixValues = 1.0_DFP
field = NodalVariable(matrixValues, Matrix, Constant)

! Test 1: allocation retains matrix dimensions but has no quadrature points
BLOCK
  REAL(DFP), ALLOCATABLE :: ans(:, :, :, :), fieldAns(:, :, :, :)

  CALL GetInterpolation(obj=obj, ans=ans, val=val)
  CALL OK(ALL(SHAPE(ans) .EQ. [2, 3, 0, 0]), &
          "empty raw matrix: fresh allocation shape")
  CALL GetInterpolation(obj=obj, ans=fieldAns, val=field)
  CALL OK(ALL(SHAPE(fieldAns) .EQ. [2, 3, 0, 0]), &
          "empty FEVariable matrix: fresh allocation shape")
END BLOCK

! Test 2: existing allocations resize to the empty output shape
BLOCK
  REAL(DFP), ALLOCATABLE :: ans(:, :, :, :)

  ALLOCATE (ans(3, 4, 2, 2))
  CALL GetInterpolation(obj=obj, ans=ans, val=val)
  CALL OK(ALL(SHAPE(ans) .EQ. [2, 3, 0, 0]), &
          "empty raw matrix: reallocation shape")
  CALL GetInterpolation(obj=obj, ans=ans, val=field)
  CALL OK(ALL(SHAPE(ans) .EQ. [2, 3, 0, 0]), &
          "empty FEVariable matrix: reallocation shape")
END BLOCK

! Test 3: nonallocating calls return zero active sizes and leave ans alone
BLOCK
  REAL(DFP) :: ans(3, 4, 2, 2)
  INTEGER(I4B) :: dim1, dim2, dim3, dim4

  ans = 7.0_DFP
  CALL GetInterpolation_(obj=obj, ans=ans, val=val, dim1=dim1, &
                         dim2=dim2, dim3=dim3, dim4=dim4)
  CALL OK(ALL([dim1, dim2, dim3, dim4] .EQ. 0), &
          "empty raw matrix: wrapper sizes")
  CALL OK(ALL(ans .EQ. 7.0_DFP), "empty raw matrix: wrapper buffer")
  CALL GetInterpolation_(obj=obj, ans=ans, val=field, dim1=dim1, &
                         dim2=dim2, dim3=dim3, dim4=dim4)
  CALL OK(ALL([dim1, dim2, dim3, dim4] .EQ. 0), &
          "empty FEVariable matrix: wrapper sizes")
  CALL OK(ALL(ans .EQ. 7.0_DFP), "empty FEVariable: wrapper buffer")
  DO mode = 0, 1
    CALL GetInterpolation_(obj=obj, ans=ans, val=val, dim1=dim1, &
                           dim2=dim2, dim3=dim3, dim4=dim4, &
                           scale=2.0_DFP, addContribution=(mode .EQ. 1))
    CALL OK(ALL([dim1, dim2, dim3, dim4] .EQ. 0), &
            "empty raw matrix: scaled sizes")
    CALL OK(ALL(ans .EQ. 7.0_DFP), "empty raw matrix: scaled buffer")
    CALL GetInterpolation_(obj=obj, ans=ans, val=field, dim1=dim1, &
                           dim2=dim2, dim3=dim3, dim4=dim4, &
                           scale=2.0_DFP, addContribution=(mode .EQ. 1))
    CALL OK(ALL([dim1, dim2, dim3, dim4] .EQ. 0), &
            "empty FEVariable matrix: scaled sizes")
    CALL OK(ALL(ans .EQ. 7.0_DFP), "empty FEVariable: scaled buffer")
  END DO
END BLOCK
END PROGRAM main
