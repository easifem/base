!> author: OpenAI
! date: 2026-11-21
! summary: Empty time-point arrays in vector interpolation

PROGRAM main
USE BaseType, ONLY: Constant => TypeFEVariableConstant
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: STElemShapeData_
USE BaseType, ONLY: Vector => TypeFEVariableVector
USE ElemShapeData_Method, ONLY: GetInterpolation
USE ElemShapeData_Method, ONLY: GetInterpolation_
USE FEVariable_Method, ONLY: NodalVariable
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
IMPLICIT NONE

TYPE(STElemShapeData_) :: obj(0)
TYPE(FEVariable_) :: field
REAL(DFP) :: val(2, 1, 1)

val = 1.0_DFP
field = NodalVariable([1.0_DFP, 2.0_DFP], Vector, Constant)

! Test 1: allocating calls preserve ncomp with no time points
BLOCK
  REAL(DFP), ALLOCATABLE :: ans(:, :, :)

  ALLOCATE (ans(3, 3, 3))
  CALL GetInterpolation(obj=obj, ans=ans, val=val)
  CALL OK(ALL(SHAPE(ans) .EQ. [2, 0, 0]), &
          "empty raw vector: allocated shape")
  CALL GetInterpolation(obj=obj, ans=ans, val=field)
  CALL OK(ALL(SHAPE(ans) .EQ. [2, 0, 0]), &
          "empty FEVariable vector: allocated shape")
END BLOCK

! Test 2: nonallocating raw calls return zero sizes and leave ans alone
BLOCK
  REAL(DFP) :: ans(2, 2, 2)
  INTEGER(I4B) :: dim1, dim2, dim3

  ans = 7.0_DFP
  CALL GetInterpolation_(obj=obj, ans=ans, val=val, &
                         dim1=dim1, dim2=dim2, dim3=dim3)
  CALL OK(ALL([dim1, dim2, dim3] .EQ. 0), &
          "empty raw vector: unscaled sizes")
  CALL OK(ALL(ans .EQ. 7.0_DFP), "empty raw vector: unscaled buffer")
  CALL GetInterpolation_(obj=obj, ans=ans, val=val, &
                         dim1=dim1, dim2=dim2, dim3=dim3, &
                         scale=2.0_DFP, addContribution=.FALSE.)
  CALL OK(ALL([dim1, dim2, dim3] .EQ. 0), &
          "empty raw vector: scaled sizes")
  CALL OK(ALL(ans .EQ. 7.0_DFP), "empty raw vector: scaled buffer")
  CALL GetInterpolation_(obj=obj, ans=ans, val=val, &
                         dim1=dim1, dim2=dim2, dim3=dim3, &
                         scale=2.0_DFP, addContribution=.TRUE.)
  CALL OK(ALL([dim1, dim2, dim3] .EQ. 0), &
          "empty raw vector: additive sizes")
  CALL OK(ALL(ans .EQ. 7.0_DFP), "empty raw vector: additive buffer")
END BLOCK

! Test 3: nonallocating FEVariable calls follow the same empty convention
BLOCK
  REAL(DFP) :: ans(2, 2, 2)
  INTEGER(I4B) :: dim1, dim2, dim3

  ans = 7.0_DFP
  CALL GetInterpolation_(obj=obj, ans=ans, val=field, &
                         dim1=dim1, dim2=dim2, dim3=dim3)
  CALL OK(ALL([dim1, dim2, dim3] .EQ. 0), &
          "empty FEVariable vector: unscaled sizes")
  CALL OK(ALL(ans .EQ. 7.0_DFP), &
          "empty FEVariable vector: unscaled buffer")
  CALL GetInterpolation_(obj=obj, ans=ans, val=field, &
                         dim1=dim1, dim2=dim2, dim3=dim3, &
                         scale=2.0_DFP, addContribution=.FALSE.)
  CALL OK(ALL([dim1, dim2, dim3] .EQ. 0), &
          "empty FEVariable vector: scaled sizes")
  CALL OK(ALL(ans .EQ. 7.0_DFP), &
          "empty FEVariable vector: scaled buffer")
  CALL GetInterpolation_(obj=obj, ans=ans, val=field, &
                         dim1=dim1, dim2=dim2, dim3=dim3, &
                         scale=2.0_DFP, addContribution=.TRUE.)
  CALL OK(ALL([dim1, dim2, dim3] .EQ. 0), &
          "empty FEVariable vector: additive sizes")
  CALL OK(ALL(ans .EQ. 7.0_DFP), &
          "empty FEVariable vector: additive buffer")
END BLOCK
END PROGRAM main
