!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: Mean of a FEVariable over space and time

PROGRAM main
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
USE BaseType, ONLY: Constant => TypeFEVariableConstant
USE BaseType, ONLY: Matrix => TypeFEVariableMatrix
USE BaseType, ONLY: Scalar => TypeFEVariableScalar
USE BaseType, ONLY: Space => TypeFEVariableSpace
USE BaseType, ONLY: SpaceTime => TypeFEVariableSpaceTime
USE BaseType, ONLY: Time => TypeFEVariableTime
USE BaseType, ONLY: Vector => TypeFEVariableVector
USE FEVariable_Method, ONLY: Get
USE FEVariable_Method, ONLY: MEAN
USE FEVariable_Method, ONLY: NodalVariable
USE FEVariable_Method, ONLY: QuadratureVariable
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
INTEGER(I4B) :: ii
REAL(DFP) :: v(12)

v = [(REAL(ii, DFP), ii=1, 12)]

! Test 1: mean of scalar variables as a real
BLOCK
  TYPE(FEVariable_) :: obj

  obj = QuadratureVariable(4.0_DFP, Scalar, Constant)
  CALL OK(ABS(MEAN(obj, Scalar) - 4.0_DFP) .LT. tol, "scalar constant")

  obj = QuadratureVariable(v(1:4), Scalar, Space)
  CALL OK(ABS(MEAN(obj, Scalar) - 2.5_DFP) .LT. tol, "scalar space")

  obj = QuadratureVariable(v(1:3), Scalar, Time)
  CALL OK(ABS(MEAN(obj, Scalar) - 2.0_DFP) .LT. tol, "scalar time")

  obj = QuadratureVariable(RESHAPE(v(1:6), [3, 2]), Scalar, SpaceTime)
  CALL OK(ABS(MEAN(obj, Scalar) - 3.5_DFP) .LT. tol, "scalar spaceTime")
END BLOCK

! Test 2: mean of vector variables, average over space and time
BLOCK
  TYPE(FEVariable_) :: obj
  REAL(DFP), ALLOCATABLE :: ans(:)

  obj = QuadratureVariable(v(1:3), Vector, Constant)
  ans = MEAN(obj, Vector)
  CALL OK(ALL(ABS(ans - v(1:3)) .LT. tol), "vector constant")

  ! columns [1, 2], [3, 4], [5, 6]: mean [3, 4]
  obj = QuadratureVariable(RESHAPE(v(1:6), [2, 3]), Vector, Space)
  ans = MEAN(obj, Vector)
  CALL OK(SIZE(ans) .EQ. 2, "vector space: SIZE = 2")
  CALL OK(ALL(ABS(ans - [3.0_DFP, 4.0_DFP]) .LT. tol), "vector space")

  obj = QuadratureVariable(RESHAPE(v(1:6), [2, 3]), Vector, Time)
  ans = MEAN(obj, Vector)
  CALL OK(ALL(ABS(ans - [3.0_DFP, 4.0_DFP]) .LT. tol), "vector time")

  ! 6 columns [1, 2], ..., [11, 12]: mean [6, 7]
  obj = QuadratureVariable(RESHAPE(v, [2, 3, 2]), Vector, SpaceTime)
  ans = MEAN(obj, Vector)
  CALL OK(ALL(ABS(ans - [6.0_DFP, 7.0_DFP]) .LT. tol), "vector spaceTime")
END BLOCK

! Test 3: mean of matrix variables
BLOCK
  TYPE(FEVariable_) :: obj
  REAL(DFP), ALLOCATABLE :: ans(:, :)

  obj = QuadratureVariable(RESHAPE(v(1:4), [2, 2]), Matrix, Constant)
  ans = MEAN(obj, Matrix)
  CALL OK(ALL(ABS(ans - RESHAPE(v(1:4), [2, 2])) .LT. tol), &
          "matrix constant")

  ! matrices [1 3; 2 4], [5 7; 6 8], [9 11; 10 12]: mean [5 7; 6 8]
  obj = QuadratureVariable(RESHAPE(v, [2, 2, 3]), Matrix, Space)
  ans = MEAN(obj, Matrix)
  CALL OK(ALL(ABS(ans - RESHAPE(v(5:8), [2, 2])) .LT. tol), &
          "matrix space")

  obj = QuadratureVariable(RESHAPE(v, [2, 2, 3]), Matrix, Time)
  ans = MEAN(obj, Matrix)
  CALL OK(ALL(ABS(ans - RESHAPE(v(5:8), [2, 2])) .LT. tol), &
          "matrix time")

  ! 3 space points and 1 time point
  obj = QuadratureVariable(RESHAPE(v, [2, 2, 3, 1]), Matrix, SpaceTime)
  ans = MEAN(obj, Matrix)
  CALL OK(ALL(ABS(ans - RESHAPE(v(5:8), [2, 2])) .LT. tol), &
          "matrix spaceTime")
END BLOCK

! Test 4: MEAN(obj) returns a constant FEVariable with the same rank
BLOCK
  TYPE(FEVariable_) :: obj, ans

  obj = NodalVariable(RESHAPE(v(1:6), [2, 3]), Vector, Space)
  ans = MEAN(obj)
  CALL OK(ans%rank .EQ. fevaropt%vector, "MEAN(obj): rank vector")
  CALL OK(ans%varType .EQ. fevaropt%constant, "MEAN(obj): constant")
  CALL OK(ans%defineOn .EQ. fevaropt%nodal, "MEAN(obj): nodal")
  CALL OK(ALL(ABS(Get(ans, Vector, Constant) - [3.0_DFP, 4.0_DFP]) &
              .LT. tol), "MEAN(obj): values")

  obj = QuadratureVariable(v(1:4), Scalar, Space)
  ans = MEAN(obj)
  CALL OK(ans%defineOn .EQ. fevaropt%quadrature, "MEAN(obj): quadrature")
  CALL OK(ABS(Get(ans, Scalar, Constant) - 2.5_DFP) .LT. tol, &
          "MEAN(obj): scalar value")
END BLOCK

END PROGRAM main
