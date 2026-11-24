!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Mean of matrix FEVariables and MEAN(obj) for each rank

PROGRAM main
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
USE BaseType, ONLY: Constant => TypeFEVariableConstant
USE BaseType, ONLY: Matrix => TypeFEVariableMatrix
USE BaseType, ONLY: Scalar => TypeFEVariableScalar
USE BaseType, ONLY: Space => TypeFEVariableSpace
USE BaseType, ONLY: SpaceTime => TypeFEVariableSpaceTime
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
REAL(DFP) :: w(24)

w = [(REAL(ii, DFP), ii=1, 24)]

! Test 1: mean of matrix variables with several space and time points
BLOCK
  TYPE(FEVariable_) :: obj
  REAL(DFP), ALLOCATABLE :: ans(:, :)

  ! 2x3 matrices [1..6] and [7..12]: mean is [4..9]
  obj = QuadratureVariable(RESHAPE(w(1:12), [2, 3, 2]), Matrix, Space)
  ans = MEAN(obj, Matrix)
  CALL OK(ALL(SHAPE(ans) .EQ. [2, 3]), "matrix space: SHAPE = [2, 3]")
  CALL OK(ALL(ABS(ans - RESHAPE(w(4:9), [2, 3])) .LT. tol), &
          "matrix space: 2x3")

  ! 3 space points x 2 time points, 2x2 matrices [1..4], ..., [21..24]
  ! mean is [11..14]
  obj = QuadratureVariable(RESHAPE(w, [2, 2, 3, 2]), Matrix, SpaceTime)
  ans = MEAN(obj, Matrix)
  CALL OK(ALL(ABS(ans - RESHAPE(w(11:14), [2, 2])) .LT. tol), &
          "matrix spaceTime: 3 x 2 points")
END BLOCK

! Test 2: MEAN(obj) for matrix variables
BLOCK
  TYPE(FEVariable_) :: obj, ans

  ! matrices [1..4], [5..8], [9..12]: mean is [5..8]
  obj = NodalVariable(RESHAPE(w(1:12), [2, 2, 3]), Matrix, Space)
  ans = MEAN(obj)
  CALL OK(ans%rank .EQ. fevaropt%matrix, "MEAN(obj): rank matrix")
  CALL OK(ans%varType .EQ. fevaropt%constant, "MEAN(obj): matrix constant")
  CALL OK(ans%defineOn .EQ. fevaropt%nodal, "MEAN(obj): matrix nodal")
  CALL OK(ALL(ABS(Get(ans, Matrix, Constant) - RESHAPE(w(5:8), [2, 2])) &
              .LT. tol), "MEAN(obj): matrix nodal values")

  obj = QuadratureVariable(RESHAPE(w, [2, 2, 3, 2]), Matrix, SpaceTime)
  ans = MEAN(obj)
  CALL OK(ans%defineOn .EQ. fevaropt%quadrature, &
          "MEAN(obj): matrix quadrature")
  CALL OK(ALL(ABS(Get(ans, Matrix, Constant) - RESHAPE(w(11:14), [2, 2])) &
              .LT. tol), "MEAN(obj): matrix quadrature values")
END BLOCK

! Test 3: MEAN(obj) for nodal scalar and quadrature vector variables
BLOCK
  TYPE(FEVariable_) :: obj, ans

  obj = NodalVariable(RESHAPE(w(1:6), [3, 2]), Scalar, SpaceTime)
  ans = MEAN(obj)
  CALL OK(ans%rank .EQ. fevaropt%scalar, "MEAN(obj): rank scalar")
  CALL OK(ans%defineOn .EQ. fevaropt%nodal, "MEAN(obj): scalar nodal")
  CALL OK(ABS(Get(ans, Scalar, Constant) - 3.5_DFP) .LT. tol, &
          "MEAN(obj): scalar nodal value")

  ! 2 components, 3 space x 2 time points: mean is [6, 7]
  obj = QuadratureVariable(RESHAPE(w(1:12), [2, 3, 2]), Vector, SpaceTime)
  ans = MEAN(obj)
  CALL OK(ans%defineOn .EQ. fevaropt%quadrature, &
          "MEAN(obj): vector quadrature")
  CALL OK(ALL(ABS(Get(ans, Vector, Constant) - [6.0_DFP, 7.0_DFP]) &
              .LT. tol), "MEAN(obj): vector quadrature values")
END BLOCK

! Test 4: MEAN(obj) of a constant variable keeps its value
BLOCK
  TYPE(FEVariable_) :: obj, ans

  obj = QuadratureVariable(w(1:3), Vector, Constant)
  ans = MEAN(obj)
  CALL OK(ans%varType .EQ. fevaropt%constant, "MEAN(constant): constant")
  CALL OK(ALL(ABS(Get(ans, Vector, Constant) - w(1:3)) .LT. tol), &
          "MEAN(constant): same values")
END BLOCK

END PROGRAM main
