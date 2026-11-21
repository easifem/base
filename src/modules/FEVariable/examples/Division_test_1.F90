!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: Division of FEVariables by the operator /

PROGRAM main
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
USE BaseType, ONLY: Constant => TypeFEVariableConstant
USE BaseType, ONLY: Matrix => TypeFEVariableMatrix
USE BaseType, ONLY: Scalar => TypeFEVariableScalar
USE BaseType, ONLY: Space => TypeFEVariableSpace
USE BaseType, ONLY: Time => TypeFEVariableTime
USE BaseType, ONLY: Vector => TypeFEVariableVector
USE FEVariable_Method, ONLY: Get
USE FEVariable_Method, ONLY: OPERATOR(/)
USE FEVariable_Method, ONLY: QuadratureVariable
USE FEVariable_Method, ONLY: SHAPE
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP) :: a(3), b(2), c(3), v(2), m(2, 2)
REAL(DFP) :: vs(2, 3), ms(2, 2, 3)
INTEGER(I4B) :: ii, jj

a = [1.0_DFP, 2.0_DFP, 3.0_DFP]
b = [10.0_DFP, 20.0_DFP]
c = [100.0_DFP, 200.0_DFP, 300.0_DFP]
v = [0.5_DFP, 0.25_DFP]
m = RESHAPE([1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP], [2, 2])
vs = RESHAPE([(REAL(ii, DFP), ii=1, 6)], [2, 3])
ms = RESHAPE([(REAL(ii, DFP), ii=1, 12)], [2, 2, 3])

! Test 1: scalar constant / scalar constant
BLOCK
  TYPE(FEVariable_) :: ans

  ans = QuadratureVariable(2.0_DFP, Scalar, Constant) / &
        QuadratureVariable(4.0_DFP, Scalar, Constant)
  CALL OK(ABS(Get(ans, Scalar, Constant) - (2.0_DFP / 4.0_DFP)) &
          .LT. tol, "scalar constant / scalar constant")
  CALL OK(ans%varType .EQ. fevaropt%constant, "constant result")
  CALL OK(ans%defineOn .EQ. fevaropt%quadrature, "defineOn of obj1")
END BLOCK

! Test 2: scalar space / scalar space
BLOCK
  TYPE(FEVariable_) :: ans

  ans = QuadratureVariable(a, Scalar, Space) / &
        QuadratureVariable(c, Scalar, Space)
  CALL OK(ALL(ABS(Get(ans, Scalar, Space) - (a / c)) .LT. tol), &
          "scalar space / scalar space")
END BLOCK

! Test 3: scalar space / scalar time gives scalar spaceTime
BLOCK
  TYPE(FEVariable_) :: ans
  REAL(DFP) :: ref(3, 2)

  ans = QuadratureVariable(a, Scalar, Space) / &
        QuadratureVariable(b, Scalar, Time)
  DO jj = 1, 2
    ref(:, jj) = a / b(jj)
  END DO
  CALL OK(ans%varType .EQ. fevaropt%spacetime, "space / time: spaceTime")
  CALL OK(ALL(SHAPE(ans) .EQ. [3, 2]), "space / time: shape [3, 2]")
  CALL OK(ALL(ABS(ans%val(1:6) - RESHAPE(ref, [6])) .LT. tol), &
          "space / time: values")
END BLOCK

! Test 4: scalar space / vector constant gives vector space
BLOCK
  TYPE(FEVariable_) :: ans
  REAL(DFP) :: ref(2, 3)

  ans = QuadratureVariable(a, Scalar, Space) / &
        QuadratureVariable(v, Vector, Constant)
  DO jj = 1, 3
    ref(:, jj) = a(jj) / v
  END DO
  CALL OK(ans%rank .EQ. fevaropt%vector, "scalar / vector: vector")
  CALL OK(ALL(SHAPE(ans) .EQ. [2, 3]), "scalar / vector: shape [2, 3]")
  CALL OK(ALL(ABS(Get(ans, Vector, Space) - ref) .LT. tol), &
          "scalar space / vector constant")
END BLOCK

! Test 5: vector space / scalar space, vector space / vector space
BLOCK
  TYPE(FEVariable_) :: ans
  REAL(DFP) :: ref(2, 3)

  ans = QuadratureVariable(vs, Vector, Space) / &
        QuadratureVariable(a, Scalar, Space)
  DO jj = 1, 3
    ref(:, jj) = vs(:, jj) / a(jj)
  END DO
  CALL OK(ALL(ABS(Get(ans, Vector, Space) - ref) .LT. tol), &
          "vector space / scalar space")

  ans = QuadratureVariable(vs, Vector, Space) / &
        QuadratureVariable(vs + 1.0_DFP, Vector, Space)
  CALL OK(ALL(ABS(Get(ans, Vector, Space) - (vs / (vs + 1.0_DFP))) &
              .LT. tol), "vector space / vector space")
END BLOCK

! Test 6: matrix with scalar, matrix with matrix (element-wise)
BLOCK
  TYPE(FEVariable_) :: ans
  REAL(DFP) :: ref(2, 2, 3)

  ans = QuadratureVariable(m, Matrix, Constant) / &
        QuadratureVariable(2.0_DFP, Scalar, Constant)
  CALL OK(ALL(ABS(Get(ans, Matrix, Constant) - (m / 2.0_DFP)) &
              .LT. tol), "matrix constant / scalar constant")

  ans = QuadratureVariable(a, Scalar, Space) / &
        QuadratureVariable(m, Matrix, Constant)
  DO jj = 1, 3
    ref(:, :, jj) = a(jj) / m
  END DO
  CALL OK(ALL(SHAPE(ans) .EQ. [2, 2, 3]), "scalar / matrix: shape")
  CALL OK(ALL(ABS(Get(ans, Matrix, Space) - ref) .LT. tol), &
          "scalar space / matrix constant")

  ans = QuadratureVariable(ms, Matrix, Space) / &
        QuadratureVariable(ms + 1.0_DFP, Matrix, Space)
  CALL OK(ALL(ABS(Get(ans, Matrix, Space) - (ms / (ms + 1.0_DFP))) &
              .LT. tol), "matrix space / matrix space")
END BLOCK

! Test 7: FEVariable / real and real / FEVariable
BLOCK
  TYPE(FEVariable_) :: ans

  ans = QuadratureVariable(a, Scalar, Space) / 2.0_DFP
  CALL OK(ALL(ABS(Get(ans, Scalar, Space) - (a / 2.0_DFP)) .LT. tol), &
          "obj / real")
  ans = 2.0_DFP / QuadratureVariable(vs, Vector, Space)
  CALL OK(ALL(ABS(Get(ans, Vector, Space) - (2.0_DFP / vs)) .LT. tol), &
          "real / obj")
  CALL OK(ALL(SHAPE(ans) .EQ. [2, 3]), "real / obj: shape kept")
END BLOCK

END PROGRAM main
