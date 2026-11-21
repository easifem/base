!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Division by the operator / with time and space-time operands

PROGRAM main
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
USE BaseType, ONLY: Constant => TypeFEVariableConstant
USE BaseType, ONLY: Scalar => TypeFEVariableScalar
USE BaseType, ONLY: Space => TypeFEVariableSpace
USE BaseType, ONLY: SpaceTime => TypeFEVariableSpaceTime
USE BaseType, ONLY: Time => TypeFEVariableTime
USE BaseType, ONLY: Vector => TypeFEVariableVector
USE FEVariable_Method, ONLY: OPERATOR(/)
USE FEVariable_Method, ONLY: QuadratureVariable
USE FEVariable_Method, ONLY: SHAPE
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP) :: a(3), b(2), st(3, 2), vt(2, 2), vs(2, 3)
INTEGER(I4B) :: ii, jj, kk

! a: 3 space points, b: 2 time points
a = [1.0_DFP, 2.0_DFP, 3.0_DFP]
b = [10.0_DFP, 20.0_DFP]
st = RESHAPE([(REAL(ii, DFP), ii=1, 6)], [3, 2])
vt = RESHAPE([(REAL(ii, DFP), ii=1, 4)], [2, 2])
vs = RESHAPE([(REAL(ii, DFP), ii=1, 6)], [2, 3])

! Test 1: scalar time / scalar space gives scalar spaceTime
BLOCK
  TYPE(FEVariable_) :: ans
  REAL(DFP) :: ref(3, 2)

  ans = QuadratureVariable(b, Scalar, Time) / &
        QuadratureVariable(a, Scalar, Space)
  DO kk = 1, 2
    ref(:, kk) = b(kk) / a
  END DO
  CALL OK(ans%varType .EQ. fevaropt%spacetime, "time / space: spaceTime")
  CALL OK(ALL(SHAPE(ans) .EQ. [3, 2]), "time / space: shape [3, 2]")
  CALL OK(ans%len .EQ. 6, "time / space: len = 6")
  CALL OK(ALL(ABS(ans%val(1:6) - RESHAPE(ref, [6])) .LT. tol), &
          "time / space: values")
END BLOCK

! Test 2: scalar spaceTime / scalar space and / scalar time
BLOCK
  TYPE(FEVariable_) :: ans
  REAL(DFP) :: ref(3, 2)

  ans = QuadratureVariable(st, Scalar, SpaceTime) / &
        QuadratureVariable(a, Scalar, Space)
  DO kk = 1, 2
    ref(:, kk) = st(:, kk) / a
  END DO
  CALL OK(ALL(SHAPE(ans) .EQ. [3, 2]), "spaceTime / space: shape")
  CALL OK(ALL(ABS(ans%val(1:6) - RESHAPE(ref, [6])) .LT. tol), &
          "spaceTime / space: values")

  ans = QuadratureVariable(st, Scalar, SpaceTime) / &
        QuadratureVariable(b, Scalar, Time)
  DO kk = 1, 2
    ref(:, kk) = st(:, kk) / b(kk)
  END DO
  CALL OK(ALL(SHAPE(ans) .EQ. [3, 2]), "spaceTime / time: shape")
  CALL OK(ALL(ABS(ans%val(1:6) - RESHAPE(ref, [6])) .LT. tol), &
          "spaceTime / time: values")
END BLOCK

! Test 3: scalar constant / scalar spaceTime
BLOCK
  TYPE(FEVariable_) :: ans

  ans = QuadratureVariable(2.0_DFP, Scalar, Constant) / &
        QuadratureVariable(st, Scalar, SpaceTime)
  CALL OK(ans%varType .EQ. fevaropt%spacetime, &
          "constant / spaceTime: spaceTime")
  CALL OK(ALL(ABS(ans%val(1:6) - RESHAPE(2.0_DFP / st, [6])) .LT. tol), &
          "constant / spaceTime: values")
END BLOCK

! Test 4: vector time / scalar space gives vector spaceTime
BLOCK
  TYPE(FEVariable_) :: ans
  REAL(DFP) :: ref(2, 3, 2)

  ans = QuadratureVariable(vt, Vector, Time) / &
        QuadratureVariable(a, Scalar, Space)
  DO kk = 1, 2
    DO jj = 1, 3
      ref(:, jj, kk) = vt(:, kk) / a(jj)
    END DO
  END DO
  CALL OK(ans%varType .EQ. fevaropt%spacetime, &
          "vector time / scalar space: spaceTime")
  CALL OK(ALL(SHAPE(ans) .EQ. [2, 3, 2]), &
          "vector time / scalar space: shape [2, 3, 2]")
  CALL OK(ALL(ABS(ans%val(1:12) - RESHAPE(ref, [12])) .LT. tol), &
          "vector time / scalar space: values")
END BLOCK

! Test 5: scalar time / vector space gives vector spaceTime
BLOCK
  TYPE(FEVariable_) :: ans
  REAL(DFP) :: ref(2, 3, 2)

  ans = QuadratureVariable(b, Scalar, Time) / &
        QuadratureVariable(vs, Vector, Space)
  DO kk = 1, 2
    ref(:, :, kk) = b(kk) / vs
  END DO
  CALL OK(ALL(SHAPE(ans) .EQ. [2, 3, 2]), &
          "scalar time / vector space: shape [2, 3, 2]")
  CALL OK(ALL(ABS(ans%val(1:12) - RESHAPE(ref, [12])) .LT. tol), &
          "scalar time / vector space: values")
END BLOCK

END PROGRAM main
