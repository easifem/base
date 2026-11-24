!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Operator * for time and space-time FEVariables

PROGRAM main
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
USE BaseType, ONLY: Scalar => TypeFEVariableScalar
USE BaseType, ONLY: Space => TypeFEVariableSpace
USE BaseType, ONLY: SpaceTime => TypeFEVariableSpaceTime
USE BaseType, ONLY: Time => TypeFEVariableTime
USE BaseType, ONLY: Vector => TypeFEVariableVector
USE FEVariable_Method, ONLY: OPERATOR(*)
USE FEVariable_Method, ONLY: QuadratureVariable
USE FEVariable_Method, ONLY: SHAPE
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP) :: a(3), b(2), st(3, 2), vs(2, 3), vt(2, 2)
INTEGER(I4B) :: ii, jj, kk

a = [1.0_DFP, 2.0_DFP, 3.0_DFP]
b = [10.0_DFP, 20.0_DFP]
st = RESHAPE([(REAL(ii, DFP), ii=1, 6)], [3, 2])
vs = RESHAPE([(REAL(ii, DFP), ii=1, 6)], [2, 3])
vt = RESHAPE([0.5_DFP, 0.25_DFP, 2.0_DFP, 4.0_DFP], [2, 2])

! Test 1: scalar time * scalar space gives scalar spaceTime
BLOCK
  TYPE(FEVariable_) :: ans
  REAL(DFP) :: ref(3, 2)

  ans = QuadratureVariable(b, Scalar, Time) * &
        QuadratureVariable(a, Scalar, Space)
  DO kk = 1, 2
    ref(:, kk) = b(kk) * a
  END DO
  CALL OK(ans%varType .EQ. fevaropt%spacetime, "time * space: spaceTime")
  CALL OK(ans%len .EQ. 6, "time * space: len = 6")
  CALL OK(ALL(SHAPE(ans) .EQ. [3, 2]), "time * space: shape [3, 2]")
  CALL OK(ALL(ABS(ans%val(1:6) - RESHAPE(ref, [6])) .LT. tol), &
          "time * space: values")
END BLOCK

! Test 2: scalar spaceTime * scalar spaceTime
BLOCK
  TYPE(FEVariable_) :: ans

  ans = QuadratureVariable(st, Scalar, SpaceTime) * &
        QuadratureVariable(st + 1.0_DFP, Scalar, SpaceTime)
  CALL OK(ans%varType .EQ. fevaropt%spacetime, &
          "spaceTime * spaceTime: spaceTime")
  CALL OK(ALL(SHAPE(ans) .EQ. [3, 2]), &
          "spaceTime * spaceTime: shape [3, 2]")
  CALL OK(ALL(ABS(ans%val(1:6) - RESHAPE(st * (st + 1.0_DFP), [6])) &
              .LT. tol), "spaceTime * spaceTime: values")
END BLOCK

! Test 3: scalar space * scalar spaceTime, scalar spaceTime * scalar time
BLOCK
  TYPE(FEVariable_) :: ans
  REAL(DFP) :: ref(3, 2)

  ans = QuadratureVariable(a, Scalar, Space) * &
        QuadratureVariable(st, Scalar, SpaceTime)
  DO kk = 1, 2
    ref(:, kk) = a * st(:, kk)
  END DO
  CALL OK(ALL(SHAPE(ans) .EQ. [3, 2]), "space * spaceTime: shape")
  CALL OK(ALL(ABS(ans%val(1:6) - RESHAPE(ref, [6])) .LT. tol), &
          "space * spaceTime: values")

  ans = QuadratureVariable(st, Scalar, SpaceTime) * &
        QuadratureVariable(b, Scalar, Time)
  DO kk = 1, 2
    ref(:, kk) = st(:, kk) * b(kk)
  END DO
  CALL OK(ALL(SHAPE(ans) .EQ. [3, 2]), "spaceTime * time: shape")
  CALL OK(ALL(ABS(ans%val(1:6) - RESHAPE(ref, [6])) .LT. tol), &
          "spaceTime * time: values")
END BLOCK

! Test 4: vector space * vector time gives vector spaceTime
BLOCK
  TYPE(FEVariable_) :: ans
  REAL(DFP) :: ref(2, 3, 2)

  ans = QuadratureVariable(vs, Vector, Space) * &
        QuadratureVariable(vt, Vector, Time)
  DO kk = 1, 2
    DO jj = 1, 3
      ref(:, jj, kk) = vs(:, jj) * vt(:, kk)
    END DO
  END DO
  CALL OK(ans%varType .EQ. fevaropt%spacetime, &
          "vector space * vector time: spaceTime")
  CALL OK(ALL(SHAPE(ans) .EQ. [2, 3, 2]), &
          "vector space * vector time: shape [2, 3, 2]")
  CALL OK(ALL(ABS(ans%val(1:12) - RESHAPE(ref, [12])) .LT. tol), &
          "vector space * vector time: values")
END BLOCK

END PROGRAM main
