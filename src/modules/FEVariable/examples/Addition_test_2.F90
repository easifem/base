!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Operator + with time, spaceTime and nodal operands

PROGRAM main
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
USE BaseType, ONLY: Constant => TypeFEVariableConstant
USE BaseType, ONLY: Scalar => TypeFEVariableScalar
USE BaseType, ONLY: Space => TypeFEVariableSpace
USE BaseType, ONLY: SpaceTime => TypeFEVariableSpaceTime
USE BaseType, ONLY: Time => TypeFEVariableTime
USE BaseType, ONLY: Vector => TypeFEVariableVector
USE FEVariable_Method, ONLY: Get
USE FEVariable_Method, ONLY: NodalVariable
USE FEVariable_Method, ONLY: OPERATOR(+)
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
vt = RESHAPE([100.0_DFP, 200.0_DFP, 300.0_DFP, 400.0_DFP], [2, 2])

! Test 1: scalar time + scalar space gives scalar spaceTime
! The result has shape [nns, nnt], the same as space + time.
BLOCK
  TYPE(FEVariable_) :: ans
  REAL(DFP) :: ref(3, 2)

  ans = QuadratureVariable(b, Scalar, Time) + &
        QuadratureVariable(a, Scalar, Space)
  DO jj = 1, 2
    ref(:, jj) = b(jj) + a
  END DO
  CALL OK(ans%varType .EQ. fevaropt%spacetime, "time + space: spaceTime")
  CALL OK(ans%len .EQ. 6, "time + space: len = 6")
  CALL OK(ALL(SHAPE(ans) .EQ. [3, 2]), "time + space: shape [3, 2]")
  CALL OK(ALL(ABS(ans%val(1:6) - RESHAPE(ref, [6])) .LT. tol), &
          "time + space: values")
END BLOCK

! Test 2: scalar spaceTime + scalar constant
BLOCK
  TYPE(FEVariable_) :: ans

  ans = QuadratureVariable(st, Scalar, SpaceTime) + &
        QuadratureVariable(0.5_DFP, Scalar, Constant)
  CALL OK(ans%varType .EQ. fevaropt%spacetime, &
          "spaceTime + constant: spaceTime")
  CALL OK(ALL(ABS(Get(ans, Scalar, SpaceTime) - (st + 0.5_DFP)) .LT. tol), &
          "spaceTime + constant: values")
END BLOCK

! Test 3: scalar spaceTime + scalar spaceTime
BLOCK
  TYPE(FEVariable_) :: ans

  ans = QuadratureVariable(st, Scalar, SpaceTime) + &
        QuadratureVariable(st, Scalar, SpaceTime)
  CALL OK(ALL(ABS(Get(ans, Scalar, SpaceTime) - 2.0_DFP * st) .LT. tol), &
          "spaceTime + spaceTime: values")
END BLOCK

! Test 4: scalar constant + vector space gives vector space
BLOCK
  TYPE(FEVariable_) :: ans

  ans = QuadratureVariable(2.0_DFP, Scalar, Constant) + &
        QuadratureVariable(vs, Vector, Space)
  CALL OK(ans%rank .EQ. fevaropt%vector, "constant + vector: vector")
  CALL OK(ALL(ABS(Get(ans, Vector, Space) - (vs + 2.0_DFP)) .LT. tol), &
          "scalar constant + vector space")
END BLOCK

! Test 5: vector time + vector space gives vector spaceTime
! Result entry (i, j, k) is vt(i, k) + vs(i, j).
BLOCK
  TYPE(FEVariable_) :: ans
  REAL(DFP) :: ref(2, 3, 2)

  ans = QuadratureVariable(vt, Vector, Time) + &
        QuadratureVariable(vs, Vector, Space)
  DO kk = 1, 2
    DO jj = 1, 3
      ref(:, jj, kk) = vt(:, kk) + vs(:, jj)
    END DO
  END DO
  CALL OK(ans%varType .EQ. fevaropt%spacetime, &
          "vector time + space: spaceTime")
  CALL OK(ALL(SHAPE(ans) .EQ. [2, 3, 2]), &
          "vector time + space: shape [2, 3, 2]")
  CALL OK(ALL(ABS(Get(ans, Vector, SpaceTime) - ref) .LT. tol), &
          "vector time + space: values")
END BLOCK

! Test 6: defineOn of the result is that of the first operand
BLOCK
  TYPE(FEVariable_) :: ans

  ans = NodalVariable(a, Scalar, Space) + &
        QuadratureVariable(a, Scalar, Space)
  CALL OK(ans%defineOn .EQ. fevaropt%nodal, "nodal + quadrature: nodal")
  CALL OK(ALL(ABS(Get(ans, Scalar, Space) - 2.0_DFP * a) .LT. tol), &
          "nodal + quadrature: values")
END BLOCK

END PROGRAM main
