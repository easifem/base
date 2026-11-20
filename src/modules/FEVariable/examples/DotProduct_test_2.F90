!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Dot product of time and space-time vector FEVariables

PROGRAM main
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
USE BaseType, ONLY: Constant => TypeFEVariableConstant
USE BaseType, ONLY: Scalar => TypeFEVariableScalar
USE BaseType, ONLY: SpaceTime => TypeFEVariableSpaceTime
USE BaseType, ONLY: Time => TypeFEVariableTime
USE BaseType, ONLY: Vector => TypeFEVariableVector
USE FEVariable_Method, ONLY: DOT_PRODUCT
USE FEVariable_Method, ONLY: Get
USE FEVariable_Method, ONLY: NodalVariable
USE FEVariable_Method, ONLY: QuadratureVariable
USE FEVariable_Method, ONLY: SHAPE
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Nodal
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP) :: w(3), ut(3, 2), ust(3, 2, 2)
INTEGER(I4B) :: ii

! w: 3 components; ut: 3 components at 2 time points;
! ust: 3 components at 2 space points and 2 time points
w = [1.0_DFP, -1.0_DFP, 2.0_DFP]
ut = RESHAPE([(REAL(ii, DFP), ii=1, 6)], [3, 2])
ust = RESHAPE([(REAL(ii, DFP), ii=1, 12)], [3, 2, 2])

! Test 1: time . time gives a scalar time
BLOCK
  TYPE(FEVariable_) :: ans

  ans = DOT_PRODUCT(QuadratureVariable(ut, Vector, Time), &
                    QuadratureVariable(ut, Vector, Time))
  CALL OK(ans%rank .EQ. fevaropt%scalar, "time . time: scalar")
  CALL OK(ans%varType .EQ. fevaropt%time, "time . time: time")
  CALL OK(ALL(SHAPE(ans) .EQ. [2]), "time . time: shape [2]")
  CALL OK(ALL(ABS(Get(ans, Scalar, Time) - SUM(ut * ut, dim=1)) &
              .LT. tol), "time . time: values")
END BLOCK

! Test 2: time . constant gives a scalar time
BLOCK
  TYPE(FEVariable_) :: ans

  ans = DOT_PRODUCT(QuadratureVariable(ut, Vector, Time), &
                    QuadratureVariable(w, Vector, Constant))
  CALL OK(ans%varType .EQ. fevaropt%time, "time . constant: time")
  CALL OK(ALL(ABS(Get(ans, Scalar, Time) - MATMUL(w, ut)) .LT. tol), &
          "time . constant: values")
END BLOCK

! Test 3: spaceTime . spaceTime gives a scalar spaceTime
BLOCK
  TYPE(FEVariable_) :: ans

  ans = DOT_PRODUCT(QuadratureVariable(ust, Vector, SpaceTime), &
                    QuadratureVariable(ust, Vector, SpaceTime))
  CALL OK(ans%varType .EQ. fevaropt%spacetime, &
          "spaceTime . spaceTime: spaceTime")
  CALL OK(ALL(SHAPE(ans) .EQ. [2, 2]), &
          "spaceTime . spaceTime: shape [2, 2]")
  CALL OK(ALL(ABS(Get(ans, Scalar, SpaceTime) - SUM(ust * ust, dim=1)) &
              .LT. tol), "spaceTime . spaceTime: values")
END BLOCK

! Test 4: constant . spaceTime gives a scalar spaceTime
BLOCK
  TYPE(FEVariable_) :: ans
  REAL(DFP) :: ref(2, 2)
  INTEGER(I4B) :: jj, kk

  ans = DOT_PRODUCT(QuadratureVariable(w, Vector, Constant), &
                    QuadratureVariable(ust, Vector, SpaceTime))
  DO kk = 1, 2
    DO jj = 1, 2
      ref(jj, kk) = DOT_PRODUCT(w, ust(:, jj, kk))
    END DO
  END DO
  CALL OK(ALL(SHAPE(ans) .EQ. [2, 2]), &
          "constant . spaceTime: shape [2, 2]")
  CALL OK(ALL(ABS(Get(ans, Scalar, SpaceTime) - ref) .LT. tol), &
          "constant . spaceTime: values")
END BLOCK

! Test 5: nodal operands give a nodal result
BLOCK
  TYPE(FEVariable_) :: ans

  ans = DOT_PRODUCT(NodalVariable(w, Vector, Constant), &
                    NodalVariable(w, Vector, Constant))
  CALL OK(ans%defineOn .EQ. Nodal, "nodal: defineOn is nodal")
  CALL OK(ABS(Get(ans, Scalar, Constant) - 6.0_DFP) .LT. tol, &
          "nodal: w . w = 6")
END BLOCK

END PROGRAM main
