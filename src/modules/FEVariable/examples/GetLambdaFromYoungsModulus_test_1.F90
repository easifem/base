!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: Lame parameter lambda from Young's and shear modulus

PROGRAM main
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
USE BaseType, ONLY: Constant => TypeFEVariableConstant
USE BaseType, ONLY: Scalar => TypeFEVariableScalar
USE BaseType, ONLY: Space => TypeFEVariableSpace
USE FEVariable_Method, ONLY: GetLambdaFromYoungsModulus
USE FEVariable_Method, ONLY: QuadratureVariable
USE GlobalData, ONLY: DFP
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP

! Test 1: constant moduli, E = 2 mu (1 + nu), nu = 0.25
BLOCK
  TYPE(FEVariable_) :: E, mu, lambda
  REAL(DFP) :: nu, Ev, muv, ans

  nu = 0.25_DFP
  Ev = 100.0_DFP
  muv = Ev / (2.0_DFP * (1.0_DFP + nu))
  ans = Ev * nu / ((1.0_DFP + nu) * (1.0_DFP - 2.0_DFP * nu))

  E = QuadratureVariable(Ev, Scalar, Constant)
  mu = QuadratureVariable(muv, Scalar, Constant)
  CALL GetLambdaFromYoungsModulus(youngsModulus=E, shearModulus=mu, &
                                  lambda=lambda)
  CALL OK(ABS(lambda%val(1) - ans) .LT. tol, "constant: lambda = 40")
  CALL OK(lambda%rank .EQ. fevaropt%scalar, "constant: rank of lambda")
  CALL OK(lambda%varType .EQ. fevaropt%constant, "constant: vartype")
END BLOCK

! Test 2: moduli at quadrature points
BLOCK
  TYPE(FEVariable_) :: E, mu, lambda
  REAL(DFP) :: Ev(3), muv(3), ans(3)

  Ev = [100.0_DFP, 200.0_DFP, 300.0_DFP]
  muv = Ev / 2.5_DFP
  ans = Ev * 0.25_DFP / (1.25_DFP * 0.5_DFP)

  E = QuadratureVariable(Ev, Scalar, Space)
  mu = QuadratureVariable(muv, Scalar, Space)
  CALL GetLambdaFromYoungsModulus(youngsModulus=E, shearModulus=mu, &
                                  lambda=lambda)
  CALL OK(lambda%len .EQ. 3, "space: len of lambda = 3")
  CALL OK(ALL(ABS(lambda%val(1:3) - ans) .LT. tol), "space: lambda")
END BLOCK

END PROGRAM main
