!> author: Vikas Sharma, Ph. D.
! date: 2026-11-21
! summary: SUPG parameter in space, with time step and Darcy terms

PROGRAM main
USE BaseType, ONLY: Constant => TypeFEVariableConstant
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: qpopt => TypeQuadratureOpt
USE BaseType, ONLY: QuadraturePoint_
USE BaseType, ONLY: Scalar => TypeFEVariableScalar
USE BaseType, ONLY: Space => TypeFEVariableSpace
USE BaseType, ONLY: Vector => TypeFEVariableVector
USE ElemShapeData_Method, ONLY: GetSUPGParam
USE ElemShapeData_Method, ONLY: LagrangeElemShapeData
USE ElemShapeData_Method, ONLY: Set
USE FEVariable_Method, ONLY: Get
USE FEVariable_Method, ONLY: NodalVariable
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: Quadrangle4
USE QuadraturePoint_Method, ONLY: QP_Initiate => Initiate
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP), PARAMETER :: quadCoord(2, 4) = RESHAPE([-1.0_DFP, -1.0_DFP, &
                        1.0_DFP, -1.0_DFP, 1.0_DFP, 1.0_DFP, -1.0_DFP, &
                                                   1.0_DFP], [2, 4])
! physical cell: rectangle [0, 2] x [0, 1]
REAL(DFP), PARAMETER :: xcell(2, 4) = RESHAPE([0.0_DFP, 0.0_DFP, 2.0_DFP, &
                                               0.0_DFP, 2.0_DFP, 1.0_DFP, &
                                               0.0_DFP, 1.0_DFP], [2, 4])
! With c = (1, 0) and val = y - 1/2 (so r = (0, 1)), at every point:
! t1 = SUM_I |c . dN_I/dx| = 1, t3 = nu (SUM_I |r . dN_I/dx|)^2 = 4 nu.
! With nu = 1/4, t1 = t3 = 1.
TYPE(ElemShapeData_) :: cell
TYPE(QuadraturePoint_) :: quad
TYPE(FEVariable_) :: c, val
REAL(DFP) :: p(4)

CALL QP_Initiate(obj=quad, elemType=Quadrangle4, domainName="BIUNIT", &
                 nips=[2], quadratureType=qpopt%GaussLegendre)
CALL LagrangeElemShapeData(obj=cell, quad=quad, nsd=2, xidim=2, &
                           elemType=Quadrangle4, refelemCoord=quadCoord, &
                           domainName="BIUNIT", order=1)
CALL Set(obj=cell, val=xcell, N=cell%N, dNdXi=cell%dNdXi)
c = NodalVariable([1.0_DFP, 0.0_DFP], Vector, Constant)
p = xcell(2, :) - 0.5_DFP
val = NodalVariable(p, Scalar, Space)

! Test 1: FEVariable nu, opt = 1 and opt = 2, without dt
BLOCK
  TYPE(FEVariable_) :: tau, nu

  nu = NodalVariable(0.25_DFP, Scalar, Constant)
  CALL GetSUPGParam(obj=cell, tau=tau, c=c, val=val, nu=nu)
  CALL OK(SIZE(Get(tau, Scalar, Space)) .EQ. 4, "tau has nips values")
  CALL OK(ALL(ABS(Get(tau, Scalar, Space) - 1.0_DFP / SQRT(2.0_DFP)) &
              .LT. tol), "opt = 1: tau = 1 / SQRT(t1^2 + t3^2)")

  CALL GetSUPGParam(obj=cell, tau=tau, c=c, val=val, nu=nu, opt=2)
  CALL OK(ALL(ABS(Get(tau, Scalar, Space) - 0.5_DFP) .LT. tol), &
          "opt = 2: tau = 1 / (t1 + t3)")
END BLOCK

! Test 2: FEVariable nu with dt = 2, so t2 = 2 / dt = 1
BLOCK
  TYPE(FEVariable_) :: tau, nu

  nu = NodalVariable(0.25_DFP, Scalar, Constant)
  CALL GetSUPGParam(obj=cell, tau=tau, c=c, val=val, nu=nu, dt=2.0_DFP)
  CALL OK(ALL(ABS(Get(tau, Scalar, Space) - 1.0_DFP / SQRT(3.0_DFP)) &
              .LT. tol), "dt, opt = 1: tau = 1 / SQRT(3)")

  CALL GetSUPGParam(obj=cell, tau=tau, c=c, val=val, nu=nu, dt=2.0_DFP, &
                    opt=2)
  CALL OK(ALL(ABS(Get(tau, Scalar, Space) - 1.0_DFP / 3.0_DFP) .LT. tol), &
          "dt, opt = 2: tau = 1 / 3")
END BLOCK

! Test 3: FEVariable k = 1/4 and phi = 1/2, t4 = 2 phi nu / k = 1
BLOCK
  TYPE(FEVariable_) :: tau, nu, k, phi

  nu = NodalVariable(0.25_DFP, Scalar, Constant)
  k = NodalVariable(0.25_DFP, Scalar, Constant)
  phi = NodalVariable(0.5_DFP, Scalar, Constant)
  CALL GetSUPGParam(obj=cell, tau=tau, c=c, val=val, nu=nu, k=k, &
                    phi=phi, opt=2)
  CALL OK(ALL(ABS(Get(tau, Scalar, Space) - 1.0_DFP / 3.0_DFP) .LT. tol), &
          "Darcy, opt = 2: tau = 1 / (t1 + t3 + t4)")
END BLOCK

! Test 4: real nu, k and phi give the same values
BLOCK
  TYPE(FEVariable_) :: tau

  CALL GetSUPGParam(obj=cell, tau=tau, c=c, val=val, nu=0.25_DFP)
  CALL OK(ALL(ABS(Get(tau, Scalar, Space) - 1.0_DFP / SQRT(2.0_DFP)) &
              .LT. tol), "real nu, opt = 1")

  CALL GetSUPGParam(obj=cell, tau=tau, c=c, val=val, nu=0.25_DFP, &
                    dt=2.0_DFP, opt=2)
  CALL OK(ALL(ABS(Get(tau, Scalar, Space) - 1.0_DFP / 3.0_DFP) .LT. tol), &
          "real nu, dt, opt = 2")

  CALL GetSUPGParam(obj=cell, tau=tau, c=c, val=val, nu=0.25_DFP, &
                    k=0.25_DFP, phi=0.5_DFP, opt=2)
  CALL OK(ALL(ABS(Get(tau, Scalar, Space) - 1.0_DFP / 3.0_DFP) .LT. tol), &
          "real nu, Darcy, opt = 2")
END BLOCK

END PROGRAM main
