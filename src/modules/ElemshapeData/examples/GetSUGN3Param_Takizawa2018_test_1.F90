!> author: Vikas Sharma, Ph. D.
! date: 2026-11-21
! summary: SUGN3 parameter of Takizawa (2018) in space

PROGRAM main
USE BaseType, ONLY: Constant => TypeFEVariableConstant
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: qpopt => TypeQuadratureOpt
USE BaseType, ONLY: QuadraturePoint_
USE BaseType, ONLY: Scalar => TypeFEVariableScalar
USE BaseType, ONLY: Space => TypeFEVariableSpace
USE ElemShapeData_Method, ONLY: GetSUGN3Param_Takizawa2018
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
! physical cell: rectangle [0, 2] x [0, 1], so hmin = 1 and hmax = 2
REAL(DFP), PARAMETER :: xcell(2, 4) = RESHAPE([0.0_DFP, 0.0_DFP, 2.0_DFP, &
                                               0.0_DFP, 2.0_DFP, 1.0_DFP, &
                                               0.0_DFP, 1.0_DFP], [2, 4])
TYPE(ElemShapeData_) :: cell
TYPE(QuadraturePoint_) :: quad

CALL QP_Initiate(obj=quad, elemType=Quadrangle4, domainName="BIUNIT", &
                 nips=[2], quadratureType=qpopt%GaussLegendre)
CALL LagrangeElemShapeData(obj=cell, quad=quad, nsd=2, xidim=2, &
                           elemType=Quadrangle4, refelemCoord=quadCoord, &
                           domainName="BIUNIT", order=1)
CALL Set(obj=cell, val=xcell, N=cell%N, dNdXi=cell%dNdXi)

! Test 1: val = y - 1/2, r = (0, 1), h = 1; with |r| = 1,
! tau = h^2 / (4 nu) = 1/2 for nu = 1/2 (constant FEVariable)
BLOCK
  TYPE(FEVariable_) :: tau, h, hmax, hmin, nu
  REAL(DFP) :: p(4)

  p = xcell(2, :) - 0.5_DFP
  nu = NodalVariable(0.5_DFP, Scalar, Constant)
  CALL GetSUGN3Param_Takizawa2018(obj=cell, tau=tau, &
                                  val=NodalVariable(p, Scalar, Space), &
                                  nu=nu, h=h, hmax=hmax, hmin=hmin)
  CALL OK(SIZE(Get(tau, Scalar, Space)) .EQ. 4, "tau has nips values")
  CALL OK(ALL(ABS(Get(h, Scalar, Space) - 1.0_DFP) .LT. tol), &
          "FEVariable nu: h = 1")
  CALL OK(ALL(ABS(Get(hmin, Scalar, Space) - 1.0_DFP) .LT. tol), &
          "FEVariable nu: hmin = 1")
  CALL OK(ALL(ABS(Get(hmax, Scalar, Space) - 2.0_DFP) .LT. tol), &
          "FEVariable nu: hmax = 2")
  CALL OK(ALL(ABS(Get(tau, Scalar, Space) - 0.5_DFP) .LT. tol), &
          "FEVariable nu: tau = 1/2")
END BLOCK

! Test 2: val = x - 1, r = (1, 0), h = 2, real nu = 1/2, so tau = 2
BLOCK
  TYPE(FEVariable_) :: tau, h

  CALL GetSUGN3Param_Takizawa2018(obj=cell, tau=tau, &
                                  val=NodalVariable(xcell(1, :) - 1.0_DFP, &
                                                    Scalar, Space), &
                                  nu=0.5_DFP, h=h)
  CALL OK(ALL(ABS(Get(h, Scalar, Space) - 2.0_DFP) .LT. tol), &
          "real nu: h = 2")
  CALL OK(ALL(ABS(Get(tau, Scalar, Space) - 2.0_DFP) .LT. tol), &
          "real nu: tau = 2")
END BLOCK

! Test 3: constant val, r = 0 and h = 0, so tau = hmin^2 / (4 nu) = 1/2
BLOCK
  TYPE(FEVariable_) :: tau, h

  CALL GetSUGN3Param_Takizawa2018(obj=cell, tau=tau, &
                                  val=NodalVariable(1.0_DFP, Scalar, &
                                                    Constant), &
                                  nu=0.5_DFP, h=h)
  CALL OK(ALL(ABS(Get(h, Scalar, Space)) .LT. tol), "zero gradient: h = 0")
  CALL OK(ALL(ABS(Get(tau, Scalar, Space) - 0.5_DFP) .LT. tol), &
          "zero gradient: tau = hmin^2 / (4 nu)")
END BLOCK

END PROGRAM main
