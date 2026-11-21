!> author: Vikas Sharma, Ph. D.
! date: 2026-11-21
! summary: SUGN3 parameter in space, tau = h^2 / (4 nu)

PROGRAM main
USE BaseType, ONLY: Constant => TypeFEVariableConstant
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: qpopt => TypeQuadratureOpt
USE BaseType, ONLY: QuadraturePoint_
USE BaseType, ONLY: Scalar => TypeFEVariableScalar
USE BaseType, ONLY: Space => TypeFEVariableSpace
USE ElemShapeData_Method, ONLY: GetSUGN3Param
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
TYPE(ElemShapeData_) :: cell
TYPE(QuadraturePoint_) :: quad

CALL QP_Initiate(obj=quad, elemType=Quadrangle4, domainName="BIUNIT", &
                 nips=[2], quadratureType=qpopt%GaussLegendre)
CALL LagrangeElemShapeData(obj=cell, quad=quad, nsd=2, xidim=2, &
                           elemType=Quadrangle4, refelemCoord=quadCoord, &
                           domainName="BIUNIT", order=1)
CALL Set(obj=cell, val=xcell, N=cell%N, dNdXi=cell%dNdXi)

! Test 1: val = y - 1/2, r = (0, 1), h = cell height = 1
! nu is a constant FEVariable, nu = 1/2, so tau = 1 / (4 nu) = 1/2
BLOCK
  TYPE(FEVariable_) :: tau, h, nu
  REAL(DFP) :: p(4)

  p = xcell(2, :) - 0.5_DFP
  nu = NodalVariable(0.5_DFP, Scalar, Constant)
  CALL GetSUGN3Param(obj=cell, tau=tau, val=NodalVariable(p, Scalar, &
                     Space), nu=nu, h=h)
  CALL OK(SIZE(Get(tau, Scalar, Space)) .EQ. 4, "tau has nips values")
  CALL OK(ALL(ABS(Get(h, Scalar, Space) - 1.0_DFP) .LT. tol), &
          "FEVariable nu: h = 1")
  CALL OK(ALL(ABS(Get(tau, Scalar, Space) - 0.5_DFP) .LT. tol), &
          "FEVariable nu: tau = h^2 / (4 nu) = 1/2")
END BLOCK

! Test 2: val = x - 1, r = (1, 0), h = cell width = 2
! nu is a real number, nu = 1/2, so tau = 4 / (4 nu) = 2
BLOCK
  TYPE(FEVariable_) :: tau, h
  REAL(DFP) :: p(4)

  p = xcell(1, :) - 1.0_DFP
  CALL GetSUGN3Param(obj=cell, tau=tau, val=NodalVariable(p, Scalar, &
                     Space), nu=0.5_DFP, h=h)
  CALL OK(ALL(ABS(Get(h, Scalar, Space) - 2.0_DFP) .LT. tol), &
          "real nu: h = 2")
  CALL OK(ALL(ABS(Get(tau, Scalar, Space) - 2.0_DFP) .LT. tol), &
          "real nu: tau = h^2 / (4 nu) = 2")
END BLOCK

! Test 3: constant val has no gradient, so h = hmin = 1 and tau = 1/2
BLOCK
  TYPE(FEVariable_) :: tau

  CALL GetSUGN3Param(obj=cell, tau=tau, val=NodalVariable(1.0_DFP, &
                     Scalar, Constant), nu=0.5_DFP)
  CALL OK(ALL(ABS(Get(tau, Scalar, Space) - 0.5_DFP) .LT. tol), &
          "zero gradient: h = hmin, tau = 1/2")
END BLOCK

END PROGRAM main
