!> author: Vikas Sharma, Ph. D.
! date: 2026-11-21
! summary: SUGN3 parameter in space-time, tau = h^2 / (4 nu)

PROGRAM main
USE BaseType, ONLY: Constant => TypeFEVariableConstant
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: qpopt => TypeQuadratureOpt
USE BaseType, ONLY: QuadraturePoint_
USE BaseType, ONLY: Scalar => TypeFEVariableScalar
USE BaseType, ONLY: Space => TypeFEVariableSpace
USE BaseType, ONLY: SpaceTime => TypeFEVariableSpaceTime
USE BaseType, ONLY: STElemShapeData_
USE ElemShapeData_Method, ONLY: ALLOCATE
USE ElemShapeData_Method, ONLY: GetSUGN3Param
USE ElemShapeData_Method, ONLY: Initiate
USE ElemShapeData_Method, ONLY: LagrangeElemShapeData
USE ElemShapeData_Method, ONLY: SetdNdXt
USE ElemShapeData_Method, ONLY: SetdNTdt
USE ElemShapeData_Method, ONLY: SetdNTdXt
USE ElemShapeData_Method, ONLY: SetJacobian
USE FEVariable_Method, ONLY: Get
USE FEVariable_Method, ONLY: NodalVariable
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Line2
USE QuadraturePoint_Method, ONLY: QP_Initiate => Initiate
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP), PARAMETER :: lineCoord(1, 2) = RESHAPE([-1.0_DFP, 1.0_DFP], &
                                                  [1, 2])
! space element [0, 2], time element [0, 1]: x = 1 + xi, jt = 1/2
REAL(DFP), PARAMETER :: xst(1, 2, 2) = RESHAPE([0.0_DFP, 2.0_DFP, 0.0_DFP, &
                                                2.0_DFP], [1, 2, 2])
! val = x - 1 at the space nodes, so r = sign(val) = +1 or -1
REAL(DFP), PARAMETER :: p(2) = [-1.0_DFP, 1.0_DFP]
TYPE(ElemShapeData_) :: elemsd
TYPE(STElemShapeData_), ALLOCATABLE :: stelemsd(:)
TYPE(QuadraturePoint_) :: quad

CALL QP_Initiate(obj=quad, elemType=Line2, domainName="BIUNIT", &
                 nips=[2], quadratureType=qpopt%GaussLegendre)
CALL LagrangeElemShapeData(obj=elemsd, quad=quad, nsd=1, xidim=1, &
                           elemType=Line2, refelemCoord=lineCoord, &
                           domainName="BIUNIT", order=1)
CALL MakeST(elemsd, stelemsd)

! Test 1: all time points; SUM_I SUM_a |dNT/dx| = 1, so h = 2
! nu is a constant FEVariable, nu = 1/2, so tau = 4 / (4 nu) = 2
BLOCK
  TYPE(FEVariable_) :: tau, h, nu

  nu = NodalVariable(0.5_DFP, Scalar, Constant)
  CALL GetSUGN3Param(obj=stelemsd, tau=tau, &
                     val=NodalVariable(p, Scalar, Space), nu=nu, h=h)
  CALL OK(ALL(SHAPE(Get(tau, Scalar, SpaceTime)) .EQ. [2, 2]), &
          "tau(nips, nipt)")
  CALL OK(ALL(ABS(Get(h, Scalar, SpaceTime) - 2.0_DFP) .LT. tol), &
          "FEVariable nu: h = 2")
  CALL OK(ALL(ABS(Get(tau, Scalar, SpaceTime) - 2.0_DFP) .LT. tol), &
          "FEVariable nu: tau = 2")
END BLOCK

! Test 2: all time points, real nu = 1/4, so tau = 4 / (4 nu) = 4
BLOCK
  TYPE(FEVariable_) :: tau

  CALL GetSUGN3Param(obj=stelemsd, tau=tau, &
                     val=NodalVariable(p, Scalar, Space), nu=0.25_DFP)
  CALL OK(ALL(ABS(Get(tau, Scalar, SpaceTime) - 4.0_DFP) .LT. tol), &
          "real nu: tau = 4")
END BLOCK

! Test 3: one time point, tau is a space variable
BLOCK
  TYPE(FEVariable_) :: tau

  CALL GetSUGN3Param(obj=stelemsd(2), tau=tau, &
                     val=NodalVariable(p, Scalar, Space), nu=0.5_DFP)
  CALL OK(ALL(ABS(Get(tau, Scalar, Space) - 2.0_DFP) .LT. tol), &
          "one time point: tau = 2")
END BLOCK

CONTAINS

!> Space-time shape data of a linear line in space and in time, on a
! fixed mesh with jt = 1/2
SUBROUTINE MakeST(elemsd, stelemsd)
  TYPE(ElemShapeData_), INTENT(IN) :: elemsd
  !! shape data of a linear line, used in space and in time
  TYPE(STElemShapeData_), ALLOCATABLE, INTENT(INOUT) :: stelemsd(:)
  !! space-time shape data, one per time point
  INTEGER(I4B) :: ipt

  CALL Initiate(stelemsd, elemsd)
  DO ipt = 1, SIZE(stelemsd)
    CALL ALLOCATE (stelemsd(ipt), nsd=1, xidim=1, nns=2, nips=2, nnt=2)
    stelemsd(ipt)%T = elemsd%N(:, ipt)
    stelemsd(ipt)%dTdTheta = elemsd%dNdXi(:, 1, ipt)
    stelemsd(ipt)%jt = 0.5_DFP
    stelemsd(ipt)%wt = elemsd%ws(ipt)
    stelemsd(ipt)%N = elemsd%N
    stelemsd(ipt)%dNdXi = elemsd%dNdXi
    stelemsd(ipt)%ws = elemsd%ws
    CALL SetJacobian(obj=stelemsd(ipt), val=xst, dNdXi=elemsd%dNdXi, &
                     T=stelemsd(ipt)%T)
    CALL SetdNdXt(obj=stelemsd(ipt))
    CALL SetdNTdXt(obj=stelemsd(ipt))
    CALL SetdNTdt(obj=stelemsd(ipt), val=xst)
  END DO
END SUBROUTINE MakeST

END PROGRAM main
