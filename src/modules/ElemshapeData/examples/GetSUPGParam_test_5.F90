!> author: OpenAI
! date: 2026-11-21
! summary: SUPG defaults omitted porosity to one

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
USE BaseType, ONLY: Vector => TypeFEVariableVector
USE ElemShapeData_Method, ONLY: ALLOCATE
USE ElemShapeData_Method, ONLY: GetSUPGParam
USE ElemShapeData_Method, ONLY: Initiate
USE ElemShapeData_Method, ONLY: LagrangeElemShapeData
USE ElemShapeData_Method, ONLY: Set
USE ElemShapeData_Method, ONLY: SetdNdXt
USE ElemShapeData_Method, ONLY: SetdNTdt
USE ElemShapeData_Method, ONLY: SetdNTdXt
USE ElemShapeData_Method, ONLY: SetJacobian
USE FEVariable_Method, ONLY: ASSIGNMENT(=)
USE FEVariable_Method, ONLY: Get
USE FEVariable_Method, ONLY: NodalVariable
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Line2
USE QuadraturePoint_Method, ONLY: QP_Initiate => Initiate
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP), PARAMETER :: coords(1, 2) = &
                        RESHAPE([-1.0_DFP, 1.0_DFP], [1, 2])
REAL(DFP), PARAMETER :: xst(1, 2, 2) = &
                        RESHAPE([-1.0_DFP, 1.0_DFP, -1.0_DFP, &
                        1.0_DFP], [1, 2, 2])
TYPE(ElemShapeData_) :: cell
TYPE(STElemShapeData_), ALLOCATABLE :: st(:)
TYPE(QuadraturePoint_) :: quad
TYPE(FEVariable_) :: c, val, nu, k, phi, tau, reference
INTEGER(I4B) :: ipt, opt

CALL QP_Initiate(obj=quad, elemType=Line2, domainName="BIUNIT", &
                 nips=[2], quadratureType=qpopt%GaussLegendre)
CALL LagrangeElemShapeData(obj=cell, quad=quad, nsd=1, xidim=1, &
                           elemType=Line2, refelemCoord=coords, &
                           domainName="BIUNIT", order=1)
CALL Set(obj=cell, val=coords, N=cell%N, dNdXi=cell%dNdXi)
CALL Initiate(st, cell)
DO ipt = 1, SIZE(st)
  CALL ALLOCATE (st(ipt), nsd=1, xidim=1, nns=2, nips=2, nnt=2)
  st(ipt)%T = cell%N(:, ipt)
  st(ipt)%dTdTheta = cell%dNdXi(:, 1, ipt)
  st(ipt)%jt = 0.5_DFP
  st(ipt)%wt = cell%ws(ipt)
  st(ipt)%N = cell%N
  st(ipt)%dNdXi = cell%dNdXi
  st(ipt)%ws = cell%ws
  CALL SetJacobian(obj=st(ipt), val=xst, dNdXi=cell%dNdXi, T=st(ipt)%T)
  CALL SetdNdXt(obj=st(ipt))
  CALL SetdNTdXt(obj=st(ipt))
  CALL SetdNTdt(obj=st(ipt), val=xst)
END DO
c = NodalVariable([1.0_DFP], Vector, Constant)
val = NodalVariable([-1.0_DFP, 1.0_DFP], Scalar, Space)
nu = NodalVariable(0.25_DFP, Scalar, Constant)
k = NodalVariable(0.5_DFP, Scalar, Constant)
phi = NodalVariable(1.0_DFP, Scalar, Constant)

! Test 1: omitted phi matches explicit unit porosity in every helper
DO opt = 1, 2
  CALL CheckSingle(cell, opt)
  CALL CheckSingle(st(2), opt)
  CALL GetSUPGParam(obj=st, tau=tau, c=c, val=val, nu=nu, k=k, opt=opt)
  CALL GetSUPGParam(obj=st, tau=reference, c=c, val=val, nu=nu, &
                    k=k, phi=phi, opt=opt)
  CALL OK(ALL(ABS(Get(tau, Scalar, SpaceTime) - &
                  Get(reference, Scalar, SpaceTime)) .LT. tol), &
          "space-time FEVariable: default phi = 1")
  CALL GetSUPGParam(obj=st, tau=tau, c=c, val=val, nu=0.25_DFP, &
                    k=0.5_DFP, opt=opt)
  CALL GetSUPGParam(obj=st, tau=reference, c=c, val=val, nu=0.25_DFP, &
                    k=0.5_DFP, phi=1.0_DFP, opt=opt)
  CALL OK(ALL(ABS(Get(tau, Scalar, SpaceTime) - &
                  Get(reference, Scalar, SpaceTime)) .LT. tol), &
          "space-time real: default phi = 1")
END DO

CONTAINS

SUBROUTINE CheckSingle(obj, option)
  CLASS(ElemShapeData_), INTENT(IN) :: obj
  !! spatial or single-time shape data
  INTEGER(I4B), INTENT(IN) :: option
  !! combination option
  TYPE(FEVariable_) :: actual, expected
  REAL(DFP) :: known

  CALL GetSUPGParam(obj=obj, tau=actual, c=c, val=val, nu=0.25_DFP, &
                    k=0.5_DFP, opt=option)
  CALL GetSUPGParam(obj=obj, tau=expected, c=c, val=val, nu=0.25_DFP, &
                    k=0.5_DFP, phi=1.0_DFP, opt=option)
  CALL OK(ALL(ABS(Get(actual, Scalar, Space) - &
                  Get(expected, Scalar, Space)) .LT. tol), &
          "single real: default phi = 1")
  CALL GetSUPGParam(obj=obj, tau=actual, c=c, val=val, nu=nu, k=k, &
                    opt=option)
  CALL OK(ALL(ABS(Get(actual, Scalar, Space) - &
                  Get(expected, Scalar, Space)) .LT. tol), &
          "single FEVariable: default phi = 1")
  SELECT TYPE (obj)
  TYPE IS (ElemShapeData_)
    IF (option .EQ. 1) THEN
      known = 1.0_DFP / SQRT(1.0_DFP + 0.25_DFP**2 + 1.0_DFP)
    ELSE
      known = 1.0_DFP / 2.25_DFP
    END IF
    CALL OK(ALL(ABS(Get(actual, Scalar, Space) - known) .LT. tol), &
            "spatial: known Darcy result")
  END SELECT

  ! Test 2: phi alone leaves the no-Darcy result unchanged
  CALL GetSUPGParam(obj=obj, tau=expected, c=c, val=val, nu=nu, opt=option)
  CALL GetSUPGParam(obj=obj, tau=actual, c=c, val=val, nu=nu, phi=phi, &
                    opt=option)
  CALL OK(ALL(Get(actual, Scalar, Space) .EQ. &
              Get(expected, Scalar, Space)), "FEVariable phi alone ignored")
  CALL GetSUPGParam(obj=obj, tau=actual, c=c, val=val, nu=0.25_DFP, &
                    phi=2.0_DFP, opt=option)
  CALL OK(ALL(Get(actual, Scalar, Space) .EQ. &
              Get(expected, Scalar, Space)), "real phi alone ignored")
END SUBROUTINE CheckSingle
END PROGRAM main
