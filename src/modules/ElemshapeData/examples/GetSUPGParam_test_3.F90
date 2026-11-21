!> author: OpenAI
! date: 2026-11-21
! summary: SUPG uses each time column of quadrature coefficients

PROGRAM main
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
USE ElemShapeData_Method, ONLY: SetdNdXt
USE ElemShapeData_Method, ONLY: SetdNTdt
USE ElemShapeData_Method, ONLY: SetdNTdXt
USE ElemShapeData_Method, ONLY: SetJacobian
USE FEVariable_Method, ONLY: ASSIGNMENT(=)
USE FEVariable_Method, ONLY: Get
USE FEVariable_Method, ONLY: NodalVariable
USE FEVariable_Method, ONLY: QuadratureVariable
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
                        RESHAPE([0.0_DFP, 2.0_DFP, 0.0_DFP, &
                        2.0_DFP], [1, 2, 2])
TYPE(ElemShapeData_) :: elemsd
TYPE(STElemShapeData_), ALLOCATABLE :: st(:)
TYPE(QuadraturePoint_) :: quad
TYPE(FEVariable_) :: c, nu, k, phi, val, tau, reference
REAL(DFP) :: cq(1, 2, 2), nuq(2, 2), kq(2, 2), phiq(2, 2)
REAL(DFP), ALLOCATABLE :: actual(:, :)
INTEGER(I4B) :: ipt, scenario, opt

CALL QP_Initiate(obj=quad, elemType=Line2, domainName="BIUNIT", &
                 nips=[2], quadratureType=qpopt%GaussLegendre)
CALL LagrangeElemShapeData(obj=elemsd, quad=quad, nsd=1, xidim=1, &
                           elemType=Line2, refelemCoord=coords, &
                           domainName="BIUNIT", order=1)
CALL Initiate(st, elemsd)
DO ipt = 1, SIZE(st)
  CALL ALLOCATE (st(ipt), nsd=1, xidim=1, nns=2, nips=2, nnt=2)
  st(ipt)%T = elemsd%N(:, ipt)
  st(ipt)%dTdTheta = elemsd%dNdXi(:, 1, ipt)
  st(ipt)%jt = 0.5_DFP
  st(ipt)%wt = elemsd%ws(ipt)
  st(ipt)%N = elemsd%N
  st(ipt)%dNdXi = elemsd%dNdXi
  st(ipt)%ws = elemsd%ws
  CALL SetJacobian(obj=st(ipt), val=xst, dNdXi=elemsd%dNdXi, &
                   T=st(ipt)%T)
  CALL SetdNdXt(obj=st(ipt))
  CALL SetdNTdXt(obj=st(ipt))
  CALL SetdNTdt(obj=st(ipt), val=xst)
END DO
val = NodalVariable([-1.0_DFP, 1.0_DFP], Scalar, Space)

! Test 1: vary diffusivity, permeability/porosity, and velocity separately
DO scenario = 1, 4
  cq = 1.0_DFP
  nuq = 0.25_DFP
  kq = 0.5_DFP
  phiq = 0.5_DFP
  SELECT CASE (scenario)
  CASE (1)
    nuq(:, 2) = [0.5_DFP, 0.75_DFP]
  CASE (2)
    kq(:, 2) = [1.0_DFP, 2.0_DFP]
    phiq(:, 2) = [0.25_DFP, 0.75_DFP]
  CASE DEFAULT
    cq(1, :, 2) = [4.0_DFP, 8.0_DFP]
  END SELECT
  c = QuadratureVariable(cq, Vector, SpaceTime)
  nu = QuadratureVariable(nuq, Scalar, SpaceTime)
  k = QuadratureVariable(kq, Scalar, SpaceTime)
  phi = QuadratureVariable(phiq, Scalar, SpaceTime)
  DO opt = 1, 2
    IF (scenario .EQ. 4) THEN
      CALL GetSUPGParam(obj=st, tau=tau, c=c, val=val, nu=0.25_DFP, &
                        k=0.5_DFP, phi=0.5_DFP, opt=opt)
    ELSE
      CALL GetSUPGParam(obj=st, tau=tau, c=c, val=val, nu=nu, &
                        k=k, phi=phi, opt=opt)
    END IF
    actual = Get(tau, Scalar, SpaceTime)
    DO ipt = 1, 2
      IF (scenario .EQ. 4) THEN
        CALL GetSUPGParam(obj=st(ipt), tau=reference, &
                          c=QuadratureVariable(cq(:, :, ipt), &
                                               Vector, Space), &
                          val=val, nu=0.25_DFP, k=0.5_DFP, &
                          phi=0.5_DFP, opt=opt)
      ELSE
        CALL GetSUPGParam(obj=st(ipt), tau=reference, &
                          c=QuadratureVariable(cq(:, :, ipt), &
                                               Vector, Space), &
                          val=val, &
                          nu=QuadratureVariable(nuq(:, ipt), Scalar, Space), &
                          k=QuadratureVariable(kq(:, ipt), Scalar, Space), &
                          phi=QuadratureVariable(phiq(:, ipt), Scalar, &
                                                 Space), opt=opt)
      END IF
      CALL OK(ALL(ABS(actual(:, ipt) - Get(reference, Scalar, Space)) &
                  .LT. tol), "time column agrees with spatial coefficients")
    END DO
  END DO
END DO
END PROGRAM main
