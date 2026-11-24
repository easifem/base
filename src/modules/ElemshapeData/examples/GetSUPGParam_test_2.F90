!> author: Vikas Sharma, Ph. D.
! date: 2026-11-21
! summary: SUPG parameter in space-time

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
! convective velocity
REAL(DFP), PARAMETER :: cx = 1.0_DFP
! diffusivity; t3 = nu (SUM_I SUM_a |r dNT/dx|)^2 = nu
REAL(DFP), PARAMETER :: nu0 = 0.25_DFP
TYPE(ElemShapeData_) :: elemsd
TYPE(STElemShapeData_), ALLOCATABLE :: stelemsd(:)
TYPE(QuadraturePoint_) :: quad
TYPE(FEVariable_) :: c, val
REAL(DFP) :: t12(2, 2)
!! t12(ips, ipt) = SUM_I SUM_a |c dNT/dx + dNT/dt|
INTEGER(I4B) :: ipt

CALL QP_Initiate(obj=quad, elemType=Line2, domainName="BIUNIT", &
                 nips=[2], quadratureType=qpopt%GaussLegendre)
CALL LagrangeElemShapeData(obj=elemsd, quad=quad, nsd=1, xidim=1, &
                           elemType=Line2, refelemCoord=lineCoord, &
                           domainName="BIUNIT", order=1)
CALL MakeST(elemsd, stelemsd)
c = NodalVariable([cx], Vector, Constant)
val = NodalVariable(p, Scalar, Space)
DO ipt = 1, 2
  t12(:, ipt) = GetT12(stelemsd(ipt))
END DO

! Test 1: all time points, FEVariable nu, opt = 1 and opt = 2
BLOCK
  TYPE(FEVariable_) :: tau, nu

  nu = NodalVariable(nu0, Scalar, Constant)
  CALL GetSUPGParam(obj=stelemsd, tau=tau, c=c, val=val, nu=nu)
  CALL OK(ALL(SHAPE(Get(tau, Scalar, SpaceTime)) .EQ. [2, 2]), &
          "tau(nips, nipt)")
  CALL OK(ALL(ABS(Get(tau, Scalar, SpaceTime) - 1.0_DFP / &
                  SQRT(t12**2 + nu0**2)) .LT. tol), &
          "opt = 1: tau = 1 / SQRT(t12^2 + t3^2)")

  CALL GetSUPGParam(obj=stelemsd, tau=tau, c=c, val=val, nu=nu, opt=2)
  CALL OK(ALL(ABS(Get(tau, Scalar, SpaceTime) - 1.0_DFP / (t12 + nu0)) &
              .LT. tol), "opt = 2: tau = 1 / (t12 + t3)")

  ! the time derivative is in dNTdt, so dt does not change tau
  CALL GetSUPGParam(obj=stelemsd, tau=tau, c=c, val=val, nu=nu, &
                    dt=1.0_DFP, opt=2)
  CALL OK(ALL(ABS(Get(tau, Scalar, SpaceTime) - 1.0_DFP / (t12 + nu0)) &
              .LT. tol), "dt is not used in space-time")
END BLOCK

! Test 2: all time points, real nu, k = 1/4, phi = 1/2, so t4 = 1
BLOCK
  TYPE(FEVariable_) :: tau

  CALL GetSUPGParam(obj=stelemsd, tau=tau, c=c, val=val, nu=nu0, opt=2)
  CALL OK(ALL(ABS(Get(tau, Scalar, SpaceTime) - 1.0_DFP / (t12 + nu0)) &
              .LT. tol), "real nu, opt = 2")

  CALL GetSUPGParam(obj=stelemsd, tau=tau, c=c, val=val, nu=nu0, &
                    k=0.25_DFP, phi=0.5_DFP, opt=2)
  CALL OK(ALL(ABS(Get(tau, Scalar, SpaceTime) - 1.0_DFP / &
                  (t12 + nu0 + 1.0_DFP)) .LT. tol), &
          "real nu, Darcy, opt = 2: tau = 1 / (t12 + t3 + t4)")
END BLOCK

! Test 3: one time point, tau is a space variable
BLOCK
  TYPE(FEVariable_) :: tau

  CALL GetSUPGParam(obj=stelemsd(2), tau=tau, c=c, val=val, &
                    nu=NodalVariable(nu0, Scalar, Constant), opt=2)
  CALL OK(ALL(ABS(Get(tau, Scalar, Space) - 1.0_DFP / (t12(:, 2) + nu0)) &
              .LT. tol), "one time point, FEVariable nu")

  CALL GetSUPGParam(obj=stelemsd(2), tau=tau, c=c, val=val, nu=nu0)
  CALL OK(ALL(ABS(Get(tau, Scalar, Space) - 1.0_DFP / &
                  SQRT(t12(:, 2)**2 + nu0**2)) .LT. tol), &
          "one time point, real nu")
END BLOCK

CONTAINS

!> Returns SUM_I SUM_a |cx dN_I/dx T_a + N_I dT_a/dt| at each space
! quadrature point of one time point
FUNCTION GetT12(obj) RESULT(ans)
  TYPE(STElemShapeData_), INTENT(IN) :: obj
  !! space-time shape data at one time point
  REAL(DFP) :: ans(2)
  !! value at each space quadrature point
  INTEGER(I4B) :: ips, ii, aa

  ans = 0.0_DFP
  DO ips = 1, 2
    DO aa = 1, 2
      DO ii = 1, 2
        ans(ips) = ans(ips) + ABS(cx * obj%dNdXt(ii, 1, ips) * obj%T(aa) &
                                  + obj%N(ii, ips) * obj%dTdTheta(aa) / &
                                  obj%jt)
      END DO
    END DO
  END DO
END FUNCTION GetT12

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
