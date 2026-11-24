!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: Time derivatives of space-time shape functions

PROGRAM main
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: qpopt => TypeQuadratureOpt
USE BaseType, ONLY: QuadraturePoint_
USE BaseType, ONLY: STElemShapeData_
USE ElemShapeData_Method, ONLY: ALLOCATE
USE ElemShapeData_Method, ONLY: Initiate
USE ElemShapeData_Method, ONLY: LagrangeElemShapeData
USE ElemShapeData_Method, ONLY: SetdNTdt
USE ElemShapeData_Method, ONLY: SetdNTdXt
USE ElemShapeData_Method, ONLY: SetJacobian
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Line2
USE GlobalData, ONLY: Quadrangle4
USE QuadraturePoint_Method, ONLY: QP_Initiate => Initiate
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP), PARAMETER :: lineCoord(1, 2) = RESHAPE([-1.0_DFP, 1.0_DFP], &
                                                  [1, 2])
! space element [0, 2], time element [0, 1]: x = 1 + xi, jt = 1/2
REAL(DFP), PARAMETER :: xst(1, 2, 2) = RESHAPE([0.0_DFP, 2.0_DFP, 0.0_DFP, &
                                                2.0_DFP], [1, 2, 2])
TYPE(ElemShapeData_) :: elemsd
TYPE(STElemShapeData_), ALLOCATABLE :: stelemsd(:)
TYPE(QuadraturePoint_) :: quad

CALL QP_Initiate(obj=quad, elemType=Line2, domainName="BIUNIT", &
                 nips=[2], quadratureType=qpopt%GaussLegendre)
CALL LagrangeElemShapeData(obj=elemsd, quad=quad, nsd=1, xidim=1, &
                           elemType=Line2, refelemCoord=lineCoord, &
                           domainName="BIUNIT", order=1)
CALL MakeST(elemsd, stelemsd)

! Test 1: fixed mesh, dNTdt(I, a, ip) = N_I dT_a/dtheta / jt
BLOCK
  INTEGER(I4B) :: ips, aa

  CALL SetJacobian(obj=stelemsd(1), val=xst, dNdXi=elemsd%dNdXi, &
                   T=stelemsd(1)%T)
  CALL SetdNTdXt(obj=stelemsd(1))
  CALL SetdNTdt(obj=stelemsd(1), val=xst)
  DO ips = 1, 2
    DO aa = 1, 2
      CALL OK(ALL(ABS(stelemsd(1)%dNTdt(:, aa, ips) - elemsd%N(:, ips) * &
                  stelemsd(1)%dTdTheta(aa) / 0.5_DFP) .LT. tol), &
              "dNTdt = N dT/dtheta / jt")
    END DO
  END DO
  CALL OK(ALL(ABS(SUM(SUM(stelemsd(1)%dNTdt, dim=1), dim=1)) .LT. tol), &
          "sum of dNTdt = 0")
END BLOCK

CONTAINS

!> Space-time shape data of a linear line in space and in time; the
! space part is the same at every time point and jt = 1/2
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
  END DO
END SUBROUTINE MakeST

END PROGRAM main
