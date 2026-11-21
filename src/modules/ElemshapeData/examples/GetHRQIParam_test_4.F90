!> author: Vikas Sharma, Ph. D.
! date: 2026-11-21
! summary: HRQI length scale as space-time quadrature FEVariables

PROGRAM main
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: feopt => TypeFEVariableOpt
USE BaseType, ONLY: qpopt => TypeQuadratureOpt
USE BaseType, ONLY: QuadraturePoint_
USE BaseType, ONLY: Scalar => TypeFEVariableScalar
USE BaseType, ONLY: Space => TypeFEVariableSpace
USE BaseType, ONLY: STElemShapeData_
USE ElemShapeData_Method, ONLY: ALLOCATE
USE ElemShapeData_Method, ONLY: GetHRQIParam
USE ElemShapeData_Method, ONLY: Initiate
USE ElemShapeData_Method, ONLY: LagrangeElemShapeData
USE ElemShapeData_Method, ONLY: Set
USE FEVariable_Method, ONLY: NodalVariable
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Line2
USE GlobalData, ONLY: Quadrangle4
USE QuadraturePoint_Method, ONLY: QP_Initiate => Initiate
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP), PARAMETER :: quadCoord(2, 4) = RESHAPE([-1.0_DFP, -1.0_DFP, &
                        1.0_DFP, -1.0_DFP, 1.0_DFP, 1.0_DFP, -1.0_DFP, &
                                                   1.0_DFP], [2, 4])
REAL(DFP), PARAMETER :: lineCoord(1, 2) = RESHAPE([-1.0_DFP, 1.0_DFP], &
                                                  [1, 2])
! space: rectangle [0, 2] x [0, 1], G = inv(J J^T) = diag(1, 4)
REAL(DFP), PARAMETER :: xcell(2, 4) = RESHAPE([0.0_DFP, 0.0_DFP, 2.0_DFP, &
                                               0.0_DFP, 2.0_DFP, 1.0_DFP, &
                                               0.0_DFP, 1.0_DFP], [2, 4])
TYPE(ElemShapeData_) :: cell, line
TYPE(QuadraturePoint_) :: quad, lquad
TYPE(STElemShapeData_), ALLOCATABLE :: st(:)
REAL(DFP) :: p(4)

CALL QP_Initiate(obj=quad, elemType=Quadrangle4, domainName="BIUNIT", &
                 nips=[2], quadratureType=qpopt%GaussLegendre)
CALL QP_Initiate(obj=lquad, elemType=Line2, domainName="BIUNIT", &
                 nips=[2], quadratureType=qpopt%GaussLegendre)
CALL LagrangeElemShapeData(obj=cell, quad=quad, nsd=2, xidim=2, &
                           elemType=Quadrangle4, refelemCoord=quadCoord, &
                           domainName="BIUNIT", order=1)
CALL LagrangeElemShapeData(obj=line, quad=lquad, nsd=1, xidim=1, &
                           elemType=Line2, refelemCoord=lineCoord, &
                           domainName="BIUNIT", order=1)
CALL MakeST(cell, line, st)
! level set p = y - 1/2 (space nodal values), so h = 1 at every point
p = xcell(2, :) - 0.5_DFP

! Test 1: only h, a scalar space-time quadrature variable
BLOCK
  TYPE(FEVariable_) :: h

  CALL GetHRQIParam(obj=st, h=h, val=NodalVariable(p, Scalar, Space))
  CALL OK(h%rank .EQ. feopt%scalar, "h: scalar")
  CALL OK(h%defineOn .EQ. feopt%quadrature, "h: quadrature")
  CALL OK(h%varType .EQ. feopt%spacetime, "h: space-time")
  CALL OK(ALL(h%s(1:2) .EQ. [4, 2]), "h: shape (nips, nipt)")
  CALL OK(ALL(ABS(h%val(1:8) - 1.0_DFP) .LT. tol), "h = 1")
END BLOCK

! Test 2: h, hmax, hmin and r together
BLOCK
  TYPE(FEVariable_) :: h, hmax, hmin, r

  CALL GetHRQIParam(obj=st, h=h, val=NodalVariable(p, Scalar, Space), &
                    hmax=hmax, hmin=hmin, r=r)
  CALL OK(ALL(ABS(h%val(1:8) - 1.0_DFP) .LT. tol), "all: h = 1")
  CALL OK(ALL(ABS(hmax%val(1:8) - 2.0_DFP) .LT. tol), "all: hmax = 2")
  CALL OK(ALL(ABS(hmin%val(1:8) - 1.0_DFP) .LT. tol), "all: hmin = 1")
  CALL OK(r%rank .EQ. feopt%vector, "all: r is a vector")
  CALL OK(r%varType .EQ. feopt%spacetime, "all: r is space-time")
  CALL OK(ALL(r%s(1:3) .EQ. [2, 4, 2]), "all: r(nsd, nips, nipt)")
END BLOCK

CONTAINS

!> Space-time shape data: the rectangle in space, fixed in time, and a
! linear line in time
SUBROUTINE MakeST(selemsd, telemsd, stelemsd)
  TYPE(ElemShapeData_), INTENT(IN) :: selemsd
  !! shape data of the space element
  TYPE(ElemShapeData_), INTENT(IN) :: telemsd
  !! shape data of the time element
  TYPE(STElemShapeData_), ALLOCATABLE, INTENT(INOUT) :: stelemsd(:)
  !! space-time shape data, one per time point
  REAL(DFP) :: xst(2, 4, 2)
  INTEGER(I4B) :: ipt

  xst(:, :, 1) = xcell
  xst(:, :, 2) = xcell
  CALL Initiate(stelemsd, telemsd)
  DO ipt = 1, SIZE(stelemsd)
    CALL ALLOCATE (stelemsd(ipt), nsd=2, xidim=2, nns=4, nips=4, nnt=2)
    stelemsd(ipt)%T = telemsd%N(:, ipt)
    stelemsd(ipt)%dTdTheta = telemsd%dNdXi(:, 1, ipt)
    stelemsd(ipt)%jt = 0.5_DFP
    stelemsd(ipt)%wt = telemsd%ws(ipt)
    stelemsd(ipt)%N = selemsd%N
    stelemsd(ipt)%dNdXi = selemsd%dNdXi
    stelemsd(ipt)%ws = selemsd%ws
    CALL Set(obj=stelemsd(ipt), val=xst, N=selemsd%N, T=stelemsd(ipt)%T, &
             dNdXi=selemsd%dNdXi)
  END DO
END SUBROUTINE MakeST

END PROGRAM main
