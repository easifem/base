!> author: Vikas Sharma, Ph. D.
! date: 2026-11-21
! summary: HRGN element length of a space-time line element

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
USE ElemShapeData_Method, ONLY: GetHRGNParam
USE ElemShapeData_Method, ONLY: Initiate
USE ElemShapeData_Method, ONLY: LagrangeElemShapeData
USE ElemShapeData_Method, ONLY: Set
USE FEVariable_Method, ONLY: NodalVariable
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Line2
USE QuadraturePoint_Method, ONLY: QP_Initiate => Initiate
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP), PARAMETER :: lineCoord(1, 2) = RESHAPE([-1.0_DFP, 1.0_DFP], &
                                                  [1, 2])
! space element [0, 2], time element [0, 1]: x = 1 + xi, jt = 1/2
REAL(DFP), PARAMETER :: xst(1, 2, 2) = RESHAPE([0.0_DFP, 2.0_DFP, 0.0_DFP, &
                                                2.0_DFP], [1, 2, 2])
! nodal values of p = x + 1, so the unit normal is r = 1
REAL(DFP), PARAMETER :: p(2) = [1.0_DFP, 3.0_DFP]
TYPE(ElemShapeData_) :: line
TYPE(STElemShapeData_), ALLOCATABLE :: st(:)
TYPE(QuadraturePoint_) :: quad
INTEGER(I4B) :: ipt

CALL QP_Initiate(obj=quad, elemType=Line2, domainName="BIUNIT", &
                 nips=[2], quadratureType=qpopt%GaussLegendre)
CALL LagrangeElemShapeData(obj=line, quad=quad, nsd=1, xidim=1, &
                           elemType=Line2, refelemCoord=lineCoord, &
                           domainName="BIUNIT", order=1)
CALL MakeST(line, st)
DO ipt = 1, SIZE(st)
  CALL Set(obj=st(ipt), val=xst, N=st(ipt)%N, T=st(ipt)%T, &
           dNdXi=st(ipt)%dNdXi)
END DO

! Test 1: one time point; dN_I/dx = -1/2, 1/2 and SUM_a T_a = 1, so
! SUM_I SUM_a |dN_I/dx T_a| = 1 and h = 2, the length of the space element
BLOCK
  REAL(DFP), ALLOCATABLE :: h(:)

  CALL GetHRGNParam(obj=st(1), h=h, val=NodalVariable(p, Scalar, Space))
  CALL OK(SIZE(h) .EQ. 2, "one time point: h(nips)")
  CALL OK(ALL(ABS(h - 2.0_DFP) .LT. tol), "one time point: h = 2")
END BLOCK

! Test 2: all time points, h(nips, nipt)
BLOCK
  REAL(DFP), ALLOCATABLE :: h(:, :)

  CALL GetHRGNParam(obj=st, h=h, val=NodalVariable(p, Scalar, Space))
  CALL OK(ALL(SHAPE(h) .EQ. [2, 2]), "all time points: h(nips, nipt)")
  CALL OK(ALL(ABS(h - 2.0_DFP) .LT. tol), "all time points: h = 2")
END BLOCK

! Test 3: all time points, h as a space-time FEVariable
BLOCK
  TYPE(FEVariable_) :: h

  CALL GetHRGNParam(obj=st, h=h, val=NodalVariable(p, Scalar, Space))
  CALL OK(h%rank .EQ. feopt%scalar, "FEVariable: scalar")
  CALL OK(h%varType .EQ. feopt%spacetime, "FEVariable: space-time")
  CALL OK(h%defineOn .EQ. feopt%quadrature, "FEVariable: quadrature")
  CALL OK(ALL(h%s(1:2) .EQ. [2, 2]), "FEVariable: shape (nips, nipt)")
  CALL OK(ALL(ABS(h%val(1:4) - 2.0_DFP) .LT. tol), "FEVariable: h = 2")
END BLOCK

CONTAINS

!> Space-time shape data of a linear line in space and in time; the
! space part is the same at every time point and jt = 1/2
SUBROUTINE MakeST(elemsd, stelemsd)
  TYPE(ElemShapeData_), INTENT(IN) :: elemsd
  !! shape data of a linear line, used in space and in time
  TYPE(STElemShapeData_), ALLOCATABLE, INTENT(INOUT) :: stelemsd(:)
  !! space-time shape data, one per time point
  INTEGER(I4B) :: ii

  CALL Initiate(stelemsd, elemsd)
  DO ii = 1, SIZE(stelemsd)
    CALL ALLOCATE (stelemsd(ii), nsd=1, xidim=1, nns=2, nips=2, nnt=2)
    stelemsd(ii)%T = elemsd%N(:, ii)
    stelemsd(ii)%dTdTheta = elemsd%dNdXi(:, 1, ii)
    stelemsd(ii)%jt = 0.5_DFP
    stelemsd(ii)%wt = elemsd%ws(ii)
    stelemsd(ii)%N = elemsd%N
    stelemsd(ii)%dNdXi = elemsd%dNdXi
    stelemsd(ii)%ws = elemsd%ws
  END DO
END SUBROUTINE MakeST

END PROGRAM main
