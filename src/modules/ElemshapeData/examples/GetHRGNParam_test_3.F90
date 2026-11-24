!> author: Vikas Sharma, Ph. D.
! date: 2026-11-21
! summary: HRGN element length when val is a matrix

PROGRAM main
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: Matrix => TypeFEVariableMatrix
USE BaseType, ONLY: qpopt => TypeQuadratureOpt
USE BaseType, ONLY: QuadraturePoint_
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
REAL(DFP), PARAMETER :: quadCoord(2, 4) = RESHAPE([-1.0_DFP, -1.0_DFP, &
                        1.0_DFP, -1.0_DFP, 1.0_DFP, 1.0_DFP, -1.0_DFP, &
                                                   1.0_DFP], [2, 4])
! physical cell: unit square [0, 1] x [0, 1], so hmin = 1
REAL(DFP), PARAMETER :: xcell(2, 4) = RESHAPE([0.0_DFP, 0.0_DFP, 1.0_DFP, &
                                               0.0_DFP, 1.0_DFP, 1.0_DFP, &
                                               0.0_DFP, 1.0_DFP], [2, 4])
TYPE(ElemShapeData_) :: line, cell
TYPE(STElemShapeData_), ALLOCATABLE :: st(:)
TYPE(QuadraturePoint_) :: quad, cellQuad
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

CALL QP_Initiate(obj=cellQuad, elemType=Quadrangle4, domainName="BIUNIT", &
                 nips=[2], quadratureType=qpopt%GaussLegendre)
CALL LagrangeElemShapeData(obj=cell, quad=cellQuad, nsd=2, xidim=2, &
                           elemType=Quadrangle4, refelemCoord=quadCoord, &
                           domainName="BIUNIT", order=1)
CALL Set(obj=cell, val=xcell, N=cell%N, dNdXi=cell%dNdXi)

! Test 1: space cell, a matrix val has no direction, so h = hmin = 1
BLOCK
  REAL(DFP), ALLOCATABLE :: h(:)
  REAL(DFP) :: m(2, 2, 4)

  m = 1.0_DFP
  CALL GetHRGNParam(obj=cell, h=h, val=NodalVariable(m, Matrix, Space))
  CALL OK(SIZE(h) .EQ. cell%nips, "space, matrix: h(nips)")
  CALL OK(ALL(ABS(h - 1.0_DFP) .LT. tol), "space, matrix: h = hmin = 1")
END BLOCK

! Test 2: space-time line, a matrix val has no direction, so h = hmin = 2,
! the length of the space element
BLOCK
  REAL(DFP), ALLOCATABLE :: h(:)
  REAL(DFP) :: m(1, 1, 2)

  m = 1.0_DFP
  CALL GetHRGNParam(obj=st(1), h=h, val=NodalVariable(m, Matrix, Space))
  CALL OK(SIZE(h) .EQ. 2, "space-time, matrix: h(nips)")
  CALL OK(ALL(ABS(h - 2.0_DFP) .LT. tol), &
          "space-time, matrix: h = hmin = 2")
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
