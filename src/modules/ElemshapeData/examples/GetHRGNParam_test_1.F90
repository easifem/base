!> author: Vikas Sharma, Ph. D.
! date: 2026-11-21
! summary: HRGN element length of a unit square along a level-set normal

PROGRAM main
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: feopt => TypeFEVariableOpt
USE BaseType, ONLY: qpopt => TypeQuadratureOpt
USE BaseType, ONLY: QuadraturePoint_
USE BaseType, ONLY: Scalar => TypeFEVariableScalar
USE BaseType, ONLY: Space => TypeFEVariableSpace
USE ElemShapeData_Method, ONLY: GetHRGNParam
USE ElemShapeData_Method, ONLY: LagrangeElemShapeData
USE ElemShapeData_Method, ONLY: Set
USE FEVariable_Method, ONLY: NodalVariable
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Quadrangle4
USE QuadraturePoint_Method, ONLY: QP_Initiate => Initiate
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP), PARAMETER :: quadCoord(2, 4) = RESHAPE([-1.0_DFP, -1.0_DFP, &
                        1.0_DFP, -1.0_DFP, 1.0_DFP, 1.0_DFP, -1.0_DFP, &
                                                   1.0_DFP], [2, 4])
! physical cell: unit square [0, 1] x [0, 1], x = (1 + xi) / 2
REAL(DFP), PARAMETER :: xcell(2, 4) = RESHAPE([0.0_DFP, 0.0_DFP, 1.0_DFP, &
                                               0.0_DFP, 1.0_DFP, 1.0_DFP, &
                                               0.0_DFP, 1.0_DFP], [2, 4])
TYPE(ElemShapeData_) :: cell
TYPE(QuadraturePoint_) :: quad

CALL QP_Initiate(obj=quad, elemType=Quadrangle4, domainName="BIUNIT", &
                 nips=[2], quadratureType=qpopt%GaussLegendre)
CALL LagrangeElemShapeData(obj=cell, quad=quad, nsd=2, xidim=2, &
                           elemType=Quadrangle4, refelemCoord=quadCoord, &
                           domainName="BIUNIT", order=1)
CALL Set(obj=cell, val=xcell, N=cell%N, dNdXi=cell%dNdXi)

! Test 1: p = y + 1, normal r = (0, 1), so h = 1 (the cell height)
BLOCK
  REAL(DFP), ALLOCATABLE :: h(:)
  REAL(DFP) :: p(4)

  p = xcell(2, :) + 1.0_DFP
  CALL GetHRGNParam(obj=cell, h=h, val=NodalVariable(p, Scalar, Space))
  CALL OK(SIZE(h) .EQ. 4, "h(nips)")
  CALL OK(ALL(ABS(h - 1.0_DFP) .LT. tol), "r = (0, 1): h = 1")
END BLOCK

! Test 2: p = x + y + 1, normal r = (1, 1) / sqrt(2)
! h = sqrt(2) / (1 + |1 - x - y|)
BLOCK
  REAL(DFP), ALLOCATABLE :: h(:)
  REAL(DFP) :: p(4), x, y, ref
  INTEGER(I4B) :: ips

  p = xcell(1, :) + xcell(2, :) + 1.0_DFP
  CALL GetHRGNParam(obj=cell, h=h, val=NodalVariable(p, Scalar, Space))
  DO ips = 1, 4
    x = 0.5_DFP * (1.0_DFP + quad%points(1, ips))
    y = 0.5_DFP * (1.0_DFP + quad%points(2, ips))
    ref = SQRT(2.0_DFP) / (1.0_DFP + ABS(1.0_DFP - x - y))
    CALL OK(ABS(h(ips) - ref) .LT. tol, "r = (1, 1) / sqrt(2)")
  END DO
END BLOCK

! Test 3: constant p, so grad(p) = 0 and h = hmin = 1
BLOCK
  REAL(DFP), ALLOCATABLE :: h(:)
  REAL(DFP) :: p(4)

  p = 1.0_DFP
  CALL GetHRGNParam(obj=cell, h=h, val=NodalVariable(p, Scalar, Space))
  CALL OK(ALL(ABS(h - 1.0_DFP) .LT. tol), "grad(p) = 0: h = hmin")
END BLOCK

! Test 4: h as a scalar space FEVariable at quadrature points
BLOCK
  TYPE(FEVariable_) :: h
  REAL(DFP) :: p(4)

  p = xcell(2, :) + 1.0_DFP
  CALL GetHRGNParam(obj=cell, h=h, val=NodalVariable(p, Scalar, Space))
  CALL OK(h%rank .EQ. feopt%scalar, "FEVariable: scalar")
  CALL OK(h%varType .EQ. feopt%space, "FEVariable: space")
  CALL OK(h%defineOn .EQ. feopt%quadrature, "FEVariable: quadrature")
  CALL OK(h%len .EQ. 4, "FEVariable: len = nips")
  CALL OK(ALL(ABS(h%val(1:4) - 1.0_DFP) .LT. tol), "FEVariable: h = 1")
END BLOCK

END PROGRAM main
