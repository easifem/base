!> author: Vikas Sharma, Ph. D.
! date: 2026-11-21
! summary: HRQI length scale with real arrays on a rectangle

PROGRAM main
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: qpopt => TypeQuadratureOpt
USE BaseType, ONLY: QuadraturePoint_
USE BaseType, ONLY: Scalar => TypeFEVariableScalar
USE BaseType, ONLY: Space => TypeFEVariableSpace
USE BaseType, ONLY: Vector => TypeFEVariableVector
USE ElemShapeData_Method, ONLY: GetHRQIParam
USE ElemShapeData_Method, ONLY: LagrangeElemShapeData
USE ElemShapeData_Method, ONLY: Set
USE FEVariable_Method, ONLY: NodalVariable
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: Quadrangle4
USE QuadraturePoint_Method, ONLY: QP_Initiate => Initiate
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP), PARAMETER :: quadCoord(2, 4) = RESHAPE([-1.0_DFP, -1.0_DFP, &
                        1.0_DFP, -1.0_DFP, 1.0_DFP, 1.0_DFP, -1.0_DFP, &
                                                   1.0_DFP], [2, 4])
! physical cell: rectangle [0, 2] x [0, 1], so x = 1 + xi, y = (1 + eta) / 2
! jacobian = diag(1, 1/2), G = inv(J J^T) = diag(1, 4)
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

! Test 1: level set p = y - 1/2, r = (0, +-1), h = 2 / sqrt(4) = 1
BLOCK
  REAL(DFP), ALLOCATABLE :: h(:), hmax(:), hmin(:), r(:, :)
  REAL(DFP) :: p(4)

  p = xcell(2, :) - 0.5_DFP
  CALL GetHRQIParam(obj=cell, h=h, val=NodalVariable(p, Scalar, Space), &
                    hmax=hmax, hmin=hmin, r=r)
  CALL OK(SIZE(h) .EQ. 4, "scalar: h(nips)")
  CALL OK(ALL(ABS(h - 1.0_DFP) .LT. tol), "scalar: h = 1 along y")
  CALL OK(ALL(ABS(hmax - 2.0_DFP) .LT. tol), "scalar: hmax = 2")
  CALL OK(ALL(ABS(hmin - 1.0_DFP) .LT. tol), "scalar: hmin = 1")
  CALL OK(ALL(SHAPE(r) .EQ. [2, 4]), "scalar: r(nsd, nips)")
  CALL OK(ALL(ABS(r(1, :)) .LT. tol), "scalar: r_x = 0")
  CALL OK(ALL(ABS(ABS(r(2, :)) - 1.0_DFP) .LT. tol), "scalar: |r_y| = 1")
END BLOCK

! Test 2: vector field (x, 0), r = (1, 0), h = 2 / sqrt(1) = 2
BLOCK
  REAL(DFP), ALLOCATABLE :: h(:)
  REAL(DFP) :: v(2, 4)

  v(1, :) = xcell(1, :)
  v(2, :) = 0.0_DFP
  CALL GetHRQIParam(obj=cell, h=h, val=NodalVariable(v, Vector, Space))
  CALL OK(ALL(ABS(h - 2.0_DFP) .LT. tol), "vector: h = 2 along x")
END BLOCK

! Test 3: constant field, grad(p) = 0, so r = 0 and h = 0
BLOCK
  REAL(DFP), ALLOCATABLE :: h(:)
  REAL(DFP) :: p(4)

  p = 1.0_DFP
  CALL GetHRQIParam(obj=cell, h=h, val=NodalVariable(p, Scalar, Space))
  CALL OK(ALL(ABS(h) .LT. tol), "constant: h = 0")
END BLOCK

END PROGRAM main
