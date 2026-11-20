!> author: Vikas Sharma, Ph. D.
! date: 2026-11-21
! summary: GetUnitNormal edge cases: zero gradient, radial and 3-component
! vector fields, matrix FEVariable

PROGRAM main
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: Matrix => TypeFEVariableMatrix
USE BaseType, ONLY: qpopt => TypeQuadratureOpt
USE BaseType, ONLY: QuadraturePoint_
USE BaseType, ONLY: Space => TypeFEVariableSpace
USE BaseType, ONLY: Vector => TypeFEVariableVector
USE ElemShapeData_Method, ONLY: GetUnitNormal
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
! physical cell: rectangle [0, 2] x [0, 1], so x = 1 + xi, y = (1 + eta) / 2
REAL(DFP), PARAMETER :: xcell(2, 4) = RESHAPE([0.0_DFP, 0.0_DFP, 2.0_DFP, &
                                               0.0_DFP, 2.0_DFP, 1.0_DFP, &
                                               0.0_DFP, 1.0_DFP], [2, 4])
TYPE(ElemShapeData_) :: cell
TYPE(QuadraturePoint_) :: quad
REAL(DFP) :: xq(2, 4), rq(4)
INTEGER(I4B) :: ips

CALL QP_Initiate(obj=quad, elemType=Quadrangle4, domainName="BIUNIT", &
                 nips=[2], quadratureType=qpopt%GaussLegendre)
CALL LagrangeElemShapeData(obj=cell, quad=quad, nsd=2, xidim=2, &
                           elemType=Quadrangle4, refelemCoord=quadCoord, &
                           domainName="BIUNIT", order=1)
CALL Set(obj=cell, val=xcell, N=cell%N, dNdXi=cell%dNdXi)

! physical coordinates of the quadrature points and their distance to 0
DO ips = 1, 4
  xq(1, ips) = 1.0_DFP + quad%points(1, ips)
  xq(2, ips) = 0.5_DFP * (1.0_DFP + quad%points(2, ips))
  rq(ips) = NORM2(xq(:, ips))
END DO

! Test 1: constant scalar, grad(p) = 0, so R = 0
BLOCK
  REAL(DFP), ALLOCATABLE :: R(:, :)
  REAL(DFP) :: p(4)

  p = 3.0_DFP
  CALL GetUnitNormal(obj=cell, R=R, val=p)
  CALL OK(ALL(SHAPE(R) .EQ. [2, 4]), "constant scalar: R(nsd, nips)")
  CALL OK(ALL(ABS(R) .LT. tol), "constant scalar: R = 0")
END BLOCK

! Test 2: R is reset when it holds old values
BLOCK
  REAL(DFP), ALLOCATABLE :: R(:, :)
  REAL(DFP) :: p(4)

  ALLOCATE (R(2, 4))
  R = 7.0_DFP
  p = 3.0_DFP
  CALL GetUnitNormal(obj=cell, R=R, val=p)
  CALL OK(ALL(ABS(R) .LT. tol), "old R: zero gradient gives R = 0")
END BLOCK

! Test 3: vector field (x, y), grad|p| = (x, y) / r, so R = (x, y) / r
BLOCK
  REAL(DFP), ALLOCATABLE :: R(:, :)
  REAL(DFP) :: v(2, 4)

  v = xcell
  CALL GetUnitNormal(obj=cell, R=R, val=v)
  CALL OK(ALL(SHAPE(R) .EQ. [2, 4]), "radial vector: R(nsd, nips)")
  CALL OK(ALL(ABS(R(1, :) - xq(1, :) / rq) .LT. tol) .AND. &
          ALL(ABS(R(2, :) - xq(2, :) / rq) .LT. tol), &
          "radial vector: R = (x, y) / r")

  CALL GetUnitNormal(obj=cell, R=R, val=NodalVariable(v, Vector, Space))
  CALL OK(ALL(ABS(R(1, :) - xq(1, :) / rq) .LT. tol) .AND. &
          ALL(ABS(R(2, :) - xq(2, :) / rq) .LT. tol), &
          "radial vector FEVariable: R = (x, y) / r")
END BLOCK

! Test 4: 3-component field (x, y, 0) on a 2D cell, R = (x, y) / r
BLOCK
  REAL(DFP), ALLOCATABLE :: R(:, :)
  REAL(DFP) :: v(3, 4)

  v(1:2, :) = xcell
  v(3, :) = 0.0_DFP
  CALL GetUnitNormal(obj=cell, R=R, val=v)
  CALL OK(ALL(SHAPE(R) .EQ. [2, 4]), "3-component vector: R(nsd, nips)")
  CALL OK(ALL(ABS(R(1, :) - xq(1, :) / rq) .LT. tol) .AND. &
          ALL(ABS(R(2, :) - xq(2, :) / rq) .LT. tol), &
          "3-component vector: R = (x, y) / r")
END BLOCK

! Test 5: zero vector field, |p| = 0 and grad(p) = 0, so R = 0
BLOCK
  REAL(DFP), ALLOCATABLE :: R(:, :)
  REAL(DFP) :: v(2, 4)

  v = 0.0_DFP
  CALL GetUnitNormal(obj=cell, R=R, val=v)
  CALL OK(ALL(ABS(R) .LT. tol), "zero vector: R = 0")
END BLOCK

! Test 6: a matrix FEVariable is not supported, R does not change
BLOCK
  REAL(DFP), ALLOCATABLE :: R(:, :)
  REAL(DFP) :: m(2, 2, 4)

  ALLOCATE (R(2, 4))
  R = 7.0_DFP
  m = 1.0_DFP
  CALL GetUnitNormal(obj=cell, R=R, val=NodalVariable(m, Matrix, Space))
  CALL OK(ALL(SHAPE(R) .EQ. [2, 4]) .AND. ALL(R .EQ. 7.0_DFP), &
          "matrix FEVariable: R does not change")
END BLOCK

END PROGRAM main
