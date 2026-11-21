!> author: OpenAI
! date: 2026-11-21
! summary: Scalar and vector unit normals agree across input overloads

PROGRAM main
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: qpopt => TypeQuadratureOpt
USE BaseType, ONLY: QuadraturePoint_
USE BaseType, ONLY: Scalar => TypeFEVariableScalar
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
REAL(DFP), PARAMETER :: coords(2, 4) = RESHAPE( &
                        [-1.0_DFP, -1.0_DFP, 1.0_DFP, -1.0_DFP, &
                         1.0_DFP, 1.0_DFP, -1.0_DFP, 1.0_DFP], [2, 4])
TYPE(ElemShapeData_) :: cell
TYPE(QuadraturePoint_) :: quad

CALL QP_Initiate(obj=quad, elemType=Quadrangle4, domainName="BIUNIT", &
                 nips=[2], quadratureType=qpopt%GaussLegendre)
CALL LagrangeElemShapeData(obj=cell, quad=quad, nsd=2, xidim=2, &
                           elemType=Quadrangle4, refelemCoord=coords, &
                           domainName="BIUNIT", order=1)
CALL Set(obj=cell, val=coords, N=cell%N, dNdXi=cell%dNdXi)

! Test 1: scalar sign changes across quadrature points
BLOCK
  REAL(DFP), ALLOCATABLE :: raw(:, :), field(:, :)
  REAL(DFP) :: val(4), expected(2, 4)

  val = coords(1, :)
  expected(1, :) = SIGN(1.0_DFP, quad%points(1, :))
  expected(2, :) = 0.0_DFP
  CALL GetUnitNormal(obj=cell, R=raw, val=val)
  CALL GetUnitNormal(obj=cell, R=field, &
                     val=NodalVariable(val, Scalar, Space))
  CALL OK(ALL(SHAPE(raw) .EQ. [2, 4]), "scalar raw shape")
  CALL OK(ALL(SHAPE(field) .EQ. [2, 4]), "scalar FEVariable shape")
  CALL OK(ALL(ABS(raw - expected) .LT. tol), "scalar known normal")
  CALL OK(ALL(ABS(field - expected) .LT. tol), &
          "scalar FEVariable known normal")
  CALL OK(ALL(ABS(raw - field) .LT. tol), "scalar overload equivalence")
END BLOCK

! Test 2: three vector components on a two-dimensional cell
BLOCK
  REAL(DFP), ALLOCATABLE :: raw(:, :), field(:, :)
  REAL(DFP) :: val(3, 4), expected(2, 4), point(2)
  INTEGER(I4B) :: ips

  val(1:2, :) = coords
  val(3, :) = 0.0_DFP
  DO ips = 1, 4
    point = quad%points(1:2, ips)
    expected(:, ips) = point / NORM2(point)
  END DO
  CALL GetUnitNormal(obj=cell, R=raw, val=val)
  CALL GetUnitNormal(obj=cell, R=field, &
                     val=NodalVariable(val, Vector, Space))
  CALL OK(ALL(SHAPE(raw) .EQ. [2, 4]), "vector raw shape")
  CALL OK(ALL(SHAPE(field) .EQ. [2, 4]), "vector FEVariable shape")
  CALL OK(ALL(ABS(raw - expected) .LT. tol), "vector known normal")
  CALL OK(ALL(ABS(field - expected) .LT. tol), &
          "vector FEVariable known normal")
  CALL OK(ALL(ABS(raw - field) .LT. tol), "vector overload equivalence")
END BLOCK
END PROGRAM main
