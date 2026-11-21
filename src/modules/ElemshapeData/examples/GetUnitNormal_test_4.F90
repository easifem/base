!> author: OpenAI
! date: 2026-11-21
! summary: Zero vector values with nonzero gradients give zero normals

PROGRAM main
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: qpopt => TypeQuadratureOpt
USE BaseType, ONLY: QuadraturePoint_
USE BaseType, ONLY: Space => TypeFEVariableSpace
USE BaseType, ONLY: Vector => TypeFEVariableVector
USE ElemShapeData_Method, ONLY: GetInterpolation
USE ElemShapeData_Method, ONLY: GetUnitNormal
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
REAL(DFP), PARAMETER :: coords(1, 2) = &
                        RESHAPE([-1.0_DFP, 1.0_DFP], [1, 2])
TYPE(ElemShapeData_) :: cell
TYPE(QuadraturePoint_) :: quad
INTEGER(I4B) :: component, center

CALL QP_Initiate(obj=quad, elemType=Line2, domainName="BIUNIT", &
                 nips=[3], quadratureType=qpopt%GaussLegendre)
! The middle Gauss point is mathematically zero; remove roundoff here.
center = MINLOC(ABS(quad%points(1, :)), DIM=1)
quad%points(1, center) = 0.0_DFP
CALL LagrangeElemShapeData(obj=cell, quad=quad, nsd=1, xidim=1, &
                           elemType=Line2, refelemCoord=coords, &
                           domainName="BIUNIT", order=1)
CALL Set(obj=cell, val=coords, N=cell%N, dNdXi=cell%dNdXi)

! Test 1: either component can cross zero at the center point
DO component = 1, 2
  BLOCK
    REAL(DFP) :: val(2, 2)
    REAL(DFP), ALLOCATABLE :: raw(:, :), field(:, :), p(:, :)
    INTEGER(I4B) :: ips

    val = 0.0_DFP
    val(component, :) = coords(1, :)
    CALL GetInterpolation(obj=cell, ans=p, val=val)
    CALL OK(ALL(p(:, center) .EQ. 0.0_DFP), &
            "regression setup: exact zero vector at center")
    ALLOCATE (raw(1, 3), field(1, 3))
    raw = 7.0_DFP
    field = 7.0_DFP
    CALL GetUnitNormal(obj=cell, R=raw, val=val)
    CALL GetUnitNormal(obj=cell, R=field, &
                       val=NodalVariable(val, Vector, Space))
    CALL OK(ALL(SHAPE(raw) .EQ. [1, 3]), "raw normal shape")
    CALL OK(ALL(SHAPE(field) .EQ. [1, 3]), "FEVariable normal shape")
    CALL OK(ALL(raw(:, center) .EQ. 0.0_DFP), &
            "raw: zero vector gives zero normal, replacing old output")
    CALL OK(ALL(field(:, center) .EQ. 0.0_DFP), &
            "FEVariable: zero vector gives zero normal")
    nonzeroPoints: DO ips = 1, 3
      IF (ips .EQ. center) CYCLE nonzeroPoints
      CALL OK(ABS(raw(1, ips) - &
                  SIGN(1.0_DFP, quad%points(1, ips))) .LT. tol, &
              "raw: nonzero point retains signed unit normal")
      CALL OK(ABS(field(1, ips) - raw(1, ips)) .LT. tol, &
              "FEVariable: nonzero point agrees with raw input")
    END DO nonzeroPoints
  END BLOCK
END DO
END PROGRAM main
