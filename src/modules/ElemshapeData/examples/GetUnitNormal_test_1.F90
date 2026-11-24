!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: Unit normal from the gradient of a scalar or vector field

PROGRAM main
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: FEVariable_
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
REAL(DFP), PARAMETER :: quadCoord(2, 4) = RESHAPE([-1.0_DFP, -1.0_DFP, &
                        1.0_DFP, -1.0_DFP, 1.0_DFP, 1.0_DFP, -1.0_DFP, &
                                                   1.0_DFP], [2, 4])
! physical cell: rectangle [0, 2] x [0, 1], so x = 1 + xi, y = (1 + eta) / 2
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
! Test 1: level set p = y - 1/2, R = sign(p) (0, 1)
BLOCK
  REAL(DFP), ALLOCATABLE :: R(:, :)
  REAL(DFP) :: p(4), py
  INTEGER(I4B) :: ips

  p = xcell(2, :) - 0.5_DFP
  CALL GetUnitNormal(obj=cell, R=R, val=p)
  CALL OK(ALL(SHAPE(R) .EQ. [2, 4]), "scalar: R(nsd, nips)")
  DO ips = 1, 4
    py = 0.5_DFP * quad%points(2, ips)
    CALL OK(ABS(R(1, ips)) .LT. tol .AND. &
            ABS(R(2, ips) - SIGN(1.0_DFP, py)) .LT. tol, &
            "scalar: R = sign(p) (0, 1)")
  END DO

  CALL GetUnitNormal(obj=cell, R=R, val=NodalVariable(p, Scalar, Space))
  CALL OK(ALL(ABS(ABS(R(2, :)) - 1.0_DFP) .LT. tol), "scalar FEVariable")
END BLOCK

! Test 2: vector field (x, 0), R = (1, 0) where x > 0
BLOCK
  REAL(DFP), ALLOCATABLE :: R(:, :)
  REAL(DFP) :: v(2, 4)

  v(1, :) = xcell(1, :)
  v(2, :) = 0.0_DFP
  CALL GetUnitNormal(obj=cell, R=R, val=v)
  CALL OK(ALL(ABS(R(1, :) - 1.0_DFP) .LT. tol), "vector: R_x = 1")
  CALL OK(ALL(ABS(R(2, :)) .LT. tol), "vector: R_y = 0")

  CALL GetUnitNormal(obj=cell, R=R, val=NodalVariable(v, Vector, Space))
  CALL OK(ALL(ABS(R(1, :) - 1.0_DFP) .LT. tol), "vector FEVariable")
END BLOCK

END PROGRAM main
